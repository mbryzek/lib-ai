package com.mbryzek.ai.claude

import cats.data.Validated.{Invalid, Valid}
import cats.data.{Validated, ValidatedNec}
import cats.implicits.*
import com.bryzek.claude.response.v0.models.*
import com.bryzek.claude.response.v0.models.json.*
import com.bryzek.claude.v0.errors.ClaudeErrorResponseResponse
import com.bryzek.claude.v0.interfaces.Client
import com.bryzek.claude.v0.models.*
import com.google.inject.ImplementedBy
import play.api.libs.json.*

import java.util.UUID
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

case class ClaudeConfig(key: String, anthropicVersion: String)
object ClaudeConfig {
  private val Version = "2023-06-01"
  def apply(key: String): ClaudeConfig = ClaudeConfig(key = key, anthropicVersion = Version)
}

sealed trait ClaudeEnvironment
object ClaudeEnvironment {
  case object Sandbox extends ClaudeEnvironment
  case object Production extends ClaudeEnvironment
}

case class ClaudeRequestMetadata(client: Client, id: String, request: ClaudeRequest) {
  val start: Long = System.currentTimeMillis()

  def error(msg: String, raw: Option[String] = None): ClaudeError =
    ClaudeError(message = s"$msg [Request ID: $id]", raw = raw)
}

case class ClaudeResponseMetadata[T](request: ClaudeRequestMetadata, response: ClaudeResponse, content: T) {
  val duration: Long = System.currentTimeMillis() - request.start
}

trait ClaudeStore {
  def storeRequest(request: ClaudeRequestMetadata): Unit
  def storeResponseError(request: ClaudeRequestMetadata, errors: Seq[ClaudeError]): Unit
  def storeResponseSuccess[T](response: ClaudeResponseMetadata[T]): Unit
}

case object NoopClaudeStore extends ClaudeStore {
  override def storeRequest(request: ClaudeRequestMetadata): Unit = ()
  override def storeResponseError(request: ClaudeRequestMetadata, errors: Seq[ClaudeError]): Unit = ()
  override def storeResponseSuccess[T](response: ClaudeResponseMetadata[T]): Unit = ()
}

@ImplementedBy(classOf[ClaudeClientFactoryImpl])
trait ClaudeClientFactory {
  final def instance(env: ClaudeEnvironment, apiKey: String)(store: ClaudeStore): ClaudeClient = {
    ClaudeClient(getClient(env), ClaudeConfig(apiKey), store)
  }

  def getClient(env: ClaudeEnvironment): Client
}

class ClaudeClientFactoryImpl @Inject() (
  productionClaudeClient: ProductionClaudeClient,
  testClaudeClient: TestClaudeClient
) extends ClaudeClientFactory {
  override def getClient(env: ClaudeEnvironment): Client = {
    env match {
      case ClaudeEnvironment.Production => productionClaudeClient
      case ClaudeEnvironment.Sandbox => testClaudeClient
    }
  }
}

object ClaudeClient {

  def makeClaudeMessage(role: ClaudeRole, msg: String*): ClaudeMessage = {
    ClaudeMessage(
      role = role,
      content = msg.map { m => ClaudeContent(ClaudeContentType.Text, m) }
    )
  }

}

case class ClaudeClient(
  client: Client,
  config: ClaudeConfig,
  store: ClaudeStore
) {
  private val defaultHeaders = Seq(
    "x-api-key" -> config.key,
    "Content-Type" -> "application/json",
    "anthropic-version" -> config.anthropicVersion
  )

  private def randomId(prefix: String): String = {
    prefix + "-" + UUID.randomUUID().toString.replaceAll("-", "")
  }

  def makeClaudeMessage(role: ClaudeRole, msg: String*): ClaudeMessage = ClaudeClient.makeClaudeMessage(role, msg*)

  def chatComments(request: ClaudeRequest)(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[String]]] = {
    chatCompletion[CommentsResponse](request, ResponseFormat.Comments)(using ec).map(_.map(_.content.comments))
  }

  def chatRecommendations(request: ClaudeRequest)(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[Recommendation]]] = {
    chatCompletion[RecommendationResponse](request, ResponseFormat.Recommendations)(using ec)
      .map(_.map(_.content.recommendations))
  }

  def chatInsight(request: ClaudeRequest)(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[String]]] = {
    chatComments(request)(using ec)
  }

  def chatSingleInsight(request: ClaudeRequest)(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, String]] = {
    chatCompletion[SingleInsightResponse](request, ResponseFormat.SingleInsight)(using ec)
      .map(_.map(_.content.insight))
  }

  def chatCompletion[T](originalRequest: ClaudeRequest, responseFormat: ResponseFormat)(implicit
    ec: ExecutionContext,
    reads: Reads[T]
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]] = {
    val request = originalRequest.copy(
      system = originalRequest.system match {
        case None => Some(responseFormat.structure)
        case Some(s) => Some(s + ". " + responseFormat.structure)
      }
    )
    val rm = ClaudeRequestMetadata(client, randomId("req"), request)
    store.storeRequest(rm)
    client.messages
      .post(request, requestHeaders = defaultHeaders)
      .map(parseContent[T](rm, _))
      .recover {
        case r: ClaudeErrorResponseResponse => r.claudeErrorResponse.error.invalidNec
        case NonFatal(e) => rm.error(e.getMessage).invalidNec
      }
      .map { res =>
        storeResponse(rm, res); res
      }
  }

  private def storeResponse[T](
    request: ClaudeRequestMetadata,
    response: ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]
  ): Unit = {
    response match {
      case Invalid(e) => store.storeResponseError(request, e.toList)
      case Valid(r) => store.storeResponseSuccess(r)
    }
  }

  private def parseContent[T](rm: ClaudeRequestMetadata, response: ClaudeResponse)(implicit
    reads: Reads[T]
  ): ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]] = {
    response.content.map(_.text).mkString("\n") match {
      case content if content.nonEmpty => parseContent[T](rm, response, content)
      case _ => rm.error("No content found in message").invalidNec
    }
  }

  private def parseContent[T](rm: ClaudeRequestMetadata, response: ClaudeResponse, content: String)(implicit
    reads: Reads[T]
  ): ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]] = {
    def parseError(msg: String) = {
      rm.error(msg, raw = Some(response.content.map(_.text).mkString("\n"))).invalidNec
    }

    Try(Json.parse(content)) match {
      case Failure(_) => parseError("Content is not valid JSON")
      case Success(js) =>
        js.validate[T] match {
          case JsSuccess(value, _) => ClaudeResponseMetadata(rm, response, value).validNec
          case JsError(errors) => {
            val messages = errors.flatMap(e => e._2.map(m => s"${e._1}: ${m.message}"))
            parseError(s"Content is not valid: ${messages.mkString(", ")}")
          }
        }
    }
  }
}

trait ResponseFormat {
  def structure: String
}

object ResponseFormat {
  private val Steps: String = """
    "steps": [
      { "explanation": "Brief explanation of your analysis step",
        "output": "The result or finding from this step"
      }
    ]
  """.strip

  val Comments: ResponseFormat = new ResponseFormat {
    override val structure: String = buildJsonMessage(s"""{
    $Steps,
    "comments": [
      "Your main response or advice as a string",
      "Additional comments if needed"
    ]
  }""")
  }

  val Recommendations: ResponseFormat = new ResponseFormat {
    override def structure: String = buildJsonMessage(s"""{
      $Steps,
      "recommendations": [
        [{"category":"name","confidence":75},
         {"category":"second name","confidence":50}
        ]
      ]
    }""")
  }

  val SingleInsight: ResponseFormat = new ResponseFormat {
    override def structure: String = buildJsonMessage(s"""{
    $Steps,
    "insight":"Your insightful comment"
  }""")
  }

  val all: List[ResponseFormat] = List(Comments, Recommendations, SingleInsight)

  private def buildJsonMessage(structure: String): String = {
    validateJsonObject(structure) match {
      case Invalid(e) => sys.error(s"Invalid JSON Structure: $e. Structure:\n$structure")
      case Valid(s) => {
        s"ALWAYS respond in the following JSON format. IMPORTANT: Return only the raw JSON without any markdown formatting or code blocks. Format:\n${Json
            .prettyPrint(s)}\n"
      }
    }
  }

  private def validateJsonObject(str: String): Validated[String, JsObject] = {
    try {
      val jsValue = Json.parse(str)
      jsValue.validate[JsObject] match {
        case JsSuccess(obj, _) => Valid(obj)
        case JsError(errors) => Invalid(s"Not a JSON object: ${JsError.toJson(errors)}")
      }
    } catch {
      case ex: Exception => Invalid(s"Invalid JSON: ${ex.getMessage}")
    }
  }
}
