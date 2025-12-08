package com.bryzek.ai.claude

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
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

case class ClaudeConfig(key: String, anthropicVersion: String, betaHeaders: Seq[String] = Seq.empty)
object ClaudeConfig {
  private val Version = "2023-06-01"
  private val StructuredOutputsBeta = "structured-outputs-2025-11-13"

  def apply(key: String): ClaudeConfig = ClaudeConfig(
    key = key,
    anthropicVersion = Version,
    betaHeaders = Seq(StructuredOutputsBeta)
  )
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

final case class ClaudeOutputFormat(
  name: String,
  schema: _root_.play.api.libs.json.JsObject
) {
  def toApi: ClaudeApiOutputFormat = ClaudeApiOutputFormat(
    `type` = com.bryzek.claude.v0.models.ClaudeOutputFormatType.JsonSchema,
    schema = schema
  )
}

case class ClaudeClient(
  client: Client,
  config: ClaudeConfig,
  store: ClaudeStore
) {
  private val defaultHeaders: Seq[(String, String)] = {
    Seq(
      "x-api-key" -> config.key,
      "Content-Type" -> "application/json",
      "anthropic-version" -> config.anthropicVersion
    ) ++ (config.betaHeaders.toList match {
      case Nil => Nil
      case all => Seq("anthropic-beta" -> all.mkString(","))
    })
  }

  private def randomId(prefix: String): String = {
    prefix + "-" + UUID.randomUUID().toString.replaceAll("-", "")
  }

  def makeClaudeMessage(role: ClaudeRole, msg: String*): ClaudeMessage = ClaudeClient.makeClaudeMessage(role, msg*)

  def chatComments(request: ClaudeRequest)(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[String]]] = {
    chatCompletion[CommentsResponse](request, ClaudeOutputFormats.CommentsResponse)(using ec)
      .map(_.map(_.content.comments))
  }

  def chatRecommendations(request: ClaudeRequest)(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[Recommendation]]] = {
    chatCompletion[RecommendationResponse](request, ClaudeOutputFormats.RecommendationsResponse)(using ec)
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
    chatCompletion[SingleInsightResponse](request, ClaudeOutputFormats.SingleInsight)(using ec)
      .map(_.map(_.content.insight))
  }

  def chatCompletion[T](originalRequest: ClaudeRequest, outputFormat: ClaudeOutputFormat)(implicit
    ec: ExecutionContext,
    reads: Reads[T]
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]] = {
    val request = originalRequest.copy(
      outputFormat = Some(outputFormat.toApi)
    )
    val rm = ClaudeRequestMetadata(client, randomId("req"), request)
    store.storeRequest(rm)
    client.messages
      .post(
        request,
        requestHeaders = defaultHeaders ++ Seq((TestClaudeClient.OutputFormatNameHeader, outputFormat.name))
      )
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

    // With structured outputs, Claude returns clean JSON without markdown delimiters
    Try(Json.parse(content.trim)) match {
      case Failure(ex) => parseError(s"Content is not valid JSON: ${ex.getMessage}")
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

object ClaudeOutputFormats {
  def create(name: String, properties: JsObject, required: Seq[String]): ClaudeOutputFormat = {
    ClaudeOutputFormat(
      name = name,
      schema = Json.obj(
        "type" -> "object",
        "properties" -> properties,
        "required" -> required,
        "additionalProperties" -> false
      )
    )
  }

  private val stepsProperty = Json.obj(
    "type" -> "array",
    "items" -> Json.obj(
      "type" -> "object",
      "properties" -> Json.obj(
        "explanation" -> Json.obj("type" -> "string"),
        "output" -> Json.obj("type" -> "string")
      ),
      "required" -> Json.arr("explanation", "output"),
      "additionalProperties" -> false
    )
  )

  val CommentsResponse: ClaudeOutputFormat = ClaudeOutputFormats.create(
    "comments_response",
    Json.obj(
      "steps" -> stepsProperty,
      "comments" -> Json.obj(
        "type" -> "array",
        "items" -> Json.obj("type" -> "string")
      )
    ),
    Seq("steps", "comments")
  )

  val RecommendationsResponse: ClaudeOutputFormat = ClaudeOutputFormats.create(
    "recommendation_response",
    Json.obj(
      "steps" -> stepsProperty,
      "recommendations" -> Json.obj(
        "type" -> "array",
        "items" -> Json.obj(
          "type" -> "object",
          "properties" -> Json.obj(
            "category" -> Json.obj("type" -> "string"),
            "confidence" -> Json.obj(
              "type" -> "integer",
              "minimum" -> 0,
              "maximum" -> 100
            )
          ),
          "required" -> Json.arr("category", "confidence"),
          "additionalProperties" -> false
        )
      )
    ),
    Seq("steps", "recommendations")
  )

  val SingleInsight: ClaudeOutputFormat = ClaudeOutputFormats.create(
    "single_insight_response",
    Json.obj(
      "steps" -> stepsProperty,
      "insight" -> Json.obj("type" -> "string")
    ),
    Seq("steps", "insight")
  )

  val all: List[ClaudeOutputFormat] = List(CommentsResponse, RecommendationsResponse, SingleInsight)
}
