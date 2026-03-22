package com.bryzek.ai.claude

import cats.data.{NonEmptyChain, ValidatedNec}
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import com.bryzek.claude.response.models.*
import com.bryzek.claude.response.models.json.*
import com.bryzek.claude.client.IClient
import generated.errors.ClaudeErrorResponseResponse
import com.bryzek.claude.models.*
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

case class AiRequest(
  messages: Seq[ClaudeMessage],
  maxTokens: Long = 30000L,
  temperature: Option[BigDecimal] = None,
  system: Option[String] = None
) {
  def toClaudeRequest(model: ClaudeModel): ClaudeRequest = ClaudeRequest(
    model = model,
    messages = messages,
    maxTokens = maxTokens,
    temperature = temperature,
    system = system,
    outputFormat = None
  )
}

case class ClaudeRequestMetadata(client: IClient, id: String, request: ClaudeRequest) {
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

  def getClient(env: ClaudeEnvironment): IClient
}

class ClaudeClientFactoryImpl @Inject() (
  productionClaudeClient: ProductionClaudeClient,
  testClaudeClient: TestClaudeClient
) extends ClaudeClientFactory {
  override def getClient(env: ClaudeEnvironment): IClient = {
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

  /** Checks if an error message indicates a 529 overloaded response from the Claude API */
  def isOverloadedError(errorMessage: String): Boolean =
    errorMessage.contains("response code[529]")

}

final case class ClaudeOutputFormat(
  name: String,
  schema: _root_.play.api.libs.json.JsObject
) {
  def toApi: ClaudeApiOutputFormat = ClaudeApiOutputFormat(
    `type` = com.bryzek.claude.models.ClaudeOutputFormatType.JsonSchema,
    schema = schema
  )
}

case class ClaudeClient(
  client: IClient,
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

  def chatComments(request: AiRequest, models: Seq[ClaudeModel])(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[String]]] = {
    chatCompletion[CommentsResponse](request, ClaudeOutputFormats.CommentsResponse, models)(using ec)
      .map(_.map(_.content.comments))
  }

  def chatRecommendations(request: AiRequest, models: Seq[ClaudeModel])(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[Recommendation]]] = {
    chatCompletion[RecommendationResponse](request, ClaudeOutputFormats.RecommendationsResponse, models)(using ec)
      .map(_.map(_.content.recommendations))
  }

  def chatInsight(request: AiRequest, models: Seq[ClaudeModel])(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[String]]] = {
    chatComments(request, models)(using ec)
  }

  def chatSingleInsight(request: AiRequest, models: Seq[ClaudeModel])(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, String]] = {
    chatCompletion[SingleInsightResponse](request, ClaudeOutputFormats.SingleInsight, models)(using ec)
      .map(_.map(_.content.insight))
  }

  def chatInsightSections(request: AiRequest, models: Seq[ClaudeModel])(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[InsightSection]]] = {
    chatCompletion[InsightSectionsResponse](request, ClaudeOutputFormats.InsightSectionsResponse, models)(using ec)
      .map(_.map(_.content.sections))
  }

  def chatText(request: AiRequest, models: Seq[ClaudeModel])(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, String]] = {
    tryModels(models) { model =>
      chatTextSingle(request.toClaudeRequest(model))
    }
  }

  def chatCompletion[T](request: AiRequest, outputFormat: ClaudeOutputFormat, models: Seq[ClaudeModel])(implicit
    ec: ExecutionContext,
    reads: Reads[T]
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]] = {
    tryModels(models) { model =>
      chatCompletionSingle(request.toClaudeRequest(model), outputFormat)
    }
  }

  private def tryModels[T](models: Seq[ClaudeModel])(
    attempt: ClaudeModel => Future[ValidatedNec[ClaudeError, T]]
  )(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, T]] = {
    def loop(remaining: List[ClaudeModel]): Future[ValidatedNec[ClaudeError, T]] = {
      remaining match {
        case Nil => Future.successful(ClaudeError(message = "No models provided").invalidNec)
        case model :: rest =>
          attempt(model).flatMap {
            case result @ Valid(_) => Future.successful(result)
            case result @ Invalid(errors) =>
              if (rest.nonEmpty && isOverloaded(errors)) {
                println(s"Claude model ${model} returned 529 overloaded, falling back to ${rest.head}")
                loop(rest)
              } else {
                Future.successful(result)
              }
          }
      }
    }
    loop(models.toList)
  }

  private def isOverloaded(errors: NonEmptyChain[ClaudeError]): Boolean =
    errors.exists(e => ClaudeClient.isOverloadedError(e.message))

  private def chatCompletionSingle[T](originalRequest: ClaudeRequest, outputFormat: ClaudeOutputFormat)(implicit
    ec: ExecutionContext,
    reads: Reads[T]
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]] = {
    val request = originalRequest.copy(
      outputFormat = Some(outputFormat.toApi)
    )
    val rm = ClaudeRequestMetadata(client, randomId("req"), request)
    store.storeRequest(rm)
    client
      .createMessage(
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

  private def chatTextSingle(originalRequest: ClaudeRequest)(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, String]] = {
    val rm = ClaudeRequestMetadata(client, randomId("req"), originalRequest)
    store.storeRequest(rm)
    client
      .createMessage(
        originalRequest,
        requestHeaders = defaultHeaders
      )
      .map { response =>
        val text = response.content.map(_.text).mkString("\n")
        if (text.nonEmpty) {
          ClaudeResponseMetadata(rm, response, text).validNec
        } else {
          rm.error("No content found in message").invalidNec
        }
      }
      .recover {
        case r: ClaudeErrorResponseResponse => r.claudeErrorResponse.error.invalidNec
        case NonFatal(e) => rm.error(e.getMessage).invalidNec
      }
      .map { res =>
        storeResponse(rm, res)
        res.map(_.content)
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
              "type" -> "integer"
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

  val InsightSectionsResponse: ClaudeOutputFormat = ClaudeOutputFormats.create(
    "insight_sections_response",
    Json.obj(
      "steps" -> stepsProperty,
      "sections" -> Json.obj(
        "type" -> "array",
        "items" -> Json.obj(
          "type" -> "object",
          "properties" -> Json.obj(
            "title" -> Json.obj("type" -> "string"),
            "icon" -> Json.obj("type" -> "string"),
            "items" -> Json.obj("type" -> "array", "items" -> Json.obj("type" -> "string")),
            "subsections" -> Json.obj(
              "type" -> "array",
              "items" -> Json.obj(
                "type" -> "object",
                "properties" -> Json.obj(
                  "heading" -> Json.obj("type" -> "string"),
                  "items" -> Json.obj("type" -> "array", "items" -> Json.obj("type" -> "string"))
                ),
                "required" -> Json.arr("heading", "items"),
                "additionalProperties" -> false
              )
            )
          ),
          "required" -> Json.arr("title", "icon"),
          "additionalProperties" -> false
        )
      )
    ),
    Seq("steps", "sections")
  )

  val all: List[ClaudeOutputFormat] =
    List(CommentsResponse, RecommendationsResponse, SingleInsight, InsightSectionsResponse)
}
