package com.mbryzek.ai.claude

import cats.implicits.*
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.implicits.*
import com.bryzek.claude.response.v0.models.*
import com.bryzek.claude.v0.models.json.*
import com.bryzek.claude.v0.errors.ClaudeErrorResponseResponse
import com.bryzek.claude.v0.interfaces.Client
import com.bryzek.claude.v0.models.*
import com.bryzek.claude.v0.models.json.*
import play.api.libs.json.*

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

sealed trait ClaudeEnvironment
object ClaudeEnvironment {
  case object Sandbox extends ClaudeEnvironment
  case object Production extends ClaudeEnvironment
}

class ClaudeClient @Inject() (
  acumenConfig: AcumenConfig,
  requestsDao: RequestsDao,
  responsesDao: ResponsesDao,
  clients: ClaudeClients,
  claudeEC: ClaudeEC
) {
  private val defaultHeaders = Seq(
    "x-api-key" -> acumenConfig.claudeConfig.key,
    "Content-Type" -> "application/json",
    "anthropic-version" -> acumenConfig.claudeConfig.anthropicVersion
  )

  private case class RequestMetadata(id: String) {
    val start: Long = System.currentTimeMillis()
    def error(msg: String, raw: Option[String] = None): ClaudeError =
      ClaudeError(message = s"$msg [Request ID: $id]", raw = raw)
  }

  private case class ClaudeContentResponse[T](response: ClaudeResponse, content: T)

  def makeClaudeMessage(role: ClaudeRole, msg: String*): ClaudeMessage = {
    ClaudeMessage(
      role = role,
      content = msg.map { m => ClaudeContent(ClaudeContentType.Text, m) }
    )
  }

  def chatComments(env: Environment, request: ClaudeRequest)(implicit
    ec: ExecutionContext = claudeEC
  ): Future[ValidatedNec[ClaudeError, Seq[String]]] = {
    chatCompletion[CommentsResponse](env, request, ResponseFormat.Comments)(using ec).map(_.map(_.content.comments))
  }

  def chatRecommendations(env: Environment, request: ClaudeRequest)(implicit
    ec: ExecutionContext = claudeEC
  ): Future[ValidatedNec[ClaudeError, Seq[Recommendation]]] = {
    chatCompletion[RecommendationResponse](env, request, ResponseFormat.Recommendations)(using ec)
      .map(_.map(_.content.recommendations))
  }

  def chatInsight(env: Environment, request: ClaudeRequest)(implicit
    ec: ExecutionContext = claudeEC
  ): Future[ValidatedNec[ClaudeError, Seq[String]]] = {
    chatComments(env, request)(using ec)
  }

  def chatSingleInsight(env: Environment, request: ClaudeRequest)(implicit
    ec: ExecutionContext = claudeEC
  ): Future[ValidatedNec[ClaudeError, String]] = {
    chatCompletion[SingleInsightResponse](env, request, ResponseFormat.SingleInsight)(using ec)
      .map(_.map(_.content.insight))
  }

  private def chatCompletion[T](env: Environment, originalRequest: ClaudeRequest, responseFormat: String)(implicit
    ec: ExecutionContext = claudeEC,
    reads: Reads[T]
  ): Future[ValidatedNec[ClaudeError, ClaudeContentResponse[T]]] = {
    val client = clients.get(env)
    val request = originalRequest.copy(
      system = originalRequest.system match {
        case None => Some(responseFormat)
        case Some(s) => Some(s + ". " + responseFormat)
      }
    )
    val rm = RequestMetadata(storeRequest(client, request))
    client.messages
      .post(request, requestHeaders = defaultHeaders)
      .map(parseContent[T](rm, _))
      .recover {
        case r: ClaudeErrorResponseResponse => r.claudeErrorResponse.error.invalidNec
        case NonFatal(e) => rm.error(e.getMessage).invalidNec
      }
      .map(storeResponse(rm))
  }

  private def parseContent[T](rm: RequestMetadata, response: ClaudeResponse)(implicit
    reads: Reads[T]
  ): ValidatedNec[ClaudeError, ClaudeContentResponse[T]] = {
    response.content.map(_.text).mkString("\n") match {
      case content if content.nonEmpty => parseContent[T](rm, response, content)
      case _ => rm.error("No content found in message").invalidNec
    }
  }

  private def parseContent[T](rm: RequestMetadata, response: ClaudeResponse, content: String)(implicit
    reads: Reads[T]
  ): ValidatedNec[ClaudeError, ClaudeContentResponse[T]] = {
    def parseError(msg: String) = {
      rm.error(msg, raw = Some(response.content.map(_.text).mkString("\n"))).invalidNec
    }

    Try(Json.parse(content)) match {
      case Failure(_) => parseError("Content is not valid JSON")
      case Success(js) =>
        js.validate[T] match {
          case JsSuccess(value, _) => ClaudeContentResponse(response, value).validNec
          case JsError(errors) => {
            val messages = errors.flatMap(e => e._2.map(m => s"${e._1}: ${m.message}"))
            parseError(s"Content is not valid: ${messages.mkString(", ")}")
          }
        }
    }
  }

  private def storeRequest(client: Client, request: ClaudeRequest): String = {
    requestsDao.insert(
      AcumenConstants.SystemUser.id,
      RequestForm(
        client = client.getClass.getName,
        raw = Json.toJson(request)
      )
    )
  }

  private def storeResponse[T](request: RequestMetadata)(
    response: ValidatedNec[ClaudeError, ClaudeContentResponse[T]]
  ): ValidatedNec[ClaudeError, ClaudeContentResponse[T]] = {
    response match {
      case Invalid(e) => storeResponseError(request, e)
      case Valid(e) => storeResponseValid(request, e)
    }
    response
  }

  private def storeResponseError(request: RequestMetadata, error: NonEmptyChain[ClaudeError]): Unit = {
    responsesDao.insert(
      AcumenConstants.SystemUser.id,
      emptyResponseForm(request).copy(
        error = Some(Json.toJson(error.head)),
        raw = error.head.raw
      )
    )
  }

  private def storeResponseValid(request: RequestMetadata, response: ClaudeContentResponse[?]): Unit = {
    val r = response.response

    responsesDao.insert(
      AcumenConstants.SystemUser.id,
      emptyResponseForm(request).copy(
        raw = Some(r.content.map(_.text).mkString("\n")),
        `type` = Some(r.`type`),
        model = Some(r.model.toString),
        role = Some(r.role.toString),
        content = Some(r.content.map(c => Json.toJson(c).asInstanceOf[JsObject])).filterNot(_.isEmpty),
        stopReason = Some(r.stopReason.toString),
        stopSequence = r.stopSequence,
        usageInputTokens = Some(r.usage.inputTokens),
        usageOutputTokens = Some(r.usage.outputTokens)
      )
    )
  }

  private def emptyResponseForm(request: RequestMetadata): ResponseForm = {
    ResponseForm(
      requestId = request.id,
      durationMs = System.currentTimeMillis() - request.start,
      error = None,
      raw = None,
      `type` = None,
      model = None,
      role = None,
      content = None,
      stopReason = None,
      stopSequence = None,
      usageInputTokens = None,
      usageOutputTokens = None
    )
  }

  private object ResponseFormat {
    private val Steps: String = """
      "steps": [
        { "explanation": "Brief explanation of your analysis step",
          "output": "The result or finding from this step"
        }
      ]
    """.strip

    val Comments: String = buildJsonMessage(s"""{
      $Steps,
      "comments": [
        "Your main response or advice as a string",
        "Additional comments if needed"
      ]
    }""")

    val Recommendations: String = buildJsonMessage(s"""{
      $Steps,
      "recommendations": [
        [{"category":"name","confidence":75},
         {"category":"second name","confidence":50}
        ]
      ]
    }""")

    val SingleInsight: String = buildJsonMessage(s"""{
      $Steps,
      "insight":"Your insightful comment"
    }""")

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
}
