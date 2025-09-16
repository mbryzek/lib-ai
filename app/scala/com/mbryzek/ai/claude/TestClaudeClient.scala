package com.mbryzek.ai.claude

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits.*
import com.bryzek.claude.response.v0.models.*
import com.bryzek.claude.response.v0.models.json.*
import com.bryzek.claude.v0.interfaces.Client
import com.bryzek.claude.v0.models.*
import play.api.libs.json.{JsValue, Json}

import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TestClaudeClient extends Client {
  override def baseUrl: String = "http://mock.localhost"
  override def messages = new TestMessages

  private def validateResponseType(system: Option[String]): ValidatedNec[String, TestResponseFormat] = {
    system
      .toValidNec("Request does not have a system message - cannot identify expected response type")
      .andThen(validateResponseType)
  }

  protected def validateResponseType(system: String): ValidatedNec[String, TestResponseFormat] = {
    ResponseFormat.all
      .find { f =>
        system.contains(f.structure)
      }
      .toValidNec(s"Could not identify response format from system message: $system")
      .andThen(toTestResponseType)
  }

  def postClaudeRequest(
    claudeRequest: ClaudeRequest,
    format: TestResponseFormat,
    requestHeaders: Seq[(String, String)] = Nil
  ): JsValue = {
    format.generateResponse(claudeRequest)
  }

  class TestMessages extends com.bryzek.claude.v0.Messages {
    def post(
      claudeRequest: ClaudeRequest,
      requestHeaders: Seq[(String, String)] = Nil
    )(implicit ec: ExecutionContext): Future[ClaudeResponse] = Future {
      val format = expectValid {
        validateResponseType(claudeRequest.system)
      }
      ClaudeResponse(
        id = "test-response-id",
        `type` = "message",
        role = ClaudeRole.Assistant,
        content = Seq(
          ClaudeResponseContent(
            `type` = ClaudeContentType.Text,
            text = Json.prettyPrint(
              postClaudeRequest(claudeRequest, format, requestHeaders)
            )
          )
        ),
        model = ClaudeModel.ClaudeSonnet420250514,
        stopReason = ClaudeStopReason.EndTurn,
        stopSequence = None,
        usage = ClaudeUsage(
          inputTokens = 10,
          outputTokens = 20
        )
      )
    }
  }

  private def toTestResponseType(f: ResponseFormat): ValidatedNec[String, TestResponseFormat] = {
    f match {
      case ResponseFormat.Comments => TestResponseFormat.Comments.validNec
      case ResponseFormat.Recommendations => TestResponseFormat.Recommendations.validNec
      case ResponseFormat.SingleInsight => TestResponseFormat.SingleInsight.validNec
      case other => s"Could not find test response format for class ${other.getClass.getName}".invalidNec
    }
  }

  private def expectValid[T](r: ValidatedNec[String, T]): T = {
    r match {
      case Invalid(e) => sys.error(e.toNonEmptyList.toList.mkString(", "))
      case Valid(result) => result
    }
  }
}

abstract class TestResponseFormat(format: ResponseFormat) {

  def generateResponse(claudeRequest: ClaudeRequest): JsValue

  protected def steps: Seq[ClaudeStep] = Seq(
    ClaudeStep(
      explanation = "Test explanation",
      output = "Test result"
    )
  )
}

object TestResponseFormat {
  val Comments: TestResponseFormat = new TestResponseFormat(ResponseFormat.Comments) {

    override def generateResponse(claudeRequest: ClaudeRequest): JsValue = buildJs(
      Seq("Test comment")
    )

    def buildJs(comments: Seq[String]): JsValue = Json.toJson(
      CommentsResponse(
        steps = steps,
        comments = comments
      )
    )
  }

  val Recommendations: TestResponseFormat = new TestResponseFormat(ResponseFormat.Recommendations) {
    override def generateResponse(claudeRequest: ClaudeRequest): JsValue = buildJs(
      Seq(
        Recommendation(category = "coffee", confidence = 75),
        Recommendation(category = "restaurants", confidence = 50)
      )
    )

    def buildJs(recommendations: Seq[Recommendation]): JsValue = Json.toJson(
      RecommendationResponse(
        steps = steps,
        recommendations = recommendations
      )
    )

  }

  val SingleInsight: TestResponseFormat = new TestResponseFormat(ResponseFormat.SingleInsight) {
    override def generateResponse(claudeRequest: ClaudeRequest): JsValue = buildJs("You are doing amazing")

    def buildJs(insight: String): JsValue = Json.toJson(
      SingleInsightResponse(
        steps = steps,
        insight = insight
      )
    )
  }
}
