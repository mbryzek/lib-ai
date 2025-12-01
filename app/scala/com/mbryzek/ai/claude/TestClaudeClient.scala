package com.mbryzek.ai.claude

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits.*
import com.bryzek.claude.response.v0.models.*
import com.bryzek.claude.response.v0.models.json.*
import com.bryzek.claude.v0.interfaces.Client
import com.bryzek.claude.v0.models.*
import play.api.libs.json.{JsObject, JsValue, Json}

import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}

object TestClaudeClient {
  val OutputFormatNameHeader: String = "X-Output-Format"
}

@Singleton
class TestClaudeClient extends Client {
  override def baseUrl: String = "http://mock.localhost"
  override def messages = new TestMessages

  private def getHeader(requestHeaders: Seq[(String, String)], name: String): Option[String] = {
    requestHeaders.find(_._1.toLowerCase.trim == name.toLowerCase.trim).map(_._2).toList.distinct match {
      case Nil => None
      case one :: Nil => Some(one)
      case multiple =>
        sys.error(s"Found multiple request headers named '$name' with values: ${multiple.mkString(", ")}")
    }
  }

  protected def validateOutputFormatByName(name: String): ValidatedNec[String, TestResponseFormat] = {
    ClaudeOutputFormats.all
      .find(_.name == name)
      .toValidNec(s"Could not identify json schema from object")
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
      val name = getHeader(requestHeaders, TestClaudeClient.OutputFormatNameHeader).getOrElse {
        sys.error(
          s"Missing header ${TestClaudeClient.OutputFormatNameHeader}. Available headers: " + requestHeaders
            .map(_._1)
            .mkString(
              ", "
            )
        )
      }
      val format = expectValid {
        validateOutputFormatByName(name)
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
        model = ClaudeModel.ClaudeSonnet45,
        stopReason = ClaudeStopReason.EndTurn,
        stopSequence = None,
        usage = ClaudeUsage(
          inputTokens = 10,
          outputTokens = 20
        )
      )
    }
  }

  private def toTestResponseType(f: ClaudeOutputFormat): ValidatedNec[String, TestResponseFormat] = {
    f match {
      case ClaudeOutputFormats.CommentsResponse => TestResponseFormat.Comments.validNec
      case ClaudeOutputFormats.RecommendationsResponse => TestResponseFormat.Recommendations.validNec
      case ClaudeOutputFormats.SingleInsight => TestResponseFormat.SingleInsight.validNec
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

abstract class TestResponseFormat(schema: ClaudeOutputFormat) {

  def generateResponse(claudeRequest: ClaudeRequest): JsValue

  protected def steps: Seq[ClaudeStep] = Seq(
    ClaudeStep(
      explanation = "Test explanation",
      output = "Test result"
    )
  )
}

object TestResponseFormat {
  val Comments: TestResponseFormat = new TestResponseFormat(ClaudeOutputFormats.CommentsResponse) {

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

  val Recommendations: TestResponseFormat = new TestResponseFormat(ClaudeOutputFormats.RecommendationsResponse) {
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

  val SingleInsight: TestResponseFormat = new TestResponseFormat(ClaudeOutputFormats.SingleInsight) {
    override def generateResponse(claudeRequest: ClaudeRequest): JsValue = buildJs("You are doing amazing")

    def buildJs(insight: String): JsValue = Json.toJson(
      SingleInsightResponse(
        steps = steps,
        insight = insight
      )
    )
  }
}
