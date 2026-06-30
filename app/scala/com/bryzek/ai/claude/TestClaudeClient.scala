package com.bryzek.ai.claude

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits.*
import com.bryzek.claude.response.models.*
import com.bryzek.claude.response.models.json.*
import com.bryzek.claude.client.IClient
import com.bryzek.claude.models.*
import play.api.libs.json.{JsValue, Json}

import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object TestClaudeClient {
  val OutputFormatNameHeader: String = "X-Output-Format"
}

@Singleton
class TestClaudeClient extends IClient {

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
    @scala.annotation.nowarn("msg=unused") requestHeaders: Seq[(String, String)] = Nil
  ): JsValue = {
    format.generateResponse(claudeRequest)
  }

  override def createMessage(
    body: ClaudeRequest,
    requestHeaders: Seq[(String, String)] = Nil
  ): Future[ClaudeResponse] = Future {
    val responseText = getHeader(requestHeaders, TestClaudeClient.OutputFormatNameHeader) match {
      case None =>
        "This is a test plain text response."
      case Some(name) =>
        val format = expectValid { validateOutputFormatByName(name) }
        Json.prettyPrint(postClaudeRequest(body, format, requestHeaders))
    }
    ClaudeResponse(
      id = "test-response-id",
      `type` = "message",
      role = ClaudeRole.Assistant,
      content = Seq(
        ClaudeResponseContent(
          `type` = ClaudeContentType.Text,
          text = responseText
        )
      ),
      model = ClaudeModel.ClaudeSonnet5,
      stopReason = ClaudeStopReason.EndTurn,
      stopSequence = None,
      usage = ClaudeUsage(
        inputTokens = 10,
        outputTokens = 20
      )
    )
  }

  private def toTestResponseType(f: ClaudeOutputFormat): ValidatedNec[String, TestResponseFormat] = {
    f match {
      case ClaudeOutputFormats.CommentsResponse => TestResponseFormat.Comments.validNec
      case ClaudeOutputFormats.RecommendationsResponse => TestResponseFormat.Recommendations.validNec
      case ClaudeOutputFormats.SingleInsight => TestResponseFormat.SingleInsight.validNec
      case ClaudeOutputFormats.InsightSectionsResponse => TestResponseFormat.InsightSections.validNec
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

abstract class TestResponseFormat(@scala.annotation.nowarn("msg=unused") schema: ClaudeOutputFormat) {

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

  val InsightSections: TestResponseFormat = new TestResponseFormat(ClaudeOutputFormats.InsightSectionsResponse) {
    override def generateResponse(claudeRequest: ClaudeRequest): JsValue = buildJs(
      Seq(
        InsightSection(
          title = "Test Section",
          icon = "chart",
          items = Some(Seq("Test insight item"))
        )
      )
    )

    def buildJs(sections: Seq[InsightSection]): JsValue = Json.toJson(
      InsightSectionsResponse(
        steps = steps,
        sections = sections
      )
    )
  }
}
