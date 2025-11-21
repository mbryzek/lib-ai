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

  private def validateOutputFormat(
    outputFormat: Option[ClaudeOutputFormat]
  ): ValidatedNec[String, TestResponseFormat] = {
    outputFormat
      .toValidNec("Request does not have an output format - cannot identify expected response type")
      .andThen { f => validateJsonSchema(f.schema) }
  }

  protected def validateJsonSchema(jsonSchema: ClaudeJsonSchema): ValidatedNec[String, TestResponseFormat] = {
    ClaudeJsonSchemas.all
      .find { f =>
        f.name == jsonSchema.name
      }
      .toValidNec(s"Could not identify json schema with name: ${jsonSchema.name}")
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
        validateOutputFormat(claudeRequest.outputFormat)
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

  private def toTestResponseType(f: ClaudeJsonSchema): ValidatedNec[String, TestResponseFormat] = {
    f match {
      case ClaudeJsonSchemas.CommentsResponse => TestResponseFormat.Comments.validNec
      case ClaudeJsonSchemas.RecommendationsResponse => TestResponseFormat.Recommendations.validNec
      case ClaudeJsonSchemas.SingleInsight => TestResponseFormat.SingleInsight.validNec
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

abstract class TestResponseFormat(schema: ClaudeJsonSchema) {

  def generateResponse(claudeRequest: ClaudeRequest): JsValue

  protected def steps: Seq[ClaudeStep] = Seq(
    ClaudeStep(
      explanation = "Test explanation",
      output = "Test result"
    )
  )
}

object TestResponseFormat {
  val Comments: TestResponseFormat = new TestResponseFormat(ClaudeJsonSchemas.CommentsResponse) {

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

  val Recommendations: TestResponseFormat = new TestResponseFormat(ClaudeJsonSchemas.RecommendationsResponse) {
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

  val SingleInsight: TestResponseFormat = new TestResponseFormat(ClaudeJsonSchemas.SingleInsight) {
    override def generateResponse(claudeRequest: ClaudeRequest): JsValue = buildJs("You are doing amazing")

    def buildJs(insight: String): JsValue = Json.toJson(
      SingleInsightResponse(
        steps = steps,
        insight = insight
      )
    )
  }
}
