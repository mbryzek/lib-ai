package com.mbryzek.ai.claude

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits.*
import com.bryzek.claude.v0.interfaces.Client
import com.bryzek.claude.v0.models.*
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TestClaudeClient extends Client {
  override def baseUrl: String = "http://mock.localhost"
  override def messages = new TestMessages
}

sealed trait TestResponseFormat {
  def format: ResponseFormat

  def generateResponse: JsValue
  protected def generateSteps: JsObject = {
    Json.obj(
      "steps" -> Seq(
        Map(
          "explanation" -> "Test explanation",
          "output" -> "Test result"
        )
      )
    )
  }
}

object TestResponseFormat {
  val Comments: TestResponseFormat = new TestResponseFormat {
    override val format: ResponseFormat = ResponseFormat.Comments

    override def generateResponse: JsValue = generateSteps ++ Json.toJsObject(
      Map(
        "comments" -> Seq(
          "Test comment"
        )
      )
    )
  }

  val Recommendations: TestResponseFormat = new TestResponseFormat {
    override val format: ResponseFormat = ResponseFormat.Recommendations

    override def generateResponse: JsValue = Json.toJson(
      generateSteps ++ Json.obj(
        "recommendations" -> Json.toJson(
          Seq(
            Json.obj("category" -> "coffee", "confidence" -> 75),
            Json.obj("category" -> "restaurants", "confidence" -> 50)
          )
        )
      )
    )
  }

  val SingleInsight: TestResponseFormat = new TestResponseFormat {
    override val format: ResponseFormat = ResponseFormat.SingleInsight

    override def generateResponse: JsValue = Json.toJson(
      generateSteps ++ Json.obj(
        "insight" -> "You are doing amazing"
      )
    )
  }
}

class TestMessages extends com.bryzek.claude.v0.Messages {
  private def validateResponseType(system: Option[String]): ValidatedNec[String, TestResponseFormat] = {
    system.toValidNec("Request does not have a system message - cannot identify expected response type").andThen {
      system =>
        ResponseFormat.all
          .find { f =>
            system.contains(f.structure)
          }
          .toValidNec(s"Could not identify response format from system message: $system")
          .andThen(toTestResponseType)
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

  def post(
    claudeRequest: ClaudeRequest,
    requestHeaders: Seq[(String, String)] = Nil
  )(implicit ec: ExecutionContext): Future[ClaudeResponse] = Future {
    val format = expectValid {
      validateResponseType(claudeRequest.system)
    }
    println("format: " + format.generateResponse.toString)
    ClaudeResponse(
      id = "test-response-id",
      `type` = "message",
      role = ClaudeRole.Assistant,
      content = Seq(
        ClaudeResponseContent(
          `type` = ClaudeContentType.Text,
          text = Json.prettyPrint(
            format.generateResponse
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
