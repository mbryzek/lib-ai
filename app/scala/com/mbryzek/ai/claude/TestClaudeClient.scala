package com.mbryzek.ai.claude

import com.bryzek.claude.v0.interfaces.Client
import com.bryzek.claude.v0.models.*
import com.bryzek.claude.response.v0.models.*
import com.bryzek.claude.response.v0.models.json.*
import play.api.libs.json.Json

import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TestClaudeClient extends Client {
  override def baseUrl: String = "http://mock.localhost"
  override def messages = new TestMessages
}

class TestMessages extends com.bryzek.claude.v0.Messages {
  def post(
    claudeRequest: ClaudeRequest,
    requestHeaders: Seq[(String, String)] = Nil
  )(implicit ec: ExecutionContext): Future[ClaudeResponse] = Future.successful {
    ClaudeResponse(
      id = "test-response-id",
      `type` = "message",
      role = ClaudeRole.Assistant,
      content = Seq(
        ClaudeResponseContent(
          `type` = ClaudeContentType.Text,
          text = Json.toJson(
            CommentsResponse(
              steps = Seq(ClaudeStep(explanation = "Test explanation", output = "Test output")),
              comments = Seq("This is a test response from Claude.")
            )
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
