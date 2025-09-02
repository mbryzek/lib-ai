package com.mbryzek.ai.claude

import com.bryzek.claude.v0.interfaces.Client
import com.bryzek.claude.v0.models.*

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
          text =
            """{"steps": [{"explanation": "Test response", "output": "Test"}], "comments": ["This is a test response from Claude."]}"""
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
