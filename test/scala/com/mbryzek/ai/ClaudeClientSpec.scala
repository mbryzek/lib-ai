package com.mbryzek.ai.claude

import com.bryzek.claude.v0.models.{ClaudeError, ClaudeModel, ClaudeRequest, ClaudeResponse, ClaudeRole}
import helpers.FutureHelpers
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

import scala.concurrent.ExecutionContext.Implicits.global

class ClaudeClientSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with FutureHelpers {

  private val testClient: ClaudeClient = {
    val factory = app.injector.instanceOf[ClaudeClientFactory]
    factory.instance(ClaudeEnvironment.Sandbox, "test-api-key")(NoopClaudeStore)
  }

  "ClaudeClient" should {

    "chatComments" in {
      await(
        testClient.chatComments(
          ClaudeRequest(
            model = ClaudeModel.ClaudeSonnet420250514,
            messages = Seq(
              ClaudeClient.makeClaudeMessage(ClaudeRole.User, "Sending a test message")
            )
          )
        )
      )
    }

  }

}
