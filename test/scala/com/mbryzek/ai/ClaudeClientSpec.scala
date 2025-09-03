package com.mbryzek.ai.claude

import com.bryzek.claude.v0.models.{ClaudeModel, ClaudeRequest, ClaudeRole}
import helpers.FutureHelpers
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

import scala.concurrent.ExecutionContext.Implicits.global

class ClaudeClientSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with FutureHelpers {

  private val testClient = app.injector.instanceOf[TestClaudeClient]

  "ClaudeClient" should {

    "parse mock response correctly" in {

      val response = await(
        testClient.messages.post(
          ClaudeRequest(
            model = ClaudeModel.ClaudeSonnet420250514,
            system = Some("You are a test assistant."),
            messages = Seq(
              ClaudeClient.makeClaudeMessage(ClaudeRole.User, "Test message")
            )
          )
        )
      )

      response.role.must(be(ClaudeRole.Assistant))
      response.model.must(be(ClaudeModel.ClaudeSonnet420250514))
      response.content.head.text.must(include("test response"))
    }

  }

}
