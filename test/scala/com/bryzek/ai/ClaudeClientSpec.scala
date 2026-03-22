package com.bryzek.ai.claude

import com.bryzek.claude.models.{ClaudeModel, ClaudeRole}
import helpers.FutureHelpers
import org.apache.pekko.util.Timeout
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{FiniteDuration, SECONDS}

class ClaudeClientSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with FutureHelpers {

  private val testClient: ClaudeClient = {
    val factory = app.injector.instanceOf[ClaudeClientFactory]
    factory.instance(ClaudeEnvironment.Sandbox, "test-api-key")(NoopClaudeStore)
  }
  private implicit val timeout: Timeout = FiniteDuration(30, SECONDS)

  "ClaudeClient" should {
    val models = Seq(ClaudeModel.ClaudeSonnet46)
    val request = AiRequest(
      messages = Seq(
        ClaudeClient.makeClaudeMessage(ClaudeRole.User, "Sending a test message")
      )
    )

    "chatComments" in {
      await(
        testClient.chatComments(request, models)
      )(using timeout)
    }

    "chatRecommendations" in {
      await(
        testClient.chatRecommendations(request, models)
      )(using timeout)
    }

    "chatInsight" in {
      await(
        testClient.chatInsight(request, models)
      )(using timeout)
    }

    "chatSingleInsight" in {
      await(
        testClient.chatSingleInsight(request, models)
      )(using timeout)
    }

    "chatText" in {
      val result = await(
        testClient.chatText(request, models)
      )(using timeout)
      result.isValid mustBe true
      result.toOption.get must not be empty
    }
  }

}
