package com.mbryzek.ai.claude

import com.bryzek.claude.v0.models.{ClaudeError, ClaudeModel, ClaudeRequest, ClaudeResponse, ClaudeRole}
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
    val request = ClaudeRequest(
      model = ClaudeModel.ClaudeSonnet45,
      messages = Seq(
        ClaudeClient.makeClaudeMessage(ClaudeRole.User, "Sending a test message")
      )
    )

    "chatComments" in {
      await(
        testClient.chatComments(request)
      )(using timeout)
    }

    "chatRecommendations" in {
      await(
        testClient.chatRecommendations(request)
      )(using timeout)
    }

    "chatInsight" in {
      await(
        testClient.chatInsight(request)
      )(using timeout)
    }

    "chatSingleInsight" in {
      await(
        testClient.chatSingleInsight(request)
      )(using timeout)
    }
  }

}
