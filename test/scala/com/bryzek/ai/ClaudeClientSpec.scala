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
      result.toOption.get.content must not be empty
      result.toOption.get.response.usage.outputTokens must be > 0L
    }

    "toClaudeRequest wraps system in a single block" in {
      val r = AiRequest(messages = Nil, system = Some("hello"))
      val out = r.toClaudeRequest(ClaudeModel.ClaudeSonnet46)
      out.system.map(_.size) mustBe Some(1)
      out.system.get.head.text mustBe "hello"
      out.system.get.head.cacheControl mustBe None
    }

    "toClaudeRequest tags system with cache_control when cacheSystem=true" in {
      val r = AiRequest(messages = Nil, system = Some("ctx"), cacheSystem = true)
      val out = r.toClaudeRequest(ClaudeModel.ClaudeSonnet46)
      out.system.get.head.cacheControl.map(_.`type`) mustBe Some(com.bryzek.claude.models.ClaudeCacheType.Ephemeral)
    }

    "toClaudeRequest tags last message content with cache_control when cacheLastMessage=true" in {
      val msgs = Seq(
        ClaudeClient.makeClaudeMessage(ClaudeRole.User, "first"),
        ClaudeClient.makeClaudeMessage(ClaudeRole.Assistant, "reply"),
        ClaudeClient.makeClaudeMessage(ClaudeRole.User, "second")
      )
      val out = AiRequest(messages = msgs, cacheLastMessage = true).toClaudeRequest(ClaudeModel.ClaudeSonnet46)
      out.messages.init.flatMap(_.content).flatMap(_.cacheControl) mustBe Nil
      out.messages.last.content.last.cacheControl.map(_.`type`) mustBe Some(
        com.bryzek.claude.models.ClaudeCacheType.Ephemeral
      )
    }

  }

}
