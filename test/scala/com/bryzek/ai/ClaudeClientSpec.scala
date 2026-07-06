package com.bryzek.ai.claude

import com.bryzek.claude.models.{
  ClaudeEffort,
  ClaudeModel,
  ClaudeResponse,
  ClaudeRole,
  ClaudeThinkingType,
  ClaudeTool,
  ClaudeToolChoiceType
}
import com.bryzek.claude.models.json.*
import com.bryzek.claude.response.models.SingleInsightResponse
import com.bryzek.claude.response.models.json.*
import com.bryzek.claude.client.IClient
import helpers.FutureHelpers
import play.api.libs.json.Json
import org.apache.pekko.util.Timeout
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

import java.io.IOException
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{FiniteDuration, SECONDS}

class ClaudeClientSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with FutureHelpers {

  private val testClient: ClaudeClient = {
    val factory = app.injector.instanceOf[ClaudeClientFactory]
    factory.instance(ClaudeEnvironment.Sandbox, "test-api-key")(NoopClaudeStore)
  }
  private implicit val timeout: Timeout = FiniteDuration(30, SECONDS)

  "ClaudeClient" should {
    val models = Seq(ClaudeModel.ClaudeSonnet5)
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
      val out = r.toClaudeRequest(ClaudeModel.ClaudeSonnet5)
      out.system.map(_.size) mustBe Some(1)
      out.system.get.head.text mustBe "hello"
      out.system.get.head.cacheControl mustBe None
    }

    "toClaudeRequest tags system with cache_control when cacheSystem=true" in {
      val r = AiRequest(messages = Nil, system = Some("ctx"), cacheSystem = true)
      val out = r.toClaudeRequest(ClaudeModel.ClaudeSonnet5)
      out.system.get.head.cacheControl.map(_.`type`) mustBe Some(com.bryzek.claude.models.ClaudeCacheType.Ephemeral)
    }

    "toClaudeRequest tags last message content with cache_control when cacheLastMessage=true" in {
      val msgs = Seq(
        ClaudeClient.makeClaudeMessage(ClaudeRole.User, "first"),
        ClaudeClient.makeClaudeMessage(ClaudeRole.Assistant, "reply"),
        ClaudeClient.makeClaudeMessage(ClaudeRole.User, "second")
      )
      val out = AiRequest(messages = msgs, cacheLastMessage = true).toClaudeRequest(ClaudeModel.ClaudeSonnet5)
      out.messages.init.flatMap(_.content).flatMap(_.cacheControl) mustBe Nil
      out.messages.last.content.last.cacheControl.map(_.`type`) mustBe Some(
        com.bryzek.claude.models.ClaudeCacheType.Ephemeral
      )
    }

    "toClaudeRequest defaults to adaptive thinking" in {
      val out = AiRequest(messages = Nil).toClaudeRequest(ClaudeModel.ClaudeSonnet5)
      out.thinking.map(_.`type`) mustBe Some(ClaudeThinkingType.Adaptive)
    }

    "toClaudeRequest can disable thinking" in {
      val out =
        AiRequest(messages = Nil, thinking = ClaudeThinkingType.Disabled).toClaudeRequest(ClaudeModel.ClaudeSonnet5)
      out.thinking.map(_.`type`) mustBe Some(ClaudeThinkingType.Disabled)
    }

    "toClaudeRequest sets effort in output_config when provided" in {
      val out = AiRequest(messages = Nil, effort = Some(ClaudeEffort.High)).toClaudeRequest(ClaudeModel.ClaudeSonnet5)
      out.outputConfig.flatMap(_.effort) mustBe Some(ClaudeEffort.High)
      out.outputConfig.flatMap(_.format) mustBe None
    }

    "toClaudeRequest omits output_config when no effort provided" in {
      AiRequest(messages = Nil).toClaudeRequest(ClaudeModel.ClaudeSonnet5).outputConfig mustBe None
    }

    "parses a response whose first content block is a non-text (thinking) block" in {
      // Sonnet 5 leads its content array with a thinking block, which carries no `text`
      // field; the flat content-block model reads it and the client skips it, still
      // extracting the trailing text block.
      val js = Json.parse(
        """
        {
          "id": "msg_x",
          "type": "message",
          "role": "assistant",
          "content": [
            { "type": "thinking", "thinking": "", "signature": "sig" },
            { "type": "text", "text": "the answer" }
          ],
          "model": "claude-sonnet-5",
          "stop_reason": "end_turn",
          "usage": { "input_tokens": 10, "output_tokens": 20 }
        }
        """
      )
      val response = js.as[ClaudeResponse]
      response.content.size mustBe 2
      response.content.head.text mustBe None
      response.content.flatMap(_.text) mustBe Seq("the answer")
    }

    "parses usage with cache token fields" in {
      val js = Json.parse(
        """
        { "input_tokens": 10, "output_tokens": 20,
          "cache_creation_input_tokens": 100, "cache_read_input_tokens": 200 }
        """
      )
      val usage = js.as[com.bryzek.claude.models.ClaudeUsage]
      usage.cacheCreationInputTokens mustBe Some(100L)
      usage.cacheReadInputTokens mustBe Some(200L)
    }

    "ClaudeToolChoiceType.None renders as none on the wire" in {
      ClaudeToolChoiceType.None.toString mustBe "none"
    }

    "preserves a redacted_thinking block's data field for verbatim echo-back" in {
      val js = Json.parse(
        """
        {
          "id": "msg_x",
          "type": "message",
          "role": "assistant",
          "content": [
            { "type": "redacted_thinking", "data": "ENCRYPTED" },
            { "type": "text", "text": "the answer" }
          ],
          "model": "claude-sonnet-5",
          "stop_reason": "end_turn",
          "usage": { "input_tokens": 10, "output_tokens": 20 }
        }
        """
      )
      val response = js.as[ClaudeResponse]
      val redacted = response.content.head
      redacted.`type` mustBe com.bryzek.claude.models.ClaudeContentType.RedactedThinking
      redacted.data mustBe Some("ENCRYPTED")
      redacted.text mustBe None
      response.content.flatMap(_.text) mustBe Seq("the answer")
    }
  }

  "runToolLoop" should {
    val models = Seq(ClaudeModel.ClaudeSonnet5)
    val tool = ClaudeTool(
      name = "get_metric",
      description = "Return a metric",
      inputSchema = Json.obj("type" -> "object", "properties" -> Json.obj(), "additionalProperties" -> false)
    )
    val request = AiRequest(
      messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, "Investigate the week"))
    )

    "executes the tool then returns a structured final answer" in {
      var executed = 0
      val result = await(
        testClient.runToolLoop[SingleInsightResponse](
          request,
          tools = Seq(tool),
          models = models,
          maxCalls = 25,
          finalFormat = ClaudeOutputFormats.SingleInsight
        ) { use =>
          executed += 1
          use.name mustBe "get_metric"
          scala.concurrent.Future.successful(ClaudeToolOutput(content = """{"total": 42}"""))
        }
      )(using timeout)

      result.isValid mustBe true
      val loop = result.toOption.get
      executed mustBe 1
      loop.turns mustBe 1
      loop.invocations.map(_.use.name) mustBe Seq("get_metric")
      loop.invocations.head.output.content mustBe """{"total": 42}"""
      loop.value.insight mustBe "You are doing amazing"
    }

    "finalizes directly when the tool-call budget is zero" in {
      var executed = 0
      val result = await(
        testClient.runToolLoop[SingleInsightResponse](
          request,
          tools = Seq(tool),
          models = models,
          maxCalls = 0,
          finalFormat = ClaudeOutputFormats.SingleInsight
        ) { _ =>
          executed += 1
          scala.concurrent.Future.successful(ClaudeToolOutput(content = "unused"))
        }
      )(using timeout)

      result.isValid mustBe true
      executed mustBe 0
      result.toOption.get.turns mustBe 0
      result.toOption.get.invocations mustBe empty
    }

    "retries a transient read timeout and succeeds" in {
      val calls = new AtomicInteger(0)
      val client = flakyClient { n =>
        if (n == 1) Some(new TimeoutException("Read timeout to api.anthropic.com/160.79.104.10:443 after 120000 ms"))
        else None
      }(calls)

      val result = await(client.chatText(request, models))(using timeout)

      result.isValid mustBe true
      calls.get mustBe 2
    }

    "retries a dropped connection and succeeds" in {
      val calls = new AtomicInteger(0)
      val client = flakyClient { n =>
        if (n == 1) Some(new IOException("Remotely closed")) else None
      }(calls)

      val result = await(client.chatText(request, models))(using timeout)

      result.isValid mustBe true
      calls.get mustBe 2
    }

    "gives up after exhausting attempts on a persistent timeout" in {
      val calls = new AtomicInteger(0)
      val client = flakyClient(_ => Some(new TimeoutException("Read timeout after 120000 ms")))(calls)

      val result = await(client.chatText(request, models))(using timeout)

      result.isInvalid mustBe true
      calls.get mustBe 3
    }
  }

  /** A client whose nth call (1-based) fails with `failure(n)` when defined, delegating to the sandbox TestClaudeClient
    * otherwise. `calls` observes how many HTTP attempts the retry layer made.
    */
  private def flakyClient(failure: Int => Option[Throwable])(calls: AtomicInteger): ClaudeClient = {
    val delegate = new TestClaudeClient()
    val flaky = new IClient {
      override def createMessage(
        body: com.bryzek.claude.models.ClaudeRequest,
        requestHeaders: Seq[(String, String)]
      ): scala.concurrent.Future[ClaudeResponse] = {
        failure(calls.incrementAndGet()) match {
          case Some(e) => scala.concurrent.Future.failed(e)
          case None => delegate.createMessage(body, requestHeaders)
        }
      }
    }
    ClaudeClient(flaky, ClaudeConfig("test-api-key"), NoopClaudeStore)
  }
}
