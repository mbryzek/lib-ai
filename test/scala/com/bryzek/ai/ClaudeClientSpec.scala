package com.bryzek.ai.claude

import com.bryzek.claude.client.IClient
import com.bryzek.claude.models.{
  ClaudeContentBlock,
  ClaudeContentType,
  ClaudeEffort,
  ClaudeModel,
  ClaudeResponse,
  ClaudeRole,
  ClaudeStopReason,
  ClaudeThinkingType,
  ClaudeTool,
  ClaudeToolChoiceType,
  ClaudeUsage
}
import com.bryzek.claude.models.json.*
import com.bryzek.claude.response.models.{RecommendationResponse, SingleInsightResponse}
import com.bryzek.claude.response.models.json.*
import helpers.FutureHelpers
import play.api.libs.json.Json
import org.apache.pekko.util.Timeout
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

import java.io.IOException
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicInteger
import javax.net.ssl.SSLException
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

    "reports stop_reason and output_tokens when the response has no text block" in {
      // Reproduces the synthesis failure: extended thinking consumed the whole output budget, so the
      // response is a lone thinking block with stop_reason=max_tokens and NO text -- the error must name
      // both so the failure is self-diagnosing rather than an opaque "no content".
      val client = fixedClient(
        ClaudeResponse(
          id = "msg_x",
          `type` = "message",
          role = ClaudeRole.Assistant,
          content = Seq(
            ClaudeContentBlock(ClaudeContentType.Thinking).copy(thinking = Some("reasoning that never finished"))
          ),
          model = ClaudeModel.ClaudeSonnet5,
          stopReason = ClaudeStopReason.MaxTokens,
          usage = ClaudeUsage(inputTokens = 5000, outputTokens = 30000)
        )
      )

      val result = await(client.chatText(request, models))(using timeout)

      result.isInvalid mustBe true
      val message = result.swap.toOption.get.toNonEmptyList.head.message
      message must include("stop_reason=max_tokens")
      message must include("output_tokens=30000")
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

    "classifies model-fallback error messages by status code, not embedded ids" in {
      ClaudeClient.isOverloadedError("POST /v1/messages failed with status 529 [req_529abc]") mustBe true
      ClaudeClient.isOverloadedError("POST /v1/messages failed with status 404 [req_529abc]") mustBe false
      ClaudeClient.isModelNotFoundError("POST /v1/messages failed with status 404 [req_404abc]") mustBe true
      ClaudeClient.isModelNotFoundError("POST /v1/messages failed with status 400 [req_404abc]") mustBe false
      ClaudeClient.isRefusalError("No text content in response (stop_reason=refusal, output_tokens=0)") mustBe true
      ClaudeClient.isRefusalError("No text content in response (stop_reason=max_tokens, output_tokens=9)") mustBe false
    }

    "falls back to the next model when the primary refuses the request" in {
      // Fable 5's safety classifiers return HTTP 200 with stop_reason=refusal and no text content;
      // the chain must degrade to the next model instead of surfacing the refusal as a terminal error.
      def response(stopReason: ClaudeStopReason, content: Seq[ClaudeContentBlock]): ClaudeResponse = {
        ClaudeResponse(
          id = "msg_x",
          `type` = "message",
          role = ClaudeRole.Assistant,
          content = content,
          model = ClaudeModel.ClaudeFable5,
          stopReason = stopReason,
          usage = ClaudeUsage(inputTokens = 10, outputTokens = 0)
        )
      }
      val perModel = new IClient {
        override def createMessage(
          body: com.bryzek.claude.models.ClaudeRequest,
          requestHeaders: Seq[(String, String)]
        ): scala.concurrent.Future[ClaudeResponse] = scala.concurrent.Future.successful {
          body.model match {
            case ClaudeModel.ClaudeFable5 => response(ClaudeStopReason.Refusal, Nil)
            case _ => response(ClaudeStopReason.EndTurn, Seq(ClaudeClient.textBlock("fallback answer")))
          }
        }
      }
      val client = ClaudeClient(perModel, ClaudeConfig("test-api-key"), NoopClaudeStore)

      val result = await(
        client.chatText(request, Seq(ClaudeModel.ClaudeFable5, ClaudeModel.ClaudeOpus48))
      )(using timeout)

      result.isValid mustBe true
      result.toOption.get.content mustBe "fallback answer"

      // A refusal on the LAST model in the chain still surfaces as a self-diagnosing error
      val exhausted = await(client.chatText(request, Seq(ClaudeModel.ClaudeFable5)))(using timeout)
      exhausted.isInvalid mustBe true
      exhausted.swap.toOption.get.toNonEmptyList.head.message must include("stop_reason=refusal")
    }

    "toClaudeRequest omits the thinking field entirely for Fable 5" in {
      // Fable 5 rejects any explicit thinking config except adaptive; omitting runs adaptive.
      AiRequest(messages = Nil).toClaudeRequest(ClaudeModel.ClaudeFable5).thinking mustBe None
      AiRequest(messages = Nil, thinking = ClaudeThinkingType.Disabled)
        .toClaudeRequest(ClaudeModel.ClaudeFable5)
        .thinking mustBe None
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

    "parses a recommendation_response with type_label" in {
      val json = """{"steps":[],"recommendations":[{"category":"Fitness","confidence":90}],"type_label":"gym"}"""
      val parsed = Json.parse(json).as[RecommendationResponse]
      parsed.recommendations.map(_.category) mustBe Seq("Fitness")
      parsed.typeLabel mustBe Some("gym")
    }

    "parses a recommendation_response with no type_label as None" in {
      val json = """{"steps":[],"recommendations":[{"category":"Fitness","confidence":90}]}"""
      val parsed = Json.parse(json).as[RecommendationResponse]
      parsed.typeLabel mustBe None
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
      loop.model mustBe ClaudeModel.ClaudeSonnet5
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
      result.toOption.get.model mustBe ClaudeModel.ClaudeSonnet5
    }
  }

  "withRetries" should {
    val models = Seq(ClaudeModel.ClaudeSonnet5)
    val request = AiRequest(
      messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, "Sending a test message"))
    )

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

    "fails fast on a permanent transport failure (TLS)" in {
      val calls = new AtomicInteger(0)
      val client = flakyClient(_ => Some(new SSLException("unable to find valid certification path")))(calls)

      val result = await(client.chatText(request, models))(using timeout)

      result.isInvalid mustBe true
      calls.get mustBe 1
    }
  }

  /** A client whose nth call (1-based) fails with `failure(n)` when defined, delegating to the sandbox TestClaudeClient
    * otherwise. `calls` observes how many HTTP attempts the retry layer made.
    */
  /** A client that always returns the given response verbatim -- used to exercise response-shape handling (e.g. a
    * no-text turn) that the sandbox TestClaudeClient never produces on its own.
    */
  private def fixedClient(response: ClaudeResponse): ClaudeClient = {
    val fixed = new IClient {
      override def createMessage(
        body: com.bryzek.claude.models.ClaudeRequest,
        requestHeaders: Seq[(String, String)]
      ): scala.concurrent.Future[ClaudeResponse] = scala.concurrent.Future.successful(response)
    }
    ClaudeClient(fixed, ClaudeConfig("test-api-key"), NoopClaudeStore)
  }

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
