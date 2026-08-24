package com.bryzek.ai.claude

import com.bryzek.claude.client.IClient
import com.bryzek.claude.models.{
  ClaudeContentBlock,
  ClaudeContentType,
  ClaudeEffort,
  ClaudeMediaType,
  ClaudeModel,
  ClaudeRequest,
  ClaudeResponse,
  ClaudeRole,
  ClaudeSourceType,
  ClaudeStopReason,
  ClaudeTaskBudgetType,
  ClaudeThinkingType,
  ClaudeToolChoiceType,
  ClaudeUsage
}
import com.bryzek.claude.models.json.*
import com.bryzek.claude.response.models.{CommentsResponse, RecommendationResponse, SingleInsightResponse}
import com.bryzek.claude.response.models.json.*
import generated.errors.ClaudeErrorResponseResponse
import generated.mock.MockWSResponse
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

    "refuses a response that stopped at the output ceiling with an answer half written" in {
      // The five log_review_analyze responses of ISS-4625: text came back, so every caller took it as the answer,
      // and it was the first 8192 tokens of one. `stop_reason` is the only thing that says so and it is gone by the
      // time the caller holds the string, so the refusal has to happen here.
      val truncated = fixedClient(
        ClaudeResponse(
          id = "msg_truncated",
          `type` = "message",
          role = ClaudeRole.Assistant,
          content = Seq(ClaudeClient.textBlock("""{"findings": [{"summary": "the worker retried""")),
          model = ClaudeModel.ClaudeSonnet5,
          stopReason = ClaudeStopReason.MaxTokens,
          usage = ClaudeUsage(inputTokens = 100000, outputTokens = 8192)
        )
      )

      // Effort is already at the floor, so this is the refusal itself rather than the step-down below.
      val result = await(truncated.chatText(request.copy(effort = Some(ClaudeEffort.Low)), models))(using timeout)

      result.isInvalid mustBe true
      val error = result.swap.toOption.get.toNonEmptyList.head
      error.message must include("truncated")
      error.message must include("stop_reason=max_tokens")
      error.message must include("output_tokens=8192")
      // The prefix is evidence for whoever reads the failure, and reachable only here -- but it is not an answer,
      // so it is in `raw` rather than in the content channel.
      error.raw mustBe Some("""{"findings": [{"summary": "the worker retried""")
    }

    "recovers a truncated answer by stepping effort down, exactly as it does an empty one" in {
      // A truncation IS budget exhaustion -- the ceiling was reached, thinking merely left enough room to start --
      // so it earns the same one-step recovery, and the caller sees one complete answer.
      val seen = efforts()
      val client = respondingClient(seen) { req =>
        if (req.outputConfig.flatMap(_.effort).contains(ClaudeEffort.Medium)) answered else truncatedAnswer
      }

      val result = await(
        client.chatText(request.copy(effort = Some(ClaudeEffort.High), maxTokens = 128000L), models)
      )(using timeout)

      result.isValid mustBe true
      result.toOption.get.content mustBe "the draft"
      seen.result() mustBe List(Some(ClaudeEffort.High), Some(ClaudeEffort.Medium))
    }

    "names the truncation on a structured response rather than the parse error it causes" in {
      // Half a JSON object is well-formed as far as it got, so the parser reports a missing brace -- the one thing
      // that is not wrong with it. The stop reason is checked before the text is parsed for exactly that reason.
      val truncated = fixedClient(
        ClaudeResponse(
          id = "msg_truncated_json",
          `type` = "message",
          role = ClaudeRole.Assistant,
          content = Seq(ClaudeClient.textBlock("""{"steps": [], "insight": "You are doing""")),
          model = ClaudeModel.ClaudeSonnet5,
          stopReason = ClaudeStopReason.MaxTokens,
          usage = ClaudeUsage(inputTokens = 500, outputTokens = 4096)
        )
      )

      val result = await(
        truncated.chatCompletion[SingleInsightResponse](
          request.copy(effort = Some(ClaudeEffort.Low)),
          ClaudeOutputFormats.SingleInsight,
          models
        )
      )(using timeout)

      result.isInvalid mustBe true
      val message = result.swap.toOption.get.toNonEmptyList.head.message
      message must include("truncated")
      message must include("stop_reason=max_tokens")
      message must not include ("Content is not valid JSON")
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
      val perModel = new MessageOnlyClient {
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
        client.chatText(request, Seq(ClaudeModel.ClaudeFable5, ClaudeModel.ClaudeOpus5))
      )(using timeout)

      result.isValid mustBe true
      result.toOption.get.content mustBe "fallback answer"

      // A refusal on the LAST model in the chain still surfaces as a self-diagnosing error
      val exhausted = await(client.chatText(request, Seq(ClaudeModel.ClaudeFable5)))(using timeout)
      exhausted.isInvalid mustBe true
      exhausted.swap.toOption.get.toNonEmptyList.head.message must include("stop_reason=refusal")
    }

    "classifies budget exhaustion by both of the shapes it takes, and nothing else" in {
      // Thinking took the whole ceiling, so there is no text block at all.
      ClaudeClient.isBudgetExhaustionError(
        "No text content in response (stop_reason=max_tokens, output_tokens=64000)"
      ) mustBe true
      // Thinking left just enough to start the draft, which was then cut off mid-string.
      ClaudeClient.isBudgetExhaustionError(
        "Response truncated at the output ceiling (stop_reason=max_tokens, output_tokens=8192); the answer is a prefix"
      ) mustBe true
      // The same truncation as an older run persisted it, before the stop reason was read at the point of parsing.
      ClaudeClient.isBudgetExhaustionError(
        "Content is not valid JSON: Unexpected end-of-input: was expecting closing quote for a string value"
      ) mustBe true
      // Bad JSON that did NOT run out of input is an ordinary content problem the model can correct.
      ClaudeClient.isBudgetExhaustionError(
        "Content is not valid JSON: Unrecognized token 'sections'"
      ) mustBe false
      // A refusal is a different failure with a different recovery (fall back to the next model).
      ClaudeClient.isBudgetExhaustionError(
        "No text content in response (stop_reason=refusal, output_tokens=0)"
      ) mustBe false
      // Half a signature is not a signature: a model asked to explain itself can echo either phrase.
      ClaudeClient.isBudgetExhaustionError("the headline section had No text content to summarize") mustBe false
      ClaudeClient.isBudgetExhaustionError("Unexpected end-of-input while reading baseline metrics") mustBe false
    }

    "toClaudeRequest sends a task budget below max_tokens so the model paces itself" in {
      // The budget is what the model SEES (a countdown it wraps up against); max_tokens is the ceiling it does not.
      // The gap between them is the headroom that turns a truncation into a graceful finish.
      val out = AiRequest(messages = Nil, maxTokens = 128000L).toClaudeRequest(ClaudeModel.ClaudeSonnet5)
      out.outputConfig.flatMap(_.taskBudget).map(_.total) mustBe Some(96000L)
      out.outputConfig.flatMap(_.taskBudget).map(_.`type`) mustBe Some(ClaudeTaskBudgetType.Tokens)
      out.maxTokens mustBe 128000L

      Seq(ClaudeModel.ClaudeOpus5, ClaudeModel.ClaudeFable5).foreach { model =>
        AiRequest(messages = Nil, maxTokens = 128000L)
          .toClaudeRequest(model)
          .outputConfig
          .flatMap(_.taskBudget)
          .map(_.total) mustBe Some(96000L)
      }
    }

    "toClaudeRequest omits the task budget where it would be rejected" in {
      // Haiku 4.5 does not take task budgets at all.
      AiRequest(messages = Nil, maxTokens = 128000L)
        .toClaudeRequest(ClaudeModel.ClaudeHaiku45)
        .outputConfig
        .flatMap(_.taskBudget) mustBe None

      // Below the API's 20,000 minimum there is no budget to send -- clamping up to the minimum would hand a small
      // request a budget larger than its own ceiling.
      AiRequest(messages = Nil, maxTokens = 8000L)
        .toClaudeRequest(ClaudeModel.ClaudeSonnet5)
        .outputConfig
        .flatMap(_.taskBudget) mustBe None

      // ... and a request with neither effort nor a budget sends no output_config at all.
      AiRequest(messages = Nil, maxTokens = 8000L)
        .toClaudeRequest(ClaudeModel.ClaudeSonnet5)
        .outputConfig mustBe None
    }

    "recovers from budget exhaustion in-call by stepping effort down once" in {
      // The whole point of ISS-1717: the caller sees ONE successful answer, not a failed stage that some later
      // attempt has to recover from after a full generation has already been paid for.
      val seen = efforts()
      val client = respondingClient(seen) { req =>
        if (req.outputConfig.flatMap(_.effort).contains(ClaudeEffort.Medium)) answered else budgetExhausted
      }

      val result = await(
        client.chatText(request.copy(effort = Some(ClaudeEffort.High), maxTokens = 128000L), models)
      )(using timeout)

      result.isValid mustBe true
      result.toOption.get.content mustBe "the draft"
      seen.result() mustBe List(Some(ClaudeEffort.High), Some(ClaudeEffort.Medium))
    }

    "steps effort down once and ONLY once, then surfaces the failure" in {
      // A second exhaustion is a real failure and must reach the caller: at that point it is the request's
      // maxTokens that is wrong, not its effort, and quietly walking the ladder to the floor only buys more
      // full-price generations that fail the same way.
      val seen = efforts()
      val client = respondingClient(seen)(_ => budgetExhausted)

      val result = await(
        client.chatText(request.copy(effort = Some(ClaudeEffort.High), maxTokens = 128000L), models)
      )(using timeout)

      result.isInvalid mustBe true
      result.swap.toOption.get.toNonEmptyList.head.message must include("stop_reason=max_tokens")
      seen.result() mustBe List(Some(ClaudeEffort.High), Some(ClaudeEffort.Medium))
    }

    "does not retry a budget exhaustion already at the bottom of the effort ladder" in {
      val seen = efforts()
      val client = respondingClient(seen)(_ => budgetExhausted)

      val result = await(
        client.chatText(request.copy(effort = Some(ClaudeEffort.Low), maxTokens = 128000L), models)
      )(using timeout)

      result.isInvalid mustBe true
      seen.result() mustBe List(Some(ClaudeEffort.Low))
    }

    "treats an absent effort as the API's own default of high when stepping down" in {
      val seen = efforts()
      val client = respondingClient(seen) { req =>
        if (req.outputConfig.flatMap(_.effort).contains(ClaudeEffort.Medium)) answered else budgetExhausted
      }

      val result = await(client.chatText(request.copy(maxTokens = 128000L), models))(using timeout)

      result.isValid mustBe true
      seen.result() mustBe List(None, Some(ClaudeEffort.Medium))
    }

    "toClaudeRequest omits the thinking field entirely for Fable 5" in {
      // Fable 5 rejects any explicit thinking config except adaptive; omitting runs adaptive.
      AiRequest(messages = Nil).toClaudeRequest(ClaudeModel.ClaudeFable5).thinking mustBe None
      AiRequest(messages = Nil, thinking = ClaudeThinkingType.Disabled)
        .toClaudeRequest(ClaudeModel.ClaudeFable5)
        .thinking mustBe None
    }

    "toClaudeRequest omits the thinking field entirely for Haiku 4.5" in {
      // Haiku 4.5 rejects adaptive thinking ("adaptive thinking is not supported on this model");
      // omitting the field runs without thinking.
      AiRequest(messages = Nil).toClaudeRequest(ClaudeModel.ClaudeHaiku45).thinking mustBe None
      AiRequest(messages = Nil, thinking = ClaudeThinkingType.Disabled)
        .toClaudeRequest(ClaudeModel.ClaudeHaiku45)
        .thinking mustBe None
    }

    "toClaudeRequest sets effort in output_config when provided" in {
      val out = AiRequest(messages = Nil, effort = Some(ClaudeEffort.High)).toClaudeRequest(ClaudeModel.ClaudeSonnet5)
      out.outputConfig.flatMap(_.effort) mustBe Some(ClaudeEffort.High)
      out.outputConfig.flatMap(_.format) mustBe None
    }

    "toClaudeRequest omits effort from output_config when none is provided" in {
      // output_config itself still ships -- it carries the task budget, which is derived from maxTokens rather than
      // asked for, so it is present on a request that named no effort at all.
      val out = AiRequest(messages = Nil).toClaudeRequest(ClaudeModel.ClaudeSonnet5)
      out.outputConfig.flatMap(_.effort) mustBe None
      out.outputConfig.flatMap(_.taskBudget).map(_.total) mustBe Some(22500L)
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

    "imageBlock builds an image content block with a base64 source" in {
      val block = ClaudeClient.imageBlock(ClaudeMediaType.ImagePng, "aGVsbG8=")
      block.`type` mustBe ClaudeContentType.Image
      block.source.get.`type` mustBe ClaudeSourceType.Base64
      block.source.get.mediaType mustBe ClaudeMediaType.ImagePng
      block.source.get.data mustBe "aGVsbG8="
      block.text mustBe None
    }

    "documentBlock builds a document content block defaulted to application/pdf" in {
      val block = ClaudeClient.documentBlock("JVBERi0xLjQK")
      block.`type` mustBe ClaudeContentType.Document
      block.source.get.`type` mustBe ClaudeSourceType.Base64
      block.source.get.mediaType mustBe ClaudeMediaType.ApplicationPdf
      block.source.get.data mustBe "JVBERi0xLjQK"
    }

    "serializes an image content block to the exact Anthropic wire shape" in {
      // https://docs.anthropic.com/en/api/messages -- vision content block. media_type must render as
      // "image/png" (the enum's `value` override), not the enum name ("image_png"), or the API rejects it.
      val block = ClaudeClient.imageBlock(ClaudeMediaType.ImagePng, "aGVsbG8=")
      Json.toJson(block) mustBe Json.parse(
        """{"type":"image","source":{"type":"base64","media_type":"image/png","data":"aGVsbG8="}}"""
      )
    }

    "serializes a document content block to the exact Anthropic wire shape" in {
      val block = ClaudeClient.documentBlock("JVBERi0xLjQK")
      Json.toJson(block) mustBe Json.parse(
        """{"type":"document","source":{"type":"base64","media_type":"application/pdf","data":"JVBERi0xLjQK"}}"""
      )
    }

    "round-trips every media type through JSON preserving the exact wire string" in {
      val cases = Seq(
        ClaudeMediaType.ImageJpeg -> "image/jpeg",
        ClaudeMediaType.ImagePng -> "image/png",
        ClaudeMediaType.ImageGif -> "image/gif",
        ClaudeMediaType.ImageWebp -> "image/webp",
        ClaudeMediaType.ApplicationPdf -> "application/pdf"
      )
      cases.foreach { case (mediaType, wire) =>
        val block = ClaudeClient.imageBlock(mediaType, "data")
        (Json.toJson(block) \ "source" \ "media_type").as[String] mustBe wire
        ClaudeMediaType(wire) mustBe mediaType
      }
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
    val tool = ClaudeTools.custom(
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

  "runToolLoopText" should {
    val models = Seq(ClaudeModel.ClaudeSonnet5)
    val tool = ClaudeTools.custom(
      name = "get_metric",
      description = "Return a metric",
      inputSchema = Json.obj("type" -> "object", "properties" -> Json.obj(), "additionalProperties" -> false)
    )
    val request = AiRequest(
      messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, "How full were the sessions?"))
    )

    "run the same tool loop and return the final turn's prose" in {
      var executed = 0
      val result = await(
        testClient.runToolLoopText(request, tools = Seq(tool), models = models, maxCalls = 25) { use =>
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
      // TestClaudeClient answers in prose exactly when no output-format header is sent, so this string IS the
      // assertion that the finalize turn declined to ask for a structured answer.
      loop.value mustBe "This is a test plain text response."
      loop.model mustBe ClaudeModel.ClaudeSonnet5
    }

    "finalize directly when the tool-call budget is zero" in {
      var executed = 0
      val result = await(
        testClient.runToolLoopText(request, tools = Seq(tool), models = models, maxCalls = 0) { _ =>
          executed += 1
          scala.concurrent.Future.successful(ClaudeToolOutput(content = "unused"))
        }
      )(using timeout)

      result.isValid mustBe true
      executed mustBe 0
      result.toOption.get.turns mustBe 0
      result.toOption.get.invocations mustBe empty
    }

    // The finalize turn is a whole round trip, and on a question the model answers without calling a tool it is half
    // the wall clock the caller waits through (ISS-3311). These pin the CALL COUNT, which is the claim -- the answer
    // text was already correct when it was being generated twice.
    "answer from the turn the model stopped on, paying for no finalize turn" in {
      val sent = scriptedClient(answered)
      val result = await(
        sent.client.runToolLoopText(request, tools = Seq(tool), models = models, maxCalls = 25) { _ =>
          sys.error("the model never asked for a tool")
        }
      )(using timeout)

      result.isValid mustBe true
      result.toOption.get.value mustBe "the draft"
      result.toOption.get.turns mustBe 0
      result.toOption.get.invocations mustBe empty
      result.toOption.get.model mustBe ClaudeModel.ClaudeSonnet5
      // One call, not two: the answer came back on the turn that stopped, so there was nothing left to ask for.
      sent.requests.size mustBe 1
    }

    "still finalize when the model stopped for a reason other than end_turn" in {
      // A truncated turn carries prose that reads as an answer and is not one, so it must not be taken as the
      // answer -- the finalize turn is what either recovers it or produces the error.
      val truncated = answered.copy(
        content = Seq(ClaudeClient.textBlock("Last month your sessions were ru")),
        stopReason = ClaudeStopReason.MaxTokens
      )
      val sent = scriptedClient(truncated, answered)
      val result = await(
        sent.client.runToolLoopText(request, tools = Seq(tool), models = models, maxCalls = 25) { _ =>
          sys.error("the model never asked for a tool")
        }
      )(using timeout)

      result.isValid mustBe true
      result.toOption.get.value mustBe "the draft"
      sent.requests.size mustBe 2
      sent.requests.last.toolChoice.map(_.`type`) mustBe Some(ClaudeToolChoiceType.None)
    }

    "still finalize when the turn that stopped carried no text at all" in {
      // `end_turn` with the whole budget spent on thinking. `noTextError` stays the guard for an empty completion,
      // so this must reach the finalize turn rather than returning an empty-string answer.
      val thinkingOnly = answered.copy(
        content = Seq(ClaudeContentBlock(ClaudeContentType.Thinking).copy(thinking = Some("still reasoning")))
      )
      val sent = scriptedClient(thinkingOnly, answered)
      val result = await(
        sent.client.runToolLoopText(request, tools = Seq(tool), models = models, maxCalls = 25) { _ =>
          sys.error("the model never asked for a tool")
        }
      )(using timeout)

      result.isValid mustBe true
      result.toOption.get.value mustBe "the draft"
      sent.requests.size mustBe 2
    }

    "surface a truncated finalize turn as an error rather than half an answer" in {
      // The loop already declines to take a truncated turn as the answer; the finalize turn it runs instead can hit
      // the same ceiling, and the prose it comes back with reads exactly like a complete reply.
      val truncated = answered.copy(
        content = Seq(ClaudeClient.textBlock("Last month your sessions were ru")),
        stopReason = ClaudeStopReason.MaxTokens
      )
      val sent = scriptedClient(truncated, truncated)
      val result = await(
        sent.client.runToolLoopText(
          request.copy(effort = Some(ClaudeEffort.Low)),
          tools = Seq(tool),
          models = models,
          maxCalls = 25
        ) { _ =>
          sys.error("the model never asked for a tool")
        }
      )(using timeout)

      result.isInvalid mustBe true
      result.swap.toOption.get.toNonEmptyList.head.message must include("truncated")
    }

    "surface an empty completion as an error rather than an empty answer" in {
      val thinkingOnly = answered.copy(
        content = Seq(ClaudeContentBlock(ClaudeContentType.Thinking).copy(thinking = Some("still reasoning")))
      )
      val sent = scriptedClient(thinkingOnly, thinkingOnly)
      val result = await(
        sent.client.runToolLoopText(request, tools = Seq(tool), models = models, maxCalls = 25) { _ =>
          sys.error("the model never asked for a tool")
        }
      )(using timeout)

      result.isInvalid mustBe true
      result.swap.toOption.get.toNonEmptyList.head.message must include("No text content")
    }

    "runToolLoop keeps its finalize turn: prose is not the parseable object its callers need" in {
      val prose = answered.copy(content = Seq(ClaudeClient.textBlock("Here is what I found, in prose.")))
      val structured = answered.copy(
        content = Seq(
          ClaudeClient.textBlock(Json.stringify(Json.obj("steps" -> Json.arr(), "insight" -> "You are doing amazing")))
        )
      )
      val sent = scriptedClient(prose, structured)
      val result = await(
        sent.client.runToolLoop[SingleInsightResponse](
          request,
          tools = Seq(tool),
          models = models,
          maxCalls = 25,
          finalFormat = ClaudeOutputFormats.SingleInsight
        ) { _ =>
          sys.error("the model never asked for a tool")
        }
      )(using timeout)

      result.isValid mustBe true
      result.toOption.get.value.insight mustBe "You are doing amazing"
      sent.requests.size mustBe 2
    }
  }

  "tool-loop prompt caching" should {
    val ephemeral = Some(com.bryzek.claude.models.ClaudeCacheType.Ephemeral)
    val tool = ClaudeTools.custom(
      name = "get_metric",
      description = "Return a metric",
      inputSchema = Json.obj("type" -> "object", "properties" -> Json.obj(), "additionalProperties" -> false)
    )

    def markers(msgs: Seq[com.bryzek.claude.models.ClaudeMessage]): Seq[Int] =
      msgs.zipWithIndex.collect { case (m, i) if m.content.exists(_.cacheControl.isDefined) => i }

    "cacheStaticPrefix breaks on the last system block, covering tools and system" in {
      val base = AiRequest(messages = Nil, system = Some("investigator rules"))
        .toClaudeRequest(ClaudeModel.ClaudeSonnet5)
        .copy(tools = Some(Seq(tool)))
      val out = ClaudeClient.cacheStaticPrefix(base)
      out.system.get.last.cacheControl.map(_.`type`) mustBe ephemeral
      // tools render before system, so the system breakpoint already covers them -- a second one would waste budget
      out.tools.get.flatMap(_.cacheControl) mustBe Nil
    }

    "cacheStaticPrefix falls back to the last tool when there is no system prompt" in {
      val base = AiRequest(messages = Nil)
        .toClaudeRequest(ClaudeModel.ClaudeSonnet5)
        .copy(tools = Some(Seq(tool, tool.copy(name = "other"))))
      val out = ClaudeClient.cacheStaticPrefix(base)
      out.system mustBe None
      out.tools.get.init.flatMap(_.cacheControl) mustBe Nil
      out.tools.get.last.cacheControl.map(_.`type`) mustBe ephemeral
    }

    "cacheConversation rolls breakpoints across turns and never exceeds the budget" in {
      def transcript(turns: Int): Seq[com.bryzek.claude.models.ClaudeMessage] =
        ClaudeClient.makeClaudeMessage(ClaudeRole.User, "start") +: (1 to turns).flatMap { n =>
          Seq(
            ClaudeClient.makeClaudeMessage(ClaudeRole.Assistant, s"calling tool $n"),
            ClaudeClient.makeClaudeMessage(ClaudeRole.User, s"result $n")
          )
        }

      markers(ClaudeClient.cacheConversation(transcript(0))) mustBe Seq(0)
      // one turn back = two messages back, so both breakpoints land on user (tool-result) turns
      markers(ClaudeClient.cacheConversation(transcript(1))) mustBe Seq(0, 2)
      markers(ClaudeClient.cacheConversation(transcript(3))) mustBe Seq(4, 6)

      (0 to 6).foreach { turns =>
        val out = ClaudeClient.cacheConversation(transcript(turns))
        withClue(s"turns=$turns: ") {
          markers(out).size must be <= ClaudeClient.ConversationBreakpoints
        }
      }
    }

    "cacheConversation clears stale markers rather than accumulating them" in {
      val stale = Seq("a", "b", "c", "d", "e").map { t =>
        val m = ClaudeClient.makeClaudeMessage(ClaudeRole.User, t)
        m.copy(content =
          m.content.map(_.copy(cacheControl = Some(com.bryzek.claude.models.ClaudeCacheControl(ttl = None))))
        )
      }
      markers(stale) mustBe Seq(0, 1, 2, 3, 4)
      markers(ClaudeClient.cacheConversation(stale)) mustBe Seq(2, 4)
    }

    // The TTL only ever appears on the wire when a caller asked for the long one. That asymmetry is the point: a
    // 1h WRITE costs 2x input against 1.25x at 5m, so a request that did not opt in must be byte-identical to what
    // it sent before the field existed, or adopting it anywhere would quietly reprice everything.
    "cacheStaticPrefix omits ttl at the default and sends 1h when asked" in {
      def prefixTtl(ttl: ClaudeCacheTtl): Option[String] = {
        val base = AiRequest(messages = Nil, system = Some("investigator rules"))
          .toClaudeRequest(ClaudeModel.ClaudeSonnet5)
          .copy(tools = Some(Seq(tool)))
        ClaudeClient.cacheStaticPrefix(base, ttl).system.get.last.cacheControl.flatMap(_.ttl)
      }
      prefixTtl(ClaudeCacheTtl.FiveMinutes) mustBe None
      prefixTtl(ClaudeCacheTtl.OneHour) mustBe Some("1h")
    }

    "cacheConversation carries the ttl onto every rolling breakpoint" in {
      val transcript = Seq("a", "b", "c").map(ClaudeClient.makeClaudeMessage(ClaudeRole.User, _))
      def ttls(ttl: ClaudeCacheTtl): Seq[Option[String]] =
        ClaudeClient.cacheConversation(transcript, ttl).flatMap(_.content).flatMap(_.cacheControl).map(_.ttl)

      ttls(ClaudeCacheTtl.FiveMinutes) mustBe Seq(None, None)
      ttls(ClaudeCacheTtl.OneHour) mustBe Seq(Some("1h"), Some("1h"))
    }

    "toClaudeRequest carries cacheTtl onto the system and last-message breakpoints" in {
      val r = AiRequest(
        messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, "hello")),
        system = Some("ctx"),
        cacheSystem = true,
        cacheLastMessage = true,
        cacheTtl = ClaudeCacheTtl.OneHour
      ).toClaudeRequest(ClaudeModel.ClaudeSonnet5)

      r.system.get.last.cacheControl.flatMap(_.ttl) mustBe Some("1h")
      r.messages.last.content.last.cacheControl.flatMap(_.ttl) mustBe Some("1h")
    }

    "AiRequest defaults to the five-minute ttl, so an untouched caller's bytes do not change" in {
      val r = AiRequest(
        messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, "hello")),
        system = Some("ctx"),
        cacheSystem = true
      ).toClaudeRequest(ClaudeModel.ClaudeSonnet5)

      AiRequest(messages = Nil).cacheTtl mustBe ClaudeCacheTtl.FiveMinutes
      r.system.get.last.cacheControl.flatMap(_.ttl) mustBe None
    }

    "cacheConversation tags the last content block of a multi-block tool-result turn" in {
      val results = com.bryzek.claude.models.ClaudeMessage(
        role = ClaudeRole.User,
        content = Seq(
          ClaudeClient.toolResultBlock("t1", ClaudeToolOutput("one")),
          ClaudeClient.toolResultBlock("t2", ClaudeToolOutput("two"))
        )
      )
      val out = ClaudeClient.cacheConversation(Seq(results))
      out.last.content.init.flatMap(_.cacheControl) mustBe Nil
      out.last.content.last.cacheControl.map(_.`type`) mustBe ephemeral
    }
  }

  /** `pause_turn` is what a server tool (web search / web fetch) produces when the API's own sampling loop hits its
    * iteration limit with the turn unfinished. It arrives as an ordinary HTTP 200 carrying whatever prose the model had
    * written so far, so a client that does not resume returns a truncated answer that reads like a complete one — the
    * failure these tests exist to pin.
    */
  "pause_turn" should {
    val models = Seq(ClaudeModel.ClaudeSonnet5)
    val request = AiRequest(
      messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, "What shipped this week? Search the web."))
    )

    /** A paused turn: a server-tool call plus the partial prose written before the loop ran out of iterations. The
      * trailing `server_tool_use` block is what the API recognizes on the resend to continue the turn.
      */
    def paused(text: String): ClaudeResponse = ClaudeResponse(
      id = "msg_paused",
      `type` = "message",
      role = ClaudeRole.Assistant,
      content = Seq(
        ClaudeClient.textBlock(text),
        ClaudeContentBlock(ClaudeContentType.ServerToolUse)
          .copy(id = Some("srvtoolu_1"), name = Some("web_search"), input = Some(Json.obj("query" -> "what shipped")))
      ),
      model = ClaudeModel.ClaudeSonnet5,
      stopReason = ClaudeStopReason.PauseTurn,
      usage = ClaudeUsage(inputTokens = 5000, outputTokens = 2000)
    )

    "resumes a paused turn and returns the finished answer" in {
      val sent = scriptedClient(paused("I searched and found"), answered)
      val result = await(sent.client.chatText(request, models))(using timeout)

      result.isValid mustBe true
      // The answer is the RESUMED turn's, not the partial prose the paused turn carried.
      result.toOption.get.content mustBe "the draft"
      sent.requests.size mustBe 2
    }

    "resends the paused assistant content verbatim, with no extra user turn" in {
      val pausedTurn = paused("I searched and found")
      val sent = scriptedClient(pausedTurn, answered)
      await(sent.client.chatText(request, models))(using timeout).isValid mustBe true

      val resumed = sent.requests(1)
      // Exactly one message appended: the paused assistant turn. A "continue" user turn would both break the
      // API's recognition of the trailing server_tool_use block and inject an instruction the caller never wrote.
      resumed.messages.size mustBe sent.requests.head.messages.size + 1
      resumed.messages.last.role mustBe ClaudeRole.Assistant
      // Verbatim: the server_tool_use block (and, on a real turn, each result's encrypted_content) is how the API
      // restores what it already fetched instead of paying to fetch it again.
      resumed.messages.last.content mustBe pausedTurn.content
    }

    "resumes repeatedly while the turn keeps pausing" in {
      val sent = scriptedClient(paused("first"), paused("second"), paused("third"), answered)
      val result = await(sent.client.chatText(request, models))(using timeout)

      result.isValid mustBe true
      result.toOption.get.content mustBe "the draft"
      sent.requests.size mustBe 4
    }

    "fails loudly rather than returning a truncated answer when the turn never converges" in {
      // Every turn pauses. The partial prose would parse as a perfectly good answer, which is exactly why
      // returning it is the wrong outcome -- a wrong answer presented as a right one.
      val sent = scriptedClient(Seq.fill(ClaudeClient.MaxPauseTurnResumes + 5)(paused("partial thought"))*)
      val result = await(sent.client.chatText(request, models))(using timeout)

      result.isInvalid mustBe true
      val message = result.swap.toOption.get.toNonEmptyList.head.message
      message must include("pause_turn")
      // Bounded: the initial request plus exactly the capped number of resumes, not an unbounded spend.
      sent.requests.size mustBe ClaudeClient.MaxPauseTurnResumes + 1
    }

    "leaves a turn that never pauses untouched" in {
      val sent = scriptedClient(answered)
      val result = await(sent.client.chatText(request, models))(using timeout)

      result.isValid mustBe true
      sent.requests.size mustBe 1
    }
  }

  "withRetries" should {
    val models = Seq(ClaudeModel.ClaudeSonnet5)
    val request = AiRequest(
      messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, "Sending a test message")),
      maxTokens = 4096L
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

    "give a large request the same retries as a small one" in {
      // ISS-1229 briefly traded a large request's retries away, because a non-streaming attempt needed an
      // ever-larger slice of a fixed wall-clock envelope to have any chance of finishing. Streaming inverts
      // that (ISS-1231): an attempt that is going to fail fails on the idle timeout in minutes, so size no
      // longer buys anything by giving up its retries.
      val calls = new AtomicInteger(0)
      val client = flakyClient(_ => Some(new TimeoutException("Request timeout after 120000 ms")))(calls)
      val large = request.copy(maxTokens = 64000L)

      val result = await(client.chatText(large, models))(using timeout)

      result.isInvalid mustBe true
      calls.get mustBe 3
    }
  }

  /** Which id a failure carries, and whether it reached the API at all.
    *
    * ISS-2522 was triaged as a spec making a live API call it was built to make none of, at cost, on whichever runner
    * picked the build up. It was a TestClaudeClient failure that never opened a socket. What made the two
    * indistinguishable was the label: `[Request ID: req-<uuid>]`, minted here, attached identically either way, and the
    * same shape Anthropic's own ids take (ISS-2542).
    */
  "error labelling" should {
    val models = Seq(ClaudeModel.ClaudeSonnet5)
    val request = AiRequest(messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, "Sending a test message")))

    "say when a failure never left the process" in {
      // The ISS-2522 failure verbatim: an output format absent from ClaudeOutputFormats.all, which TestClaudeClient
      // rejects before anything is sent.
      val unregistered = ClaudeOutputFormats.create("not_a_registered_format", Json.obj(), Nil)

      val result = await(
        testClient.chatCompletion[CommentsResponse](request, unregistered, models)
      )(using timeout)

      result.isInvalid mustBe true
      val message = result.swap.toOption.get.toNonEmptyList.head.message
      message must include("Could not identify json schema")
      message must include("simulated client, no API call")
      message must include(s"[lib-ai request ${ClaudeErrorLabels.LocalIdPrefix}-")
      // The two halves of the resemblance that misdirected the triage.
      message must not include ("Request ID")
      message must not include ("[req-")
    }

    "keep the provider's id and this library's own apart on a real API failure" in {
      // MessageOnlyClient doubles stand in for the transport rather than for the API being absent -- they exist to
      // reproduce what Anthropic does -- so they are deliberately not marked simulated, and this is what a genuine
      // provider failure reads like.
      val result = await(
        erroringClient(400, "bad model", Map("request-id" -> Seq("req_011CSprovider"))).chatText(request, models)
      )(using timeout)

      result.isInvalid mustBe true
      val message = result.swap.toOption.get.toNonEmptyList.head.message
      message must include("bad model")
      // Theirs: the only id in the message their logs can be searched by.
      message must include("[anthropic request-id: req_011CSprovider]")
      // Ours: the only id the claude.requests audit row can be searched by. It used to be dropped entirely on this
      // path, which left a real provider failure as the one error joinable to nothing.
      message must include(s"[lib-ai request ${ClaudeErrorLabels.LocalIdPrefix}-")
      message must not include ("simulated client")
    }

    "say nothing about a provider id when the response carried none" in {
      val result = await(erroringClient(400, "bad model", Map.empty).chatText(request, models))(using timeout)

      val message = result.swap.toOption.get.toNonEmptyList.head.message
      message must include("bad model")
      message must not include ("anthropic request-id")
    }

    "read the provider header whatever case it arrives in" in {
      // HTTP header names are case-insensitive and this one is quoted lowercase by Anthropic's docs and uppercased
      // by some proxies. Missing it would silently drop the only id that leads anywhere.
      ClaudeErrorLabels.providerId(Map("request-id" -> Seq("req_1"))) mustBe Some("req_1")
      ClaudeErrorLabels.providerId(Map("Request-Id" -> Seq("req_1"))) mustBe Some("req_1")
      ClaudeErrorLabels.providerId(Map("REQUEST-ID" -> Seq(" req_1 "))) mustBe Some("req_1")
      ClaudeErrorLabels.providerId(Map("retry-after" -> Seq("42"))) mustBe None
      ClaudeErrorLabels.providerId(Map("request-id" -> Seq("  "))) mustBe None
      ClaudeErrorLabels.providerSuffix(None) mustBe ""
    }
  }

  /** A client that fails the way the generated (non-streaming) client reports a non-2xx: the raw WSResponse wrapped in
    * a ClaudeErrorResponseResponse, response headers included.
    */
  private def erroringClient(status: Int, message: String, headers: Map[String, Seq[String]]): ClaudeClient = {
    val response = MockWSResponse(
      uri = MockWSResponse.DefaultURI,
      status = status,
      body = Json.stringify(Json.obj("error" -> Json.obj("message" -> message))),
      headers = headers
    )
    val failing = new MessageOnlyClient {
      override def createMessage(
        body: com.bryzek.claude.models.ClaudeRequest,
        requestHeaders: Seq[(String, String)]
      ): scala.concurrent.Future[ClaudeResponse] =
        scala.concurrent.Future.failed(ClaudeErrorResponseResponse(response))
    }
    ClaudeClient(failing, ClaudeConfig("test-api-key"), NoopClaudeStore)
  }

  /** A client whose nth call (1-based) fails with `failure(n)` when defined, delegating to the sandbox TestClaudeClient
    * otherwise. `calls` observes how many HTTP attempts the retry layer made.
    */
  /** A client that always returns the given response verbatim -- used to exercise response-shape handling (e.g. a
    * no-text turn) that the sandbox TestClaudeClient never produces on its own.
    */
  /** A turn that spent the whole output budget on thinking: `stop_reason=max_tokens` and no text block. */
  private val budgetExhausted: ClaudeResponse = ClaudeResponse(
    id = "msg_exhausted",
    `type` = "message",
    role = ClaudeRole.Assistant,
    content = Seq(ClaudeContentBlock(ClaudeContentType.Thinking).copy(thinking = Some("still reasoning"))),
    model = ClaudeModel.ClaudeSonnet5,
    stopReason = ClaudeStopReason.MaxTokens,
    usage = ClaudeUsage(inputTokens = 5000, outputTokens = 128000)
  )

  private val answered: ClaudeResponse = budgetExhausted.copy(
    id = "msg_answered",
    content = Seq(ClaudeClient.textBlock("the draft")),
    stopReason = ClaudeStopReason.EndTurn,
    usage = ClaudeUsage(inputTokens = 5000, outputTokens = 4374)
  )

  /** A turn that spent the ceiling with the answer already under way: `stop_reason=max_tokens` and a text block that
    * is the first sentence of one.
    */
  private val truncatedAnswer: ClaudeResponse = budgetExhausted.copy(
    id = "msg_truncated",
    content = Seq(ClaudeClient.textBlock("the dra")),
    usage = ClaudeUsage(inputTokens = 5000, outputTokens = 128000)
  )

  /** Records the `effort` of each request a [[respondingClient]] receives, in order. */
  private def efforts(): scala.collection.mutable.Builder[Option[ClaudeEffort], List[Option[ClaudeEffort]]] =
    List.newBuilder[Option[ClaudeEffort]]

  /** A client that answers each request from `respond`, recording the effort it was asked for. Synchronized because the
    * retry/fallback layers run their attempts on the execution context, not the calling thread.
    */
  /** An [[IClient]] that only answers messages.
    *
    * The Message Batches operations live on the same generated client but have nothing to do with these tests, so they
    * are stubbed here to throw. Loudly rather than returning a plausible empty batch: a double that quietly answers a
    * call the test did not intend to make is how a test passes while measuring nothing.
    */
  private abstract class MessageOnlyClient extends IClient {
    private def unused[T](operation: String): scala.concurrent.Future[T] =
      scala.concurrent.Future.failed(new UnsupportedOperationException(s"$operation is not part of this test double"))

    override def createClaudeBatch(
      body: com.bryzek.claude.models.ClaudeBatchForm,
      requestHeaders: Seq[(String, String)]
    ): scala.concurrent.Future[com.bryzek.claude.models.ClaudeBatch] = unused("createClaudeBatch")

    override def getClaudeBatchById(
      id: String,
      requestHeaders: Seq[(String, String)]
    ): scala.concurrent.Future[com.bryzek.claude.models.ClaudeBatch] = unused("getClaudeBatchById")

    override def cancelClaudeBatchById(
      id: String,
      requestHeaders: Seq[(String, String)]
    ): scala.concurrent.Future[com.bryzek.claude.models.ClaudeBatch] = unused("cancelClaudeBatchById")
  }

  private def respondingClient(
    seen: scala.collection.mutable.Builder[Option[ClaudeEffort], List[Option[ClaudeEffort]]]
  )(respond: com.bryzek.claude.models.ClaudeRequest => ClaudeResponse): ClaudeClient = {
    val recording = new MessageOnlyClient {
      override def createMessage(
        body: com.bryzek.claude.models.ClaudeRequest,
        requestHeaders: Seq[(String, String)]
      ): scala.concurrent.Future[ClaudeResponse] = {
        seen.synchronized(seen += body.outputConfig.flatMap(_.effort))
        scala.concurrent.Future.successful(respond(body))
      }
    }
    ClaudeClient(recording, ClaudeConfig("test-api-key"), NoopClaudeStore)
  }

  /** A [[scriptedClient]] and the requests it received, in order — so a test can assert on what was RESENT, which for a
    * resume is the whole behavior under test.
    */
  private case class Scripted(client: ClaudeClient, recorded: scala.collection.mutable.Buffer[ClaudeRequest]) {
    def requests: Seq[ClaudeRequest] = recorded.synchronized(recorded.toList)
  }

  /** A client that answers each successive call with the next scripted response, then repeats the last one. Repeating
    * rather than failing keeps a test that under-counts its script from dying with an index error that says nothing
    * about the behavior; the request count assertions are what pin the call count.
    */
  private def scriptedClient(responses: ClaudeResponse*): Scripted = {
    val recorded = scala.collection.mutable.Buffer.empty[ClaudeRequest]
    val scripted = new MessageOnlyClient {
      override def createMessage(
        body: com.bryzek.claude.models.ClaudeRequest,
        requestHeaders: Seq[(String, String)]
      ): scala.concurrent.Future[ClaudeResponse] = {
        val index = recorded.synchronized { recorded += body; recorded.size - 1 }
        scala.concurrent.Future.successful(responses(math.min(index, responses.size - 1)))
      }
    }
    Scripted(ClaudeClient(scripted, ClaudeConfig("test-api-key"), NoopClaudeStore), recorded)
  }

  private def fixedClient(response: ClaudeResponse): ClaudeClient = {
    val fixed = new MessageOnlyClient {
      override def createMessage(
        body: com.bryzek.claude.models.ClaudeRequest,
        requestHeaders: Seq[(String, String)]
      ): scala.concurrent.Future[ClaudeResponse] = scala.concurrent.Future.successful(response)
    }
    ClaudeClient(fixed, ClaudeConfig("test-api-key"), NoopClaudeStore)
  }

  private def flakyClient(failure: Int => Option[Throwable])(calls: AtomicInteger): ClaudeClient = {
    val delegate = new TestClaudeClient()
    val flaky = new MessageOnlyClient {
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
