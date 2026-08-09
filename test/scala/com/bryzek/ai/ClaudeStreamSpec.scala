package com.bryzek.ai.claude

import com.bryzek.claude.models.{ClaudeContentType, ClaudeModel, ClaudeRole, ClaudeStopReason}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** The event sequences here are captured verbatim from `POST /v1/messages` with `"stream": true` against
  * api.anthropic.com on 2026-08-09 (claude-sonnet-5, adaptive thinking), with only the long text and signature bodies
  * shortened. Hand-written SSE would only prove the accumulator agrees with itself.
  */
class ClaudeStreamSpec extends AnyWordSpec with Matchers {

  private def accumulate(lines: String*): ClaudeStreamOutcome =
    lines.foldLeft(ClaudeStream.Empty)((acc, l) => acc.line(l)).outcome

  private def completed(lines: String*): com.bryzek.claude.models.ClaudeResponse =
    accumulate(lines*) match {
      case ClaudeStreamOutcome.Completed(r) => r
      case ClaudeStreamOutcome.Failed(status, message) =>
        fail(s"expected a completed response, got Failed($status, $message)")
    }

  private def failure(lines: String*): ClaudeStreamOutcome.Failed =
    accumulate(lines*) match {
      case f: ClaudeStreamOutcome.Failed => f
      case ClaudeStreamOutcome.Completed(r) => fail(s"expected a failure, got a completed response: $r")
    }

  private val MessageStart =
    """data: {"type":"message_start","message":{"model":"claude-sonnet-5","id":"msg_011CdrTciMHPAmqaFojJXxfR",""" +
      """"type":"message","role":"assistant","content":[],"stop_reason":null,"stop_sequence":null,""" +
      """"usage":{"input_tokens":18,"cache_creation_input_tokens":0,"cache_read_input_tokens":0,"output_tokens":2}}}"""

  private val MessageDelta =
    """data: {"type":"message_delta","delta":{"stop_reason":"end_turn","stop_sequence":null},""" +
      """"usage":{"input_tokens":18,"cache_read_input_tokens":7,"output_tokens":449}}"""

  private val MessageStop = """data: {"type":"message_stop"}"""

  private val TextStart = """data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}"""
  private val TextStop = """data: {"type":"content_block_stop","index":0}"""

  private def textDelta(text: String, index: Int = 0): String =
    s"""data: {"type":"content_block_delta","index":$index,"delta":{"type":"text_delta","text":"$text"}}"""

  "a plain text turn" should {

    val response = completed(
      "event: message_start",
      MessageStart,
      "",
      TextStart,
      """data: {"type": "ping"}""",
      textDelta("12,"),
      textDelta("231"),
      TextStop,
      MessageDelta,
      MessageStop
    )

    "concatenate the deltas into one text block" in {
      response.content.map(_.`type`) mustBe Seq(ClaudeContentType.Text)
      response.content.head.text mustBe Some("12,231")
    }

    "carry the identity fields message_start supplied" in {
      response.id mustBe "msg_011CdrTciMHPAmqaFojJXxfR"
      response.`type` mustBe "message"
      response.role mustBe ClaudeRole.Assistant
      response.model mustBe ClaudeModel.ClaudeSonnet5
    }

    "take stop_reason from message_delta, where it is first reported" in {
      response.stopReason mustBe ClaudeStopReason.EndTurn
      // message_start reports it as null, which the non-optional ClaudeResponse reader cannot accept
      response.stopSequence mustBe None
    }

    "report message_delta's final usage, not message_start's opening counts" in {
      response.usage.outputTokens mustBe 449
      response.usage.inputTokens mustBe 18
      // Carried through from message_start: message_delta does not repeat every field.
      response.usage.cacheCreationInputTokens mustBe Some(0)
      response.usage.cacheReadInputTokens mustBe Some(7)
    }
  }

  "a thinking turn" should {

    "accumulate thinking text and its signature onto the thinking block" in {
      val response = completed(
        MessageStart,
        """data: {"type":"content_block_start","index":0,"content_block":{"type":"thinking","thinking":"","signature":""}}""",
        """data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"27 x 453"}}""",
        """data: {"type":"content_block_delta","index":0,"delta":{"type":"signature_delta","signature":"EpYECokB"}}""",
        """data: {"type":"content_block_delta","index":0,"delta":{"type":"signature_delta","signature":"CBAYAipA"}}""",
        """data: {"type":"content_block_stop","index":0}""",
        """data: {"type":"content_block_start","index":1,"content_block":{"type":"text","text":""}}""",
        textDelta("12,231", index = 1),
        """data: {"type":"content_block_stop","index":1}""",
        MessageDelta,
        MessageStop
      )

      response.content.map(_.`type`) mustBe Seq(ClaudeContentType.Thinking, ClaudeContentType.Text)
      response.content.head.thinking mustBe Some("27 x 453")
      response.content.head.signature mustBe Some("EpYECokBCBAYAipA")
      response.content.last.text mustBe Some("12,231")
    }
  }

  "a tool_use turn" should {

    val response = completed(
      MessageStart,
      """data: {"type":"content_block_start","index":0,"content_block":{"type":"tool_use",""" +
        """"id":"toolu_01ESWaKbmdw57KZ8qWZ1XyZa","name":"get_weather","input":{},"caller":{"type":"direct"}}}""",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":""}}""",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"{\"c"}}""",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"ity\": \"Pa"}}""",
      """data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"ris\"}"}}""",
      """data: {"type":"content_block_stop","index":0}""",
      """data: {"type":"message_delta","delta":{"stop_reason":"tool_use","stop_sequence":null},""" +
        """"usage":{"input_tokens":445,"output_tokens":50}}""",
      MessageStop
    )

    "reassemble the partial_json fragments into the tool input" in {
      val use = ClaudeClient.toolUses(response)
      use.map(_.name) mustBe Seq("get_weather")
      use.map(_.id) mustBe Seq("toolu_01ESWaKbmdw57KZ8qWZ1XyZa")
      (use.head.input \ "city").as[String] mustBe "Paris"
      response.stopReason mustBe ClaudeStopReason.ToolUse
    }

    "keep the empty input a no-argument tool call starts with" in {
      val noArgs = completed(
        MessageStart,
        """data: {"type":"content_block_start","index":0,"content_block":{"type":"tool_use",""" +
          """"id":"toolu_1","name":"list_courts","input":{}}}""",
        """data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":""}}""",
        """data: {"type":"content_block_stop","index":0}""",
        MessageDelta,
        MessageStop
      )
      ClaudeClient.toolUses(noArgs).map(_.input.keys.size) mustBe Seq(0)
    }

    "keep two parallel tool calls' arguments apart" in {
      // The model asks for several tools in one turn and their deltas interleave by index. Getting this wrong
      // would hand one tool another's arguments, which no downstream check would catch.
      val parallel = completed(
        MessageStart,
        """data: {"type":"content_block_start","index":0,"content_block":{"type":"tool_use",""" +
          """"id":"toolu_a","name":"get_weather","input":{}}}""",
        """data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"{\"city\":"}}""",
        """data: {"type":"content_block_start","index":1,"content_block":{"type":"tool_use",""" +
          """"id":"toolu_b","name":"list_courts","input":{}}}""",
        """data: {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"{\"club\":"}}""",
        """data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":" \"Paris\"}"}}""",
        """data: {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":" \"picklejar\"}"}}""",
        """data: {"type":"content_block_stop","index":0}""",
        """data: {"type":"content_block_stop","index":1}""",
        """data: {"type":"message_delta","delta":{"stop_reason":"tool_use"},"usage":{"output_tokens":80}}""",
        MessageStop
      )

      val uses = ClaudeClient.toolUses(parallel)
      uses.map(_.name) mustBe Seq("get_weather", "list_courts")
      (uses.head.input \ "city").as[String] mustBe "Paris"
      (uses.last.input \ "club").as[String] mustBe "picklejar"
    }

    "refuse a tool call whose arguments were cut off rather than invent an input object" in {
      // stop_reason=max_tokens landing inside a tool call. Returning `{}` here would hand the caller's
      // tool an empty argument object and let it act on it.
      val f = failure(
        MessageStart,
        """data: {"type":"content_block_start","index":0,"content_block":{"type":"tool_use",""" +
          """"id":"toolu_1","name":"refund","input":{}}}""",
        """data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"{\"amount\": 40"}}""",
        """data: {"type":"message_delta","delta":{"stop_reason":"max_tokens"},"usage":{"output_tokens":8}}""",
        MessageStop
      )
      f.status mustBe None
      f.message must include("tool_use input is not valid JSON")
    }
  }

  "block ordering" should {

    "follow the event index rather than arrival order" in {
      val response = completed(
        MessageStart,
        """data: {"type":"content_block_start","index":1,"content_block":{"type":"text","text":"second"}}""",
        """data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":"first"}}""",
        MessageDelta,
        MessageStop
      )
      response.content.flatMap(_.text) mustBe Seq("first", "second")
    }
  }

  "framing" should {

    "tolerate CRLF line endings, blank lines and comments" in {
      val response = completed(
        "event: message_start\r",
        MessageStart + "\r",
        "\r",
        ": this is an SSE comment",
        TextStart + "\r",
        textDelta("ok") + "\r",
        MessageDelta + "\r",
        MessageStop + "\r"
      )
      response.content.flatMap(_.text) mustBe Seq("ok")
    }

    "ignore an event type it has never seen rather than failing the response" in {
      val response = completed(
        MessageStart,
        """data: {"type":"some_future_event","index":0,"payload":{}}""",
        TextStart,
        textDelta("ok"),
        MessageDelta,
        MessageStop
      )
      response.content.flatMap(_.text) mustBe Seq("ok")
    }

    "ignore a delta type it has never seen rather than failing the response" in {
      val response = completed(
        MessageStart,
        TextStart,
        textDelta("ok"),
        """data: {"type":"content_block_delta","index":0,"delta":{"type":"citations_delta","citation":{}}}""",
        MessageDelta,
        MessageStop
      )
      response.content.flatMap(_.text) mustBe Seq("ok")
    }
  }

  "a broken stream" should {

    "fail with no status when it ends before message_delta" in {
      // A connection dropped mid-generation. Parsing what arrived would look to the caller exactly like a
      // short answer, so this is surfaced instead -- and, having no status, is retried as a transport blip.
      val f = failure(MessageStart, TextStart, textDelta("half an ans"))
      f.status mustBe None
      f.message must include("stop_reason")
    }

    "fail when it never opened with message_start" in {
      failure(TextStart, textDelta("orphan"), MessageDelta).status mustBe None
    }

    "fail on a delta for a block that never started" in {
      val f = failure(MessageStart, textDelta("orphan"), MessageDelta, MessageStop)
      f.message must include("no preceding content_block_start")
    }

    "fail on a data line that is not JSON" in {
      failure(MessageStart, "data: <html>502 Bad Gateway</html>").message must include("not valid JSON")
    }

    "report the first failure, not the last" in {
      failure(MessageStart, "data: not json", "data: also not json").message must include("not valid JSON")
    }
  }

  "a mid-stream error event" should {

    "surface overload as the same 529 it would have been at connect time" in {
      val f = failure(
        MessageStart,
        TextStart,
        textDelta("partial"),
        """data: {"type":"error","error":{"type":"overloaded_error","message":"Overloaded"}}"""
      )
      f.status mustBe Some(529)
      f.message must include("overloaded_error")
    }

    "produce an exception ClaudeClient still routes to the next model" in {
      // The bridge that makes mid-stream overload behave like connect-time overload: tryModels reads the
      // message, not the type.
      val e = ClaudeStreamException(529, "overloaded_error: Overloaded")
      ClaudeClient.isOverloadedError(e.getMessage) mustBe true
      ClaudeClient.isModelNotFoundError(ClaudeStreamException(404, "not_found_error: model").getMessage) mustBe true
    }

    "map each documented error type onto its HTTP equivalent" in {
      ClaudeStream.statusFor("overloaded_error") mustBe 529
      ClaudeStream.statusFor("rate_limit_error") mustBe 429
      ClaudeStream.statusFor("api_error") mustBe 500
      ClaudeStream.statusFor("authentication_error") mustBe 401
      ClaudeStream.statusFor("invalid_request_error") mustBe 400
    }
  }

  "describeError" should {

    "render Anthropic's error envelope as type and message" in {
      ClaudeStreamingClient.describeError(
        """{"type":"error","error":{"type":"rate_limit_error","message":"This request would exceed your rate limit"}}"""
      ) mustBe "rate_limit_error: This request would exceed your rate limit"
    }

    "fall back to the raw body when it is not that envelope" in {
      ClaudeStreamingClient.describeError("<html>502 Bad Gateway</html>") mustBe "<html>502 Bad Gateway</html>"
    }
  }
}
