package com.bryzek.ai.claude

import com.bryzek.claude.models.{
  ClaudeContentBlock,
  ClaudeContentType,
  ClaudeModel,
  ClaudeResponse,
  ClaudeRole,
  ClaudeStopReason,
  ClaudeToolType,
  ClaudeUsage
}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsValue, Json}

class ClaudeToolsSpec extends AnyWordSpec with Matchers {

  private def response(blocks: ClaudeContentBlock*): ClaudeResponse = ClaudeResponse(
    id = "msg_1",
    `type` = "message",
    role = ClaudeRole.Assistant,
    content = blocks,
    model = ClaudeModel.ClaudeSonnet5,
    stopReason = ClaudeStopReason.EndTurn,
    usage = ClaudeUsage(inputTokens = 100, outputTokens = 100)
  )

  private def resultBlock(`type`: ClaudeContentType, content: JsValue): ClaudeContentBlock =
    ClaudeContentBlock(`type`).copy(toolUseId = Some("srvtoolu_1"), content = Some(content))

  private val searchHits = Json.arr(
    Json.obj(
      "type" -> "web_search_result",
      "title" -> "All Available Versions",
      "url" -> "https://www.scala-lang.org/download/all.html",
      "encrypted_content" -> "EocDCioIEhgCIiQ1"
    )
  )

  private val fetchPage = Json.obj(
    "type" -> "web_fetch_result",
    "url" -> "https://example.com/a",
    "retrieved_at" -> "2026-08-12T00:00:00Z"
  )

  /** The shape a FAILED server tool comes back as: an HTTP 200, an ordinary result block, and a single error OBJECT
    * where a success carries a list. `max_uses_exceeded` is the one every caller that sets a cap will meet.
    */
  private val searchError = Json.obj("type" -> "web_search_tool_result_error", "error_code" -> "max_uses_exceeded")

  "ClaudeTools.custom" should {
    "carry the custom fields and no server-tool ones" in {
      val tool = ClaudeTools.custom("get_weather", "Call when asked about weather", Json.obj("type" -> "object"))

      // `type` absent is what makes it a custom tool -- and keeps the wire bytes identical to what this library
      // sent before server tools existed, so adopting them changes nothing for traffic that does not use them.
      tool.`type` mustBe None
      tool.description mustBe Some("Call when asked about weather")
      tool.inputSchema mustBe Some(Json.obj("type" -> "object"))
      tool.maxUses mustBe None
      tool.allowedDomains mustBe None
      ClaudeTools.isCustom(tool) mustBe true
    }
  }

  "ClaudeTools.webSearch" should {
    "use the API's fixed name and type, and carry no description or input_schema" in {
      val tool = ClaudeTools.webSearch(maxUses = Some(5))

      tool.name mustBe "web_search"
      tool.`type` mustBe Some(ClaudeToolType.WebSearch20260209)
      tool.maxUses mustBe Some(5L)
      // Verified against api.anthropic.com on 2026-08-12: a server tool carrying a description is rejected with
      // `tools.0.web_search_20260209.description: Extra inputs are not permitted`. The API enforces the split
      // rather than ignoring the extra field, so these must stay empty.
      tool.description mustBe None
      tool.inputSchema mustBe None
      ClaudeTools.isCustom(tool) mustBe false
    }

    "omit empty domain filters rather than sending empty lists" in {
      ClaudeTools.webSearch().allowedDomains mustBe None
      ClaudeTools.webSearch().blockedDomains mustBe None
      ClaudeTools.webSearch(allowedDomains = Seq("scala-lang.org")).allowedDomains mustBe Some(Seq("scala-lang.org"))
    }

    "refuse both domain filters at once" in {
      // Mutually exclusive on the wire; failing here names the caller error instead of surfacing it as an API 400.
      an[IllegalArgumentException] must be thrownBy
        ClaudeTools.webSearch(allowedDomains = Seq("a.com"), blockedDomains = Seq("b.com"))
    }
  }

  "ClaudeTools.webFetch" should {
    "carry its own options alongside the shared server-tool ones" in {
      val tool = ClaudeTools.webFetch(maxUses = Some(2), citations = Some(true), maxContentTokens = Some(10000))

      tool.name mustBe "web_fetch"
      tool.`type` mustBe Some(ClaudeToolType.WebFetch20260209)
      tool.maxUses mustBe Some(2L)
      tool.citations.map(_.enabled) mustBe Some(true)
      tool.maxContentTokens mustBe Some(10000L)
      tool.description mustBe None
    }

    "refuse both domain filters at once" in {
      an[IllegalArgumentException] must be thrownBy
        ClaudeTools.webFetch(allowedDomains = Seq("a.com"), blockedDomains = Seq("b.com"))
    }
  }

  "ClaudeServerToolResults" should {
    "read the hits of a successful web search" in {
      val results = ClaudeServerToolResults.searchResults(
        response(resultBlock(ClaudeContentType.WebSearchToolResult, searchHits))
      )

      results.map(_.url) mustBe Seq("https://www.scala-lang.org/download/all.html")
      // Opaque, and echoed back verbatim on a later turn so the API restores the page without re-fetching it.
      results.head.encryptedContent mustBe Some("EocDCioIEhgCIiQ1")
    }

    "read the page of a successful web fetch" in {
      val results = ClaudeServerToolResults.fetchResults(
        response(resultBlock(ClaudeContentType.WebFetchToolResult, fetchPage))
      )

      results.map(_.url) mustBe Seq("https://example.com/a")
    }

    "return a failed server tool as an error instead of throwing" in {
      // The trap this whole object exists for: HTTP 200, ordinary result block, error object where a list was
      // expected. Anything indexing into the content throws precisely on the cases worth handling.
      val resp = response(resultBlock(ClaudeContentType.WebSearchToolResult, searchError))

      ClaudeServerToolResults.errors(resp).map(_.errorCode) mustBe Seq("max_uses_exceeded")
      ClaudeServerToolResults.searchResults(resp) mustBe empty
      ClaudeServerToolResults.decodeSearch(resp.content.head).map(_.isLeft) mustBe Some(true)
    }

    "report a failure alongside the searches that did succeed" in {
      // A partly-failed turn still answers. Its answer is just built on less than it asked for, which is worth
      // logging even when it is usable -- so both readings have to be available on the same response.
      val resp = response(
        resultBlock(ClaudeContentType.WebSearchToolResult, searchHits),
        resultBlock(ClaudeContentType.WebSearchToolResult, searchError)
      )

      ClaudeServerToolResults.searchResults(resp).size mustBe 1
      ClaudeServerToolResults.errors(resp).map(_.errorCode) mustBe Seq("max_uses_exceeded")
    }

    "ignore blocks that are not server-tool results" in {
      val resp = response(ClaudeClient.textBlock("just prose"))

      ClaudeServerToolResults.searchResults(resp) mustBe empty
      ClaudeServerToolResults.fetchResults(resp) mustBe empty
      ClaudeServerToolResults.errors(resp) mustBe empty
      ClaudeServerToolResults.decodeSearch(resp.content.head) mustBe None
    }

    "not invent a failure for content it cannot read" in {
      // An unreadable block is not an error the API reported; reporting it as one would manufacture a failure.
      val resp = response(resultBlock(ClaudeContentType.WebSearchToolResult, Json.obj("unexpected" -> "shape")))

      ClaudeServerToolResults.errors(resp) mustBe empty
      ClaudeServerToolResults.searchResults(resp) mustBe empty
      ClaudeServerToolResults.decodeSearch(resp.content.head) mustBe None
    }
  }

  "ClaudeClient.toolResultBlock" should {
    "write a custom tool's answer as a json string" in {
      // `content` is raw json because the wire type varies by block type -- an array on a web search result, an
      // object on a fetch or an error. A caller-executed tool answers with a STRING, which is what the API expects
      // here, so the widened field must not change what this block sends.
      val block = ClaudeClient.toolResultBlock("toolu_1", ClaudeToolOutput("42"))

      block.content mustBe Some(Json.toJson("42"))
      block.toolUseId mustBe Some("toolu_1")
      block.isError mustBe None
    }

    "flag an errored tool result" in {
      ClaudeClient.toolResultBlock("toolu_1", ClaudeToolOutput("boom", isError = true)).isError mustBe Some(true)
    }
  }
}
