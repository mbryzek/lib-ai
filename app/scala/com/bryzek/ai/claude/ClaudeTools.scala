package com.bryzek.ai.claude

import com.bryzek.claude.models.*
import com.bryzek.claude.models.json.*
import play.api.libs.json.{JsObject, JsValue}

/** Constructors for the two kinds of tool a request can carry.
  *
  * [[com.bryzek.claude.models.ClaudeTool]] is one flat model spanning both kinds, discriminated on `type` (see the
  * spec's own note for why it is not a union). That makes an illegal tool expressible: a `description` on a server tool
  * or a missing `input_schema` on a custom one both compile. Neither is tolerated by the API -- a web_search tool
  * carrying a description is rejected with `tools.0.web_search_20260209.description: Extra inputs are not permitted`
  * (verified against api.anthropic.com 2026-08-12) -- so these constructors exist to make the two shapes unmixable at
  * the only place a tool is built. Build tools here rather than with `ClaudeTool(...)` directly.
  *
  * A SERVER tool is not answered the way a custom tool is. It runs on Anthropic's infrastructure inside the same
  * request: there is no `tool_use` block to execute and no callback to write, and its results arrive as extra content
  * blocks on the same response ([[ClaudeServerToolResults]]). It therefore composes with
  * [[ClaudeClient.chatText]]/[[ClaudeClient.chatCompletion]] as well as with a tool loop -- a plain single-shot call
  * with `web_search` in `tools` answers from the live web with no loop at all.
  */
object ClaudeTools {

  /** Fixed names the API defines for its hosted tools. Sending anything else with these types is rejected. */
  private val WebSearchName = "web_search"
  private val WebFetchName = "web_fetch"

  /** A caller-executed tool: the model emits a `tool_use` block, [[ClaudeClient.runToolLoop]] runs `execute`, and the
    * result goes back as a `tool_result`. Wire bytes are identical to what this library sent before server tools
    * existed -- `type` is omitted, not spelled `custom` -- so adopting server tools changes nothing about traffic that
    * does not use them.
    *
    * `description` is what the model picks tools from, so say WHEN to call it, not only what it does.
    */
  def custom(
    name: String,
    description: String,
    inputSchema: JsObject,
    strict: Option[Boolean] = None,
    cacheControl: Option[ClaudeCacheControl] = None
  ): ClaudeTool =
    ClaudeTool(
      name = name,
      `type` = None,
      description = Some(description),
      inputSchema = Some(inputSchema),
      strict = strict,
      maxUses = None,
      allowedDomains = None,
      blockedDomains = None,
      citations = None,
      maxContentTokens = None,
      cacheControl = cacheControl
    )

  /** Anthropic-hosted web search. The model runs its own searches and reads the results server-side; only what it kept
    * enters the context window.
    *
    * `maxUses` is the cost lever and worth setting on anything user-facing: search is billed PER REQUEST ($10 per
    * 1,000) on top of tokens, and an unbounded model deciding how many searches a question deserves is an unbounded
    * bill. Exceeding the cap does not fail the request -- the tool result comes back as an error block, which
    * [[ClaudeServerToolResults]] surfaces rather than throwing on.
    *
    * `allowedDomains` and `blockedDomains` are mutually exclusive; passing both is a caller error the API rejects.
    */
  def webSearch(
    maxUses: Option[Long] = None,
    allowedDomains: Seq[String] = Nil,
    blockedDomains: Seq[String] = Nil,
    cacheControl: Option[ClaudeCacheControl] = None
  ): ClaudeTool = {
    require(
      allowedDomains.isEmpty || blockedDomains.isEmpty,
      "web_search accepts allowedDomains or blockedDomains, not both"
    )
    serverTool(WebSearchName, ClaudeToolType.WebSearch20260209, maxUses, allowedDomains, blockedDomains, cacheControl)
  }

  /** Anthropic-hosted web fetch. Retrieves a URL and puts the page into the model's context.
    *
    * It fetches only URLs ALREADY PRESENT in the conversation -- a page the caller named, or one web search turned up.
    * It is not a way to hand the model a browser, and pairing it with [[webSearch]] is the usual shape.
    *
    * `maxContentTokens` is the other half of the cost story: a fetch of an unbounded page otherwise lands the whole
    * page in the context window at input-token rates.
    */
  def webFetch(
    maxUses: Option[Long] = None,
    allowedDomains: Seq[String] = Nil,
    blockedDomains: Seq[String] = Nil,
    citations: Option[Boolean] = None,
    maxContentTokens: Option[Long] = None,
    cacheControl: Option[ClaudeCacheControl] = None
  ): ClaudeTool = {
    require(
      allowedDomains.isEmpty || blockedDomains.isEmpty,
      "web_fetch accepts allowedDomains or blockedDomains, not both"
    )
    serverTool(WebFetchName, ClaudeToolType.WebFetch20260209, maxUses, allowedDomains, blockedDomains, cacheControl)
      .copy(
        citations = citations.map(ClaudeCitationsConfig(_)),
        maxContentTokens = maxContentTokens
      )
  }

  private def serverTool(
    name: String,
    `type`: ClaudeToolType,
    maxUses: Option[Long],
    allowedDomains: Seq[String],
    blockedDomains: Seq[String],
    cacheControl: Option[ClaudeCacheControl]
  ): ClaudeTool =
    ClaudeTool(
      name = name,
      `type` = Some(`type`),
      description = None,
      inputSchema = None,
      strict = None,
      maxUses = maxUses,
      allowedDomains = Option.when(allowedDomains.nonEmpty)(allowedDomains),
      blockedDomains = Option.when(blockedDomains.nonEmpty)(blockedDomains),
      citations = None,
      maxContentTokens = None,
      cacheControl = cacheControl
    )

  /** Whether this tool is executed by the CALLER. The distinction is load-bearing wherever code reasons about "the
    * tools on this request": a tool loop's budget, its execute dispatch, and its abort-on-failure streak are all about
    * custom tools only -- a server tool never produces a `tool_use` block for any of them to see.
    */
  def isCustom(tool: ClaudeTool): Boolean = tool.`type`.isEmpty
}

/** Reading what an Anthropic-hosted tool returned.
  *
  * A server tool's results arrive as content blocks on an ordinary response, and the shape of a block's `content`
  * varies by outcome, not just by tool:
  *
  *   - `web_search_tool_result` succeeded -> an ARRAY of [[ClaudeWebSearchResult]]
  *   - `web_fetch_tool_result` succeeded -> a single [[ClaudeWebFetchResult]] object
  *   - EITHER of them failed -> a single [[ClaudeServerToolError]] object, on an HTTP **200**
  *
  * That last row is the whole reason this object exists. A failed server tool is not an exception, not a non-2xx, and
  * not a `stop_reason` -- it is a successful response whose result block happens to hold an error object where a list
  * was expected. Code that indexes into the content throws precisely on the cases worth handling (a `max_uses` cap that
  * did its job, a blocked domain, a page that was down), and it throws from inside whatever was reading the answer.
  * Everything here returns the error instead.
  */
object ClaudeServerToolResults {

  /** Every hit from every SUCCESSFUL web search on this response, in order. Failed searches contribute nothing here and
    * are reported by [[errors]] -- so a caller that renders citations should read both, or it will show an empty list
    * with no indication that anything went wrong.
    */
  def searchResults(response: ClaudeResponse): Seq[ClaudeWebSearchResult] =
    response.content
      .collect { case b if b.`type` == ClaudeContentType.WebSearchToolResult => decodeSearch(b) }
      .collect { case Some(Right(results)) =>
        results
      }
      .flatten

  /** Every SUCCESSFUL web fetch on this response, in order. Same caveat as [[searchResults]] about failures. */
  def fetchResults(response: ClaudeResponse): Seq[ClaudeWebFetchResult] =
    response.content.collect { case b if b.`type` == ClaudeContentType.WebFetchToolResult => decodeFetch(b) }.collect {
      case Some(Right(result)) => result
    }

  /** Every server tool on this response that FAILED. Empty is the normal case; non-empty on an otherwise fine answer
    * means the model answered with less than it asked for, which is worth logging even when the answer is usable.
    */
  def errors(response: ClaudeResponse): Seq[ClaudeServerToolError] =
    response.content.flatMap { b =>
      b.`type` match {
        case ClaudeContentType.WebSearchToolResult => decodeSearch(b).collect { case Left(e) => e }
        case ClaudeContentType.WebFetchToolResult => decodeFetch(b).collect { case Left(e) => e }
        case _ => None
      }
    }

  /** One `web_search_tool_result` block: its hits, or the error it failed with. None when `block` is not a web search
    * result at all, or when its content is a shape this client does not recognize -- an unreadable block is not an
    * error the API reported, and reporting it as one would invent a failure.
    */
  def decodeSearch(block: ClaudeContentBlock): Option[Either[ClaudeServerToolError, Seq[ClaudeWebSearchResult]]] =
    Option.when(block.`type` == ClaudeContentType.WebSearchToolResult)(block).flatMap { b =>
      b.content.flatMap { content =>
        asError(content) match {
          case Some(e) => Some(Left(e))
          case None => content.asOpt[Seq[ClaudeWebSearchResult]].map(Right(_))
        }
      }
    }

  /** One `web_fetch_tool_result` block: the page it retrieved, or the error it failed with. See [[decodeSearch]] for
    * what None means.
    */
  def decodeFetch(block: ClaudeContentBlock): Option[Either[ClaudeServerToolError, ClaudeWebFetchResult]] =
    Option.when(block.`type` == ClaudeContentType.WebFetchToolResult)(block).flatMap { b =>
      b.content.flatMap { content =>
        asError(content) match {
          case Some(e) => Some(Left(e))
          case None => content.asOpt[ClaudeWebFetchResult].map(Right(_))
        }
      }
    }

  /** An error object is discriminated by carrying an `error_code`, which no success shape has. Checked BEFORE the
    * success decode rather than as a fallback: a success decode that silently matched an error object would turn a
    * reported failure into an empty result set, which is the failure mode this whole object exists to prevent.
    */
  private def asError(content: JsValue): Option[ClaudeServerToolError] =
    Option.when((content \ "error_code").asOpt[String].isDefined)(content).flatMap(_.asOpt[ClaudeServerToolError])
}
