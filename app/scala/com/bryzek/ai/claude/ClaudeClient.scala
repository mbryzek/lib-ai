package com.bryzek.ai.claude

import cats.data.{NonEmptyChain, ValidatedNec}
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*
import com.bryzek.claude.response.models.*
import com.bryzek.claude.response.models.json.*
import com.bryzek.claude.client.IClient
import generated.errors.{ApiException, ClaudeErrorResponseResponse}
import com.bryzek.claude.models.*
import com.google.inject.ImplementedBy
import play.api.libs.json.*

import java.io.IOException
import java.net.UnknownHostException
import java.nio.channels.ClosedByInterruptException
import java.util.UUID
import java.util.concurrent.{Executors, ThreadFactory, TimeUnit, TimeoutException}
import javax.net.ssl.SSLException
import javax.inject.Inject
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success, Try}

case class ClaudeConfig(key: String, anthropicVersion: String, betaHeaders: Seq[String] = Seq.empty)
object ClaudeConfig {
  private val Version = "2023-06-01"
  private val StructuredOutputsBeta = "structured-outputs-2025-11-13"

  /** Gates `output_config.task_budget` (see [[AiRequest.taskBudgetFor]]). Sent on every request rather than only the
    * ones that carry a budget: a beta header that a request does not exercise costs nothing, and making the header
    * conditional on the field would put the two in two places that have to agree.
    */
  private val TaskBudgetsBeta = "task-budgets-2026-03-13"

  def apply(key: String): ClaudeConfig = ClaudeConfig(
    key = key,
    anthropicVersion = Version,
    betaHeaders = Seq(StructuredOutputsBeta, TaskBudgetsBeta)
  )
}

sealed trait ClaudeEnvironment
object ClaudeEnvironment {
  case object Sandbox extends ClaudeEnvironment
  case object Production extends ClaudeEnvironment
}

/** How long a cache entry written at one of this request's breakpoints stays readable.
  *
  * Hand-written rather than generated: the wire values are `5m` and `1h`, and apibuilder's Scala generator names an
  * enum's case object after its wire value, neither of which is a legal Scala identifier. The wire field is a plain
  * string ([[com.bryzek.claude.models.ClaudeCacheControl.ttl]]) and this is the type callers choose from.
  *
  * Picking one is arithmetic, not taste. A read costs 0.1x input at either TTL; a WRITE costs 1.25x at 5m and 2x at 1h.
  * So the longer TTL is only cheaper when it converts misses into hits, and it is strictly worse when the prefix would
  * have been re-sent inside five minutes anyway (paying 2x for a write that 1.25x would have bought) or when it is not
  * re-sent at all (paying 2x for a write nothing ever reads). Measure the gap between consecutive calls of the same
  * shape before choosing -- and then measure `cache_read_input_tokens` afterwards to confirm it actually paid.
  */
sealed abstract class ClaudeCacheTtl(val wireValue: String)

object ClaudeCacheTtl {

  /** The API's own default, and this library's. Right for calls that repeat back-to-back -- a tool loop's turns, or a
    * per-club stage whose clubs run one after another.
    */
  case object FiveMinutes extends ClaudeCacheTtl("5m")

  /** Right for a repeated call shape whose consecutive uses are minutes apart rather than seconds -- e.g. a per-club
    * pipeline stage that runs once per club while each club's whole pipeline takes longer than five minutes, so the
    * shared prefix would otherwise expire between every pair of clubs.
    */
  case object OneHour extends ClaudeCacheTtl("1h")
}

/** A single tool call the model requested (a `tool_use` content block). */
case class ClaudeToolUse(id: String, name: String, input: JsObject)

/** The result of executing a tool, returned to the model as a `tool_result` block. */
case class ClaudeToolOutput(content: String, isError: Boolean = false)

/** One executed tool call: what the model asked for and what we returned. */
case class ClaudeToolInvocation(use: ClaudeToolUse, output: ClaudeToolOutput)

/** Outcome of [[ClaudeClient.runToolLoop]]: the parsed final answer, the full tool transcript, and `model` -- the model
  * the FINAL (structured-answer) turn resolved to after any fallback, so a caller can record which model actually
  * produced the answer (the tool-call turns may differ only under fallback).
  */
case class ClaudeToolLoopResult[T](value: T, invocations: Seq[ClaudeToolInvocation], turns: Int, model: ClaudeModel)

case class AiRequest(
  messages: Seq[ClaudeMessage],
  maxTokens: Long = 30000L,
  system: Option[String] = None,
  cacheSystem: Boolean = false,
  cacheLastMessage: Boolean = false,
  thinking: ClaudeThinkingType = ClaudeThinkingType.Adaptive,
  effort: Option[ClaudeEffort] = None,
  /** TTL for every cache breakpoint this request writes -- the ones `cacheSystem`/`cacheLastMessage` place, and the
    * ones a tool loop places on its own. See [[ClaudeCacheTtl]] for how to choose; it is a cost decision, and the wrong
    * choice makes caching more expensive rather than merely less effective.
    */
  cacheTtl: ClaudeCacheTtl = ClaudeCacheTtl.FiveMinutes
) {
  require(!cacheSystem || system.isDefined, "cacheSystem=true requires system to be set")

  def toClaudeRequest(model: ClaudeModel): ClaudeRequest = {
    def ephemeral: Option[ClaudeCacheControl] = ClaudeClient.ephemeralFor(cacheTtl)
    val systemBlocks = system.map { text =>
      Seq(
        ClaudeSystemBlock(
          text = text,
          cacheControl = if (cacheSystem) ephemeral else None
        )
      )
    }
    val msgs = if (cacheLastMessage && messages.nonEmpty) {
      val init = messages.init
      val last = messages.last
      val lastContent = last.content
      val taggedContent =
        if (lastContent.isEmpty) lastContent
        else lastContent.init :+ lastContent.last.copy(cacheControl = ephemeral)
      init :+ last.copy(content = taggedContent)
    } else {
      messages
    }
    ClaudeRequest(
      model = model,
      messages = msgs,
      maxTokens = maxTokens,
      system = systemBlocks,
      tools = None,
      toolChoice = None,
      outputConfig = {
        val budget = AiRequest.taskBudgetFor(model, maxTokens)
        Option.when(effort.isDefined || budget.isDefined)(
          ClaudeOutputConfig(effort = effort, format = None, taskBudget = budget)
        )
      },
      thinking = KnownClaudeModel.validate(model).toOption match {
        case Some(KnownClaudeModel.ClaudeFable5) =>
          // Fable 5 rejects any explicit thinking config except adaptive (thinking is always on);
          // omitting the field runs adaptive, so a Disabled request cannot be honored on this model.
          None
        case Some(KnownClaudeModel.ClaudeHaiku45) =>
          // Haiku 4.5 predates adaptive thinking and rejects it ("adaptive thinking is not
          // supported on this model"). Its only thinking mode is a budget-based config we don't
          // model, so omit the field and the request runs without thinking.
          None
        case Some(KnownClaudeModel.ClaudeSonnet5 | KnownClaudeModel.ClaudeOpus5) | None =>
          // Opus 5 thinks by default; an explicit Disabled is accepted only at effort high or
          // below, so pairing Disabled with xhigh/max is rejected by the API as a caller error.
          Some(ClaudeThinking(thinking))
      }
    )
  }
}

object AiRequest {

  /** Smallest `total` the API accepts on a task budget. Below it the request is rejected outright, so a budget that
    * would not clear it is omitted rather than clamped up -- clamping would hand a 200-token request a 20,000-token
    * budget, which is not a budget.
    */
  private[claude] val MinTaskBudget = 20000L

  /** Share of `max_tokens` offered to the model as its advisory budget. The remaining quarter is hard headroom: the
    * budget is a countdown the model paces itself against, `max_tokens` is the enforced ceiling it never sees, so
    * leaving room between the two is what turns "truncated mid-sentence" into "wrapped up early".
    */
  private[claude] val TaskBudgetFraction = 0.75

  /** The advisory budget for a request, or None when there is no useful one to send.
    *
    * `effort` is a hint about how hard to think, not a bound on it, and `budget_tokens` is removed on every model in
    * this enum -- so before task budgets there was NOTHING bounding a thinking pass, and a large enough one could eat
    * the whole of `max_tokens` and emit no answer at all (ISS-1717, ISS-1320). This is the API's purpose-built answer:
    * the server injects a countdown marker the model sees while generating.
    */
  private[claude] def taskBudgetFor(model: ClaudeModel, maxTokens: Long): Option[ClaudeTaskBudget] = {
    val supported = KnownClaudeModel.validate(model).toOption match {
      // Task budgets are not offered on Haiku 4.5; sending one is a caller error, not a no-op.
      case Some(KnownClaudeModel.ClaudeHaiku45) => false
      // An unrecognized model id is one we have not qualified. Omitting the budget loses the pacing; sending it to a
      // model that does not take it loses the whole request.
      case None => false
      case Some(KnownClaudeModel.ClaudeSonnet5 | KnownClaudeModel.ClaudeOpus5 | KnownClaudeModel.ClaudeFable5) => true
    }
    val total = (maxTokens * TaskBudgetFraction).toLong
    Option.when(supported && total >= MinTaskBudget)(ClaudeTaskBudget(total = total))
  }
}

case class ClaudeRequestMetadata(
  client: IClient,
  id: String,
  request: ClaudeRequest,
  context: Option[String] = None,
  // Which product feature/pipeline stage issued this request (e.g. "insight_investigate",
  // "sms_onboarding"), for cost/usage attribution. Distinct from `context`, which is a free-text
  // per-call identifier (e.g. a run/stage id) -- `feature` is the low-cardinality label a store
  // persists and rolls up by.
  feature: Option[String] = None
) {
  val start: Long = System.currentTimeMillis()

  def error(msg: String, raw: Option[String] = None): ClaudeError =
    ClaudeError(message = s"$msg [Request ID: $id]", raw = raw)
}

case class ClaudeResponseMetadata[T](request: ClaudeRequestMetadata, response: ClaudeResponse, content: T) {
  val duration: Long = System.currentTimeMillis() - request.start
}

/** Persists the full request/response audit. `context` on the request metadata carries the run-stage id so transcripts
  * correlate to the pipeline stage that produced them (rallyd's proven pattern).
  */
trait ClaudeStore {
  def storeRequest(request: ClaudeRequestMetadata): Unit
  def storeResponseError(request: ClaudeRequestMetadata, errors: Seq[ClaudeError]): Unit
  def storeResponseSuccess[T](response: ClaudeResponseMetadata[T]): Unit
}

case object NoopClaudeStore extends ClaudeStore {
  override def storeRequest(request: ClaudeRequestMetadata): Unit = ()
  override def storeResponseError(request: ClaudeRequestMetadata, errors: Seq[ClaudeError]): Unit = ()
  override def storeResponseSuccess[T](response: ClaudeResponseMetadata[T]): Unit = ()
}

@ImplementedBy(classOf[ClaudeClientFactoryImpl])
trait ClaudeClientFactory {
  final def instance(env: ClaudeEnvironment, apiKey: String)(store: ClaudeStore): ClaudeClient = {
    ClaudeClient(getClient(env), ClaudeConfig(apiKey), store)
  }

  def getClient(env: ClaudeEnvironment): IClient
}

class ClaudeClientFactoryImpl @Inject() (
  productionClaudeClient: ProductionClaudeClient,
  testClaudeClient: TestClaudeClient
) extends ClaudeClientFactory {
  override def getClient(env: ClaudeEnvironment): IClient = {
    env match {
      case ClaudeEnvironment.Production => productionClaudeClient
      case ClaudeEnvironment.Sandbox => testClaudeClient
    }
  }
}

object ClaudeClient {

  /** Total HTTP attempts per model before falling back / failing (initial call + retries). */
  private val MaxHttpAttempts = 3

  /** This many consecutive fully-failed tool turns aborts the loop rather than looping into more failures. */
  private val MaxConsecutiveFailedTurns = 3

  private val scheduler = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    override def newThread(r: Runnable): Thread = {
      val t = new Thread(r, "claude-retry-scheduler")
      t.setDaemon(true)
      t
    }
  })

  /** Non-blocking delay (no Thread.sleep) used to space out retries. */
  private def delay(d: FiniteDuration): Future[Unit] = {
    val p = Promise[Unit]()
    scheduler.schedule(
      new Runnable { override def run(): Unit = p.success(()) },
      d.toMillis,
      TimeUnit.MILLISECONDS
    )
    p.future
  }

  def textBlock(text: String): ClaudeContentBlock =
    ClaudeContentBlock(ClaudeContentType.Text).copy(text = Some(text))

  def imageBlock(mediaType: ClaudeMediaType, base64: String): ClaudeContentBlock =
    ClaudeContentBlock(ClaudeContentType.Image).copy(
      source = Some(ClaudeSource(`type` = ClaudeSourceType.Base64, mediaType = mediaType, data = base64))
    )

  def documentBlock(base64: String): ClaudeContentBlock =
    ClaudeContentBlock(ClaudeContentType.Document).copy(
      source = Some(
        ClaudeSource(`type` = ClaudeSourceType.Base64, mediaType = ClaudeMediaType.ApplicationPdf, data = base64)
      )
    )

  def toolResultBlock(toolUseId: String, output: ClaudeToolOutput): ClaudeContentBlock =
    ClaudeContentBlock(ClaudeContentType.ToolResult).copy(
      toolUseId = Some(toolUseId),
      content = Some(output.content),
      isError = if (output.isError) Some(true) else None
    )

  def makeClaudeMessage(role: ClaudeRole, msg: String*): ClaudeMessage = {
    ClaudeMessage(
      role = role,
      content = msg.map(textBlock)
    )
  }

  /** Trailing user turn appended when finalizing a tool loop after the model stops calling tools, so the finalize
    * request does not end in an assistant turn (which would be a rejected assistant prefill).
    */
  private[claude] def finalizeInstruction: ClaudeMessage =
    makeClaudeMessage(
      ClaudeRole.User,
      "Return your final answer now as a single JSON object matching the required schema."
    )

  /** [[finalizeInstruction]]'s counterpart for a text-answer loop. Naming a JSON schema here would be worse than
    * useless -- there is no schema on the request, so the model would be asked to satisfy one that does not exist.
    */
  private[claude] def finalizeTextInstruction: ClaudeMessage =
    makeClaudeMessage(ClaudeRole.User, "Write your final answer now, for the person who asked.")

  /** An ephemeral breakpoint at `ttl`.
    *
    * [[ClaudeCacheTtl.FiveMinutes]] sends no `ttl` at all rather than the equivalent `"5m"`, so a caller that has not
    * asked for the longer TTL produces byte-identical requests to before this field existed. That is worth the branch:
    * it makes adopting the 1h TTL a per-caller decision with no silent change to anyone else's traffic.
    */
  private[claude] def ephemeralFor(ttl: ClaudeCacheTtl): Option[ClaudeCacheControl] =
    Some(ClaudeCacheControl(ttl = Option.when(ttl != ClaudeCacheTtl.FiveMinutes)(ttl.wireValue)))

  /** How many message boundaries carry a rolling cache breakpoint. Two lets a turn read the breakpoint the previous
    * turn wrote while writing its own, so hits accrue as the transcript grows. Together with the one static-prefix
    * breakpoint this stays inside the API's limit of 4 breakpoints per request.
    */
  private[claude] val ConversationBreakpoints = 2

  /** Puts one cache breakpoint on the static prefix of a tool-loop request. Blocks render `tools` -> `system` ->
    * `messages`, so a breakpoint on the LAST system block covers tools and system together; with no system prompt the
    * last tool is the end of the static prefix instead. Every turn of a loop resends this prefix byte-identical, so
    * caching it is always the right call regardless of what the caller asked for on [[AiRequest]]. A prefix shorter
    * than the model's minimum cacheable length simply does not cache -- there is no error and no extra cost.
    */
  private[claude] def cacheStaticPrefix(
    request: ClaudeRequest,
    ttl: ClaudeCacheTtl = ClaudeCacheTtl.FiveMinutes
  ): ClaudeRequest = {
    val ephemeral = ephemeralFor(ttl)
    request.system.filter(_.nonEmpty) match {
      case Some(blocks) =>
        request.copy(system = Some(blocks.init :+ blocks.last.copy(cacheControl = ephemeral)))
      case None =>
        request.copy(tools = request.tools.filter(_.nonEmpty).map(t => t.init :+ t.last.copy(cacheControl = ephemeral)))
    }
  }

  /** Re-applies the rolling conversation breakpoints, tagging the last content block of the newest message and of the
    * message one turn (two messages) earlier -- a turn is always an assistant message plus the user message answering
    * it, whether that is tool results or the finalize instruction. Existing markers are cleared first so the count
    * stays fixed as the transcript grows rather than accumulating past the API's limit -- a breakpoint is a read point,
    * not part of the cached bytes, so moving one does not invalidate anything already written.
    */
  private[claude] def cacheConversation(
    messages: Seq[ClaudeMessage],
    ttl: ClaudeCacheTtl = ClaudeCacheTtl.FiveMinutes
  ): Seq[ClaudeMessage] = {
    val ephemeral = ephemeralFor(ttl)
    val cleared = messages.map(m => m.copy(content = m.content.map(_.copy(cacheControl = None))))
    val breakpoints = Seq.tabulate(ConversationBreakpoints)(i => cleared.size - 1 - (i * 2)).filter(_ >= 0).toSet
    cleared.zipWithIndex.map {
      case (m, i) if breakpoints(i) && m.content.nonEmpty =>
        m.copy(content = m.content.init :+ m.content.last.copy(cacheControl = ephemeral))
      case (m, _) => m
    }
  }

  def toolUses(response: ClaudeResponse): Seq[ClaudeToolUse] =
    response.content.collect {
      case b if b.`type` == ClaudeContentType.ToolUse =>
        (b.id, b.name).mapN((id, name) => ClaudeToolUse(id, name, b.input.getOrElse(Json.obj())))
    }.flatten

  /** Checks if an error message indicates a 529 overloaded response from the Claude API. Matches the generated
    * `ApiException` message (`... failed with status 529`) rather than a bare `529`, which could collide with the
    * random request id embedded in the same error string.
    */
  def isOverloadedError(errorMessage: String): Boolean =
    errorMessage.contains("status 529")

  /** Checks if an error message indicates a 404 from the Claude API — a model this API key cannot access (e.g. a newly
    * released model id). Same `ApiException` message-shape matching as [[isOverloadedError]].
    */
  def isModelNotFoundError(errorMessage: String): Boolean =
    errorMessage.contains("status 404")

  /** Checks if an error message indicates the model declined the request (`stop_reason=refusal`, an HTTP 200 with no
    * text content — Fable 5's safety classifiers). Matches the message shape produced by `noTextError`. A refusal is
    * model-specific, so falling back to the next model in the chain is the documented recovery.
    */
  def isRefusalError(errorMessage: String): Boolean =
    errorMessage.contains(s"stop_reason=${ClaudeStopReason.Refusal}")

  /** Whether an error message says the request ran out of `max_tokens` -- thinking and the answer share that ceiling,
    * so a deep enough thinking pass leaves nothing for the answer. It surfaces as one of TWO messages depending on how
    * much of the budget thinking took, and both were observed on 2026-08-09 within twenty minutes of each other:
    *
    *   - thinking took all of it, so there is no text block at all (see [[ClaudeClient.noTextError]]) -- `No text
    *     content in response (stop_reason=max_tokens, output_tokens=64000)`
    *   - thinking left just enough to start the answer, which is then cut off mid-string -- `Content is not valid JSON:
    *     Unexpected end-of-input: was expecting closing quote ...`
    *
    * The second is the same failure wearing a parser error, so it gets the same response. Each is matched on two
    * independent phrases: `Content is not valid JSON` ON ITS OWN is an ordinary malformed response -- a content problem
    * the model can correct -- and it is only truncation when the parser also ran out of input.
    *
    * This lives here, next to the code that FORMATS those messages, because it used to live in platform: rewording
    * either message would have silently switched off the recovery over there, with no compile error and no test failure
    * here to catch it (ISS-1717).
    */
  def isBudgetExhaustionError(errorMessage: String): Boolean = {
    val thinkingTookEverything =
      errorMessage.contains("No text content") && errorMessage.contains(s"stop_reason=${ClaudeStopReason.MaxTokens}")
    val answerCutOffMidStream =
      errorMessage.contains("Content is not valid JSON") && errorMessage.contains("Unexpected end-of-input")
    thinkingTookEverything || answerCutOffMidStream
  }

  /** One rung down the effort ladder, or None at the floor. `None` means the caller sent no `effort`, which is the
    * API's own default of `high`, so it steps to `medium` exactly as an explicit high does. An UNDEFINED effort is a
    * value this client does not know; it has no place on the ladder, so it does not step.
    */
  private[claude] def lowerEffort(effort: Option[ClaudeEffort]): Option[ClaudeEffort] =
    KnownClaudeEffort.validate(effort.getOrElse(ClaudeEffort.High)).toOption.flatMap {
      case KnownClaudeEffort.Max => Some(ClaudeEffort.Xhigh)
      case KnownClaudeEffort.Xhigh => Some(ClaudeEffort.High)
      case KnownClaudeEffort.High => Some(ClaudeEffort.Medium)
      case KnownClaudeEffort.Medium => Some(ClaudeEffort.Low)
      case KnownClaudeEffort.Low => None
    }

}

final case class ClaudeOutputFormat(
  name: String,
  schema: _root_.play.api.libs.json.JsObject
) {
  def toApi: ClaudeApiOutputFormat = ClaudeApiOutputFormat(
    `type` = ClaudeOutputFormatType.JsonSchema,
    schema = schema
  )
}

case class ClaudeClient(
  client: IClient,
  config: ClaudeConfig,
  store: ClaudeStore
) {
  import ClaudeClient.*

  private val defaultHeaders: Seq[(String, String)] = {
    Seq(
      "x-api-key" -> config.key,
      "Content-Type" -> "application/json",
      "anthropic-version" -> config.anthropicVersion
    ) ++ (config.betaHeaders.toList match {
      case Nil => Nil
      case all => Seq("anthropic-beta" -> all.mkString(","))
    })
  }

  private def randomId(prefix: String): String = {
    prefix + "-" + UUID.randomUUID().toString.replaceAll("-", "")
  }

  def makeClaudeMessage(role: ClaudeRole, msg: String*): ClaudeMessage = ClaudeClient.makeClaudeMessage(role, msg*)

  def chatComments(
    request: AiRequest,
    models: Seq[ClaudeModel],
    context: Option[String] = None,
    feature: Option[String] = None
  )(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[String]]] = {
    chatCompletion[CommentsResponse](request, ClaudeOutputFormats.CommentsResponse, models, context, feature)(using ec)
      .map(_.map(_.content.comments))
  }

  def chatRecommendations(
    request: AiRequest,
    models: Seq[ClaudeModel],
    context: Option[String] = None,
    feature: Option[String] = None
  )(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, RecommendationResponse]] = {
    chatCompletion[RecommendationResponse](
      request,
      ClaudeOutputFormats.RecommendationsResponse,
      models,
      context,
      feature
    )(using
      ec
    )
      .map(_.map(_.content))
  }

  def chatInsight(
    request: AiRequest,
    models: Seq[ClaudeModel],
    context: Option[String] = None,
    feature: Option[String] = None
  )(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[String]]] = {
    chatComments(request, models, context, feature)(using ec)
  }

  def chatSingleInsight(
    request: AiRequest,
    models: Seq[ClaudeModel],
    context: Option[String] = None,
    feature: Option[String] = None
  )(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, String]] = {
    chatCompletion[SingleInsightResponse](request, ClaudeOutputFormats.SingleInsight, models, context, feature)(using
      ec
    )
      .map(_.map(_.content.insight))
  }

  def chatInsightSections(
    request: AiRequest,
    models: Seq[ClaudeModel],
    context: Option[String] = None,
    feature: Option[String] = None
  )(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[InsightSection]]] = {
    chatCompletion[InsightSectionsResponse](
      request,
      ClaudeOutputFormats.InsightSectionsResponse,
      models,
      context,
      feature
    )(using
      ec
    )
      .map(_.map(_.content.sections))
  }

  def chatText(
    request: AiRequest,
    models: Seq[ClaudeModel],
    context: Option[String] = None,
    feature: Option[String] = None
  )(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[String]]] = {
    withEffortStepDown(request) { req =>
      tryModels(models) { model =>
        chatTextSingle(req.toClaudeRequest(model), context, feature)
      }
    }
  }

  def chatCompletion[T](
    request: AiRequest,
    outputFormat: ClaudeOutputFormat,
    models: Seq[ClaudeModel],
    context: Option[String] = None,
    feature: Option[String] = None
  )(implicit
    ec: ExecutionContext,
    reads: Reads[T]
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]] = {
    withEffortStepDown(request) { req =>
      tryModels(models) { model =>
        chatCompletionSingle(req.toClaudeRequest(model), outputFormat, context, feature)
      }
    }
  }

  /** Agentic tool loop. Sends `request` with `tools` and lets the model call them; every `tool_use` block is executed
    * via `execute`, and all results for a turn are returned in ONE user message (splitting degrades parallel tool
    * calling). Budget-capped at `maxCalls` tool executions; a run of [[ClaudeClient.MaxConsecutiveFailedTurns]]
    * consecutive fully-failed tool turns aborts. The final turn forces `tool_choice: none` + structured output so the
    * answer parses into `T` without regex-and-nudge. Every request/response is journaled via the store with `context`.
    *
    * That structured final turn is the most expensive one in the loop, and a caller whose answer is PROSE a person
    * reads -- rather than an object something parses -- should use [[runToolLoopText]] instead and not pay for it.
    *
    * Prompt caching is applied automatically and does not depend on [[AiRequest.cacheSystem]] /
    * [[AiRequest.cacheLastMessage]]: a loop resends tools + system + the entire growing transcript on every turn, so
    * the static prefix gets one breakpoint ([[ClaudeClient.cacheStaticPrefix]]) and the transcript gets rolling ones
    * ([[ClaudeClient.cacheConversation]]). Verify it is working via `usage.cache_read_input_tokens` on the journaled
    * responses -- a persistent zero there means something is invalidating the prefix between turns.
    */
  def runToolLoop[T](
    request: AiRequest,
    tools: Seq[ClaudeTool],
    models: Seq[ClaudeModel],
    maxCalls: Int,
    finalFormat: ClaudeOutputFormat,
    context: Option[String] = None,
    feature: Option[String] = None
  )(execute: ClaudeToolUse => Future[ClaudeToolOutput])(implicit
    ec: ExecutionContext,
    reads: Reads[T]
  ): Future[ValidatedNec[ClaudeError, ClaudeToolLoopResult[T]]] =
    runLoop[T](request, tools, models, maxCalls, finalizeInstruction, context, feature)(execute) { (base, messages) =>
      // `tools` stays on the request and `tool_choice: none` does the forbidding. Dropping the tools would change
      // the very front of the prefix and throw away the tools+system cache on the most expensive turn of the loop.
      val req = base.copy(
        messages = cacheConversation(messages, request.cacheTtl),
        toolChoice = Some(ClaudeToolChoice(ClaudeToolChoiceType.None)),
        outputConfig = Some(mergeFormat(base.outputConfig, finalFormat))
      )
      sendAndStore[T](req, structuredHeaders(finalFormat), context, feature)((rm, resp) => parseText[T](rm, resp))
    }

  /** [[runToolLoop]] for a caller that wants PROSE, not a parsed object: identical tool loop, but the final turn is a
    * plain text generation with no `output_config.format` and no schema to satisfy.
    *
    * Why this exists rather than everyone taking a structured answer. A structured finalize is not free, and it is the
    * single most expensive turn of a loop -- it runs last, after all the real work, with the largest output budget.
    * Every schema in [[ClaudeOutputFormats]] pairs its payload with a `steps` chain-of-thought array, so a caller that
    * wants one string pays for a reasoning trace as well, and then usually discards it. Measured against the live API
    * on 2026-08-06 with the club chat's own request shape (22 tools, its system prompt, a month of tool results, both
    * with thinking off): the structured finalize took a median 12.1s and emitted 807 output tokens; the same turn as
    * text took 4.6s and 198 tokens. That is the difference between an answer and a timeout on a surface with somebody
    * waiting on it (ISS-677).
    *
    * The tradeoff is real and belongs to the caller: a text answer cannot be validated, so anything that has to PARSE
    * the result -- the insight pipeline's findings, a judgment, a classification -- must keep using [[runToolLoop]].
    * This is for answers a person reads.
    */
  def runToolLoopText(
    request: AiRequest,
    tools: Seq[ClaudeTool],
    models: Seq[ClaudeModel],
    maxCalls: Int,
    context: Option[String] = None,
    feature: Option[String] = None
  )(execute: ClaudeToolUse => Future[ClaudeToolOutput])(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, ClaudeToolLoopResult[String]]] =
    runLoop[String](request, tools, models, maxCalls, finalizeTextInstruction, context, feature)(execute) {
      (base, messages) =>
        // `outputConfig` is carried through untouched rather than cleared: it also holds the caller's `effort`, and
        // it is only the FORMAT this loop declines to set.
        val req = base.copy(
          messages = cacheConversation(messages, request.cacheTtl),
          toolChoice = Some(ClaudeToolChoice(ClaudeToolChoiceType.None)),
          outputConfig = base.outputConfig
        )
        sendAndStore[String](req, defaultHeaders, context, feature) { (rm, resp) =>
          val text = textContent(resp)
          if (text.nonEmpty) ClaudeResponseMetadata(rm, resp, text).validNec else noTextError(rm, resp).invalidNec
        }
    }

  /** The tool loop itself, shared by [[runToolLoop]] and [[runToolLoopText]], which differ only in how the last turn
    * asks for the answer. `finalizeTurn` receives the cached base request and the transcript to answer from; the
    * `finalizeWith` message is the trailing user turn appended when the model stops calling tools on its own.
    */
  private def runLoop[T](
    request: AiRequest,
    tools: Seq[ClaudeTool],
    models: Seq[ClaudeModel],
    maxCalls: Int,
    finalizeWith: ClaudeMessage,
    context: Option[String],
    feature: Option[String]
  )(execute: ClaudeToolUse => Future[ClaudeToolOutput])(
    finalizeTurn: (ClaudeRequest, Seq[ClaudeMessage]) => Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]]
  )(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, ClaudeToolLoopResult[T]]] = {
    withEffortStepDown(request) { req =>
      tryModels(models) { model =>
        // Bound before `loop`'s own `req` shadows the outer one. `withEffortStepDown` only ever copies the request
        // with a lower effort, so this is the caller's TTL whichever attempt we are on.
        val ttl = req.cacheTtl
        // Every turn resends tools + system + the whole transcript, so the prefix is cached unconditionally here.
        val base = cacheStaticPrefix(req.toClaudeRequest(model).copy(tools = Some(tools)), ttl)

        def finalize(
          messages: Seq[ClaudeMessage],
          invocations: Seq[ClaudeToolInvocation],
          turns: Int
        ): Future[ValidatedNec[ClaudeError, ClaudeToolLoopResult[T]]] = {
          finalizeTurn(base, messages)
            .map(_.map(parsed => ClaudeToolLoopResult(parsed.content, invocations, turns, parsed.response.model)))
        }

        def loop(
          messages: Seq[ClaudeMessage],
          callsUsed: Int,
          consecutiveErrors: Int,
          invocations: Seq[ClaudeToolInvocation],
          turns: Int
        ): Future[ValidatedNec[ClaudeError, ClaudeToolLoopResult[T]]] = {
          if (callsUsed >= maxCalls) {
            finalize(messages, invocations, turns)
          } else {
            val req = base.copy(
              messages = cacheConversation(messages, ttl),
              toolChoice = Some(ClaudeToolChoice(ClaudeToolChoiceType.Auto)),
              outputConfig = base.outputConfig
            )
            sendRaw(req, defaultHeaders, context, feature).flatMap {
              case Invalid(errors) => Future.successful(Invalid(errors))
              case Valid(rm) =>
                val uses = ClaudeClient.toolUses(rm.response)
                if (rm.response.stopReason == ClaudeStopReason.ToolUse && uses.nonEmpty) {
                  Future.traverse(uses)(u => runTool(execute, u)).flatMap { newInvocations =>
                    val allErrored = newInvocations.forall(_.output.isError)
                    val streak = if (allErrored) consecutiveErrors + 1 else 0
                    if (streak >= MaxConsecutiveFailedTurns) {
                      Future.successful(
                        rm.request
                          .error(s"Aborting tool loop after $streak consecutive fully-failed tool turns")
                          .invalidNec
                      )
                    } else {
                      val assistantMsg = ClaudeMessage(ClaudeRole.Assistant, rm.response.content)
                      val resultMsg = ClaudeMessage(
                        ClaudeRole.User,
                        newInvocations.map(i => toolResultBlock(i.use.id, i.output))
                      )
                      loop(
                        messages :+ assistantMsg :+ resultMsg,
                        callsUsed + uses.size,
                        streak,
                        invocations ++ newInvocations,
                        turns + 1
                      )
                    }
                  }
                } else {
                  // Model stopped asking for tools; force the final answer. The finalize request must end in a
                  // user turn — a trailing assistant turn is an assistant prefill, which modern models reject
                  // with a 400 — so echo the model's turn and add an explicit user instruction.
                  val done = messages :+
                    ClaudeMessage(ClaudeRole.Assistant, rm.response.content) :+
                    finalizeWith
                  finalize(done, invocations, turns)
                }
            }
          }
        }

        loop(base.messages, 0, 0, Nil, 0)
      }
    }
  }

  /** Runs `attempt`, and if it comes back having exhausted `max_tokens`, runs it ONCE more one effort step lower
    * ([[ClaudeClient.lowerEffort]]) before returning an error to the caller.
    *
    * Recovering here rather than leaving it to the caller is the point (ISS-1717). The alternative -- and what platform
    * did -- is to recover on the NEXT task attempt: by then the stage is already marked `failed`, the error is
    * persisted where a person reads it, a framework retry is spent, and a full generation has been paid for. None of
    * that tells anyone anything they can act on, because the fix is mechanical. Absorbed here, the caller sees one
    * successful answer.
    *
    * Once, not down to the floor: a step-down means paying for a second full generation, and the ladder is a fallback
    * for a request whose budget was already misjudged, not a search. A second exhaustion is a real failure and surfaces
    * as one -- at that point the caller's `maxTokens` is the thing that is wrong, not its effort.
    */
  private def withEffortStepDown[T](request: AiRequest)(
    attempt: AiRequest => Future[ValidatedNec[ClaudeError, T]]
  )(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, T]] = {
    attempt(request).flatMap {
      case Invalid(errors) if errors.exists(e => ClaudeClient.isBudgetExhaustionError(e.message)) =>
        lowerEffort(request.effort) match {
          case None => Future.successful(Invalid(errors))
          case Some(lower) =>
            println(
              s"Claude request exhausted its output budget at effort " +
                s"${request.effort.getOrElse(ClaudeEffort.High)}, retrying once at $lower"
            )
            attempt(request.copy(effort = Some(lower)))
        }
      case result => Future.successful(result)
    }
  }

  /** Runs a caller-supplied tool, capturing a synchronous throw or a failed Future as an error result so a misbehaving
    * tool flows through the loop's error handling instead of failing the whole loop Future.
    */
  private def runTool(execute: ClaudeToolUse => Future[ClaudeToolOutput], use: ClaudeToolUse)(implicit
    ec: ExecutionContext
  ): Future[ClaudeToolInvocation] =
    Future
      .fromTry(Try(execute(use)))
      .flatten
      .recover { case NonFatal(e) =>
        ClaudeToolOutput(content = s"Tool execution failed: ${e.getMessage}", isError = true)
      }
      .map(ClaudeToolInvocation(use, _))

  private def mergeFormat(existing: Option[ClaudeOutputConfig], format: ClaudeOutputFormat): ClaudeOutputConfig =
    existing
      .getOrElse(ClaudeOutputConfig(effort = None, format = None, taskBudget = None))
      .copy(format = Some(format.toApi))

  private def structuredHeaders(format: ClaudeOutputFormat): Seq[(String, String)] =
    defaultHeaders ++ Seq((TestClaudeClient.OutputFormatNameHeader, format.name))

  private def tryModels[T](models: Seq[ClaudeModel])(
    attempt: ClaudeModel => Future[ValidatedNec[ClaudeError, T]]
  )(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, T]] = {
    def loop(remaining: List[ClaudeModel]): Future[ValidatedNec[ClaudeError, T]] = {
      remaining match {
        case Nil => Future.successful(ClaudeError(message = "No models provided").invalidNec)
        case model :: rest =>
          attempt(model).flatMap {
            case result @ Valid(_) => Future.successful(result)
            case result @ Invalid(errors) =>
              if (rest.nonEmpty && isOverloaded(errors)) {
                println(s"Claude model ${model} returned 529 overloaded, falling back to ${rest.head}")
                loop(rest)
              } else if (rest.nonEmpty && isModelNotFound(errors)) {
                println(s"Claude model ${model} returned 404 not found, falling back to ${rest.head}")
                loop(rest)
              } else if (rest.nonEmpty && isRefusal(errors)) {
                println(s"Claude model ${model} refused the request, falling back to ${rest.head}")
                loop(rest)
              } else {
                Future.successful(result)
              }
          }
      }
    }
    loop(models.toList)
  }

  private def isOverloaded(errors: NonEmptyChain[ClaudeError]): Boolean =
    errors.exists(e => ClaudeClient.isOverloadedError(e.message))

  private def isModelNotFound(errors: NonEmptyChain[ClaudeError]): Boolean =
    errors.exists(e => ClaudeClient.isModelNotFoundError(e.message))

  private def isRefusal(errors: NonEmptyChain[ClaudeError]): Boolean =
    errors.exists(e => ClaudeClient.isRefusalError(e.message))

  /** Retry the given HTTP attempt, honoring `Retry-After` on 429 and using jittered backoff on 5xx and transient
    * transport failures (read/request timeouts, connection resets). Non-retryable failures (or exhausted attempts)
    * propagate the original exception.
    *
    * Every request gets the same [[ClaudeClient.MaxHttpAttempts]], regardless of its size. ISS-1229 briefly traded
    * attempts away on large requests, because a non-streaming attempt had to be given a bigger and bigger slice of a
    * fixed wall-clock envelope to have any chance of finishing. Streaming makes that trade backwards: a failing
    * streaming attempt fails on [[ClaudeStreamingClient.IdleTimeout]] in minutes rather than burning its whole ceiling,
    * so a large request can afford its retries as easily as a small one -- and, being large, has more to lose by not
    * having them.
    */
  private def withRetries(attempt: => Future[ClaudeResponse])(implicit
    ec: ExecutionContext
  ): Future[ClaudeResponse] = {
    def loop(n: Int): Future[ClaudeResponse] =
      attempt.recoverWith {
        case NonFatal(e) if n < MaxHttpAttempts =>
          retryDelay(e) match {
            case Some(d) => delay(d).flatMap(_ => loop(n + 1))
            case None => Future.failed(e)
          }
      }
    loop(1)
  }

  private def retryDelay(e: Throwable): Option[FiniteDuration] = e match {
    case r: ClaudeErrorResponseResponse =>
      r.response.status match {
        case 429 => Some(retryAfter(r.response.header("Retry-After")))
        case s if s >= 500 => Some(jitter())
        case _ => None
      }
    // The streaming transport reports statuses through its own exception rather than a WSResponse (a streamed
    // response has no materialized body to hand back), so it gets the same status-driven decision.
    case s: ClaudeStreamException =>
      s.status match {
        case 429 => Some(retryAfter(s.retryAfter))
        case c if c >= 500 => Some(jitter())
        case _ => None
      }
    case a: ApiException if a.response.status >= 500 => Some(jitter())
    // AsyncHttpClient surfaces read/request timeouts as j.u.c.TimeoutException and dropped connections as
    // IOException; both are transient transport failures, not API rejections. Known-permanent IOException
    // subtypes (TLS/config, DNS, thread interrupt) fail fast instead of burning retries -- an exclusion
    // list, because the transient drops we do want to retry surface as bare IOException (e.g. AHC's
    // "Remotely closed") with no dedicated subtype to allow-list.
    case _: SSLException | _: UnknownHostException | _: ClosedByInterruptException => None
    case _: TimeoutException => Some(jitter())
    case _: IOException => Some(jitter())
    case _ => None
  }

  private def retryAfter(header: Option[String]): FiniteDuration =
    header
      .flatMap(h => Try(h.trim.toLong).toOption)
      .map(s => FiniteDuration(s, TimeUnit.SECONDS))
      .getOrElse(jitter())

  private def jitter(): FiniteDuration = FiniteDuration(500L + Random.nextInt(500), MILLISECONDS)

  /** Low-level send: journals the request, performs the retrying HTTP call, and returns the raw envelope. It does NOT
    * store the response — the audit outcome is recorded once, by [[sendAndStore]], on the fully content-parsed result
    * the caller actually receives (so a schema-parse failure is recorded as an error, not a success).
    */
  private def send(
    request: ClaudeRequest,
    headers: Seq[(String, String)],
    context: Option[String],
    feature: Option[String]
  )(implicit
    ec: ExecutionContext
  ): Future[(ClaudeRequestMetadata, ValidatedNec[ClaudeError, ClaudeResponse])] = {
    val rm = ClaudeRequestMetadata(client, randomId("req"), request, context, feature)
    store.storeRequest(rm)
    withRetries(client.createMessage(request, headers))
      .map(response => (rm, response.validNec))
      .recover {
        case r: ClaudeErrorResponseResponse => (rm, errorFrom(rm, r).invalidNec)
        case NonFatal(e) => (rm, rm.error(e.getMessage).invalidNec)
      }
  }

  /** Parses a non-2xx body into a [[ClaudeError]]; a malformed body (e.g. an HTML error page from a proxy) throws
    * inside the generated wrapper, so fall back to the status rather than letting it escape the Validated channel.
    */
  private def errorFrom(rm: ClaudeRequestMetadata, r: ClaudeErrorResponseResponse): ClaudeError =
    Try(r.claudeErrorResponse.error).getOrElse(rm.error(s"HTTP ${r.response.status}: ${r.getMessage}"))

  private def sendAndStore[T](
    request: ClaudeRequest,
    headers: Seq[(String, String)],
    context: Option[String],
    feature: Option[String]
  )(
    parse: (ClaudeRequestMetadata, ClaudeResponse) => ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]
  )(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]] = {
    send(request, headers, context, feature).map { case (rm, envelope) =>
      val result = envelope.andThen(resp => parse(rm, resp))
      storeResponse(rm, result)
      result
    }
  }

  /** Send that persists (and returns) the raw response — used for the intermediate tool-loop turns. */
  private def sendRaw(
    request: ClaudeRequest,
    headers: Seq[(String, String)],
    context: Option[String],
    feature: Option[String]
  )(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[ClaudeResponse]]] =
    sendAndStore(request, headers, context, feature)((rm, resp) => ClaudeResponseMetadata(rm, resp, resp).validNec)

  private def chatCompletionSingle[T](
    originalRequest: ClaudeRequest,
    outputFormat: ClaudeOutputFormat,
    context: Option[String],
    feature: Option[String]
  )(implicit
    ec: ExecutionContext,
    reads: Reads[T]
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]] = {
    val request = originalRequest.copy(
      outputConfig = Some(mergeFormat(originalRequest.outputConfig, outputFormat))
    )
    sendAndStore[T](request, structuredHeaders(outputFormat), context, feature)((rm, resp) => parseText[T](rm, resp))
  }

  private def chatTextSingle(originalRequest: ClaudeRequest, context: Option[String], feature: Option[String])(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[String]]] = {
    sendAndStore[String](originalRequest, defaultHeaders, context, feature) { (rm, resp) =>
      val text = textContent(resp)
      if (text.nonEmpty) {
        ClaudeResponseMetadata(rm, resp, text).validNec
      } else {
        noTextError(rm, resp).invalidNec
      }
    }
  }

  private def storeResponse[T](
    request: ClaudeRequestMetadata,
    response: ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]
  ): Unit = {
    response match {
      case Invalid(e) => store.storeResponseError(request, e.toList)
      case Valid(r) => store.storeResponseSuccess(r)
    }
  }

  /** Concatenates the text of all `text` content blocks, skipping thinking / tool_use / tool_result blocks. */
  private def textContent(response: ClaudeResponse): String =
    response.content.flatMap(b => if (b.`type` == ClaudeContentType.Text) b.text else None).mkString("\n")

  private def parseText[T](rm: ClaudeRequestMetadata, response: ClaudeResponse)(implicit
    reads: Reads[T]
  ): ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]] = {
    textContent(response) match {
      case content if content.nonEmpty => parseContent[T](rm, response, content)
      case _ => noTextError(rm, response).invalidNec
    }
  }

  /** A response with no `text` content block -- almost always `stop_reason=max_tokens` where extended thinking consumed
    * the whole output budget before any answer was emitted, but also a refusal or a pure-thinking turn. Names
    * `stop_reason` + `output_tokens` in the message so the failure is self-diagnosing on the run page and in the
    * persisted audit error, instead of an opaque "no content".
    */
  private def noTextError(rm: ClaudeRequestMetadata, response: ClaudeResponse): ClaudeError =
    rm.error(
      s"No text content in response (stop_reason=${response.stopReason}, output_tokens=${response.usage.outputTokens})"
    )

  private def parseContent[T](rm: ClaudeRequestMetadata, response: ClaudeResponse, content: String)(implicit
    reads: Reads[T]
  ): ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]] = {
    def parseError(msg: String) = {
      rm.error(msg, raw = Some(textContent(response))).invalidNec
    }

    // With structured outputs, Claude returns clean JSON without markdown delimiters
    Try(Json.parse(content.trim)) match {
      case Failure(ex) => parseError(s"Content is not valid JSON: ${ex.getMessage}")
      case Success(js) =>
        js.validate[T] match {
          case JsSuccess(value, _) => ClaudeResponseMetadata(rm, response, value).validNec
          case JsError(errors) => {
            val messages = errors.flatMap(e => e._2.map(m => s"${e._1}: ${m.message}"))
            parseError(s"Content is not valid: ${messages.mkString(", ")}")
          }
        }
    }
  }
}

object ClaudeOutputFormats {
  def create(name: String, properties: JsObject, required: Seq[String]): ClaudeOutputFormat = {
    ClaudeOutputFormat(
      name = name,
      schema = Json.obj(
        "type" -> "object",
        "properties" -> properties,
        "required" -> required,
        "additionalProperties" -> false
      )
    )
  }

  private val stepsProperty = Json.obj(
    "type" -> "array",
    "items" -> Json.obj(
      "type" -> "object",
      "properties" -> Json.obj(
        "explanation" -> Json.obj("type" -> "string"),
        "output" -> Json.obj("type" -> "string")
      ),
      "required" -> Json.arr("explanation", "output"),
      "additionalProperties" -> false
    )
  )

  val CommentsResponse: ClaudeOutputFormat = ClaudeOutputFormats.create(
    "comments_response",
    Json.obj(
      "steps" -> stepsProperty,
      "comments" -> Json.obj(
        "type" -> "array",
        "items" -> Json.obj("type" -> "string")
      )
    ),
    Seq("steps", "comments")
  )

  val RecommendationsResponse: ClaudeOutputFormat = ClaudeOutputFormats.create(
    "recommendation_response",
    Json.obj(
      "steps" -> stepsProperty,
      "recommendations" -> Json.obj(
        "type" -> "array",
        "items" -> Json.obj(
          "type" -> "object",
          "properties" -> Json.obj(
            "category" -> Json.obj("type" -> "string"),
            "confidence" -> Json.obj(
              "type" -> "integer"
            )
          ),
          "required" -> Json.arr("category", "confidence"),
          "additionalProperties" -> false
        )
      )
    ),
    Seq("steps", "recommendations")
  )

  val SingleInsight: ClaudeOutputFormat = ClaudeOutputFormats.create(
    "single_insight_response",
    Json.obj(
      "steps" -> stepsProperty,
      "insight" -> Json.obj("type" -> "string")
    ),
    Seq("steps", "insight")
  )

  val InsightSectionsResponse: ClaudeOutputFormat = ClaudeOutputFormats.create(
    "insight_sections_response",
    Json.obj(
      "steps" -> stepsProperty,
      "sections" -> Json.obj(
        "type" -> "array",
        "items" -> Json.obj(
          "type" -> "object",
          "properties" -> Json.obj(
            "title" -> Json.obj("type" -> "string"),
            "icon" -> Json.obj("type" -> "string"),
            "items" -> Json.obj("type" -> "array", "items" -> Json.obj("type" -> "string")),
            "subsections" -> Json.obj(
              "type" -> "array",
              "items" -> Json.obj(
                "type" -> "object",
                "properties" -> Json.obj(
                  "heading" -> Json.obj("type" -> "string"),
                  "items" -> Json.obj("type" -> "array", "items" -> Json.obj("type" -> "string"))
                ),
                "required" -> Json.arr("heading", "items"),
                "additionalProperties" -> false
              )
            )
          ),
          "required" -> Json.arr("title", "icon"),
          "additionalProperties" -> false
        )
      )
    ),
    Seq("steps", "sections")
  )

  val all: List[ClaudeOutputFormat] =
    List(CommentsResponse, RecommendationsResponse, SingleInsight, InsightSectionsResponse)
}
