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

  def apply(key: String): ClaudeConfig = ClaudeConfig(
    key = key,
    anthropicVersion = Version,
    betaHeaders = Seq(StructuredOutputsBeta)
  )
}

sealed trait ClaudeEnvironment
object ClaudeEnvironment {
  case object Sandbox extends ClaudeEnvironment
  case object Production extends ClaudeEnvironment
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
  effort: Option[ClaudeEffort] = None
) {
  require(!cacheSystem || system.isDefined, "cacheSystem=true requires system to be set")

  def toClaudeRequest(model: ClaudeModel): ClaudeRequest = {
    def ephemeral: Option[ClaudeCacheControl] = Some(ClaudeCacheControl())
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
      outputConfig = effort.map(e => ClaudeOutputConfig(effort = Some(e), format = None)),
      thinking = Some(ClaudeThinking(thinking))
    )
  }
}

case class ClaudeRequestMetadata(
  client: IClient,
  id: String,
  request: ClaudeRequest,
  context: Option[String] = None
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

  def chatComments(request: AiRequest, models: Seq[ClaudeModel], context: Option[String] = None)(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[String]]] = {
    chatCompletion[CommentsResponse](request, ClaudeOutputFormats.CommentsResponse, models, context)(using ec)
      .map(_.map(_.content.comments))
  }

  def chatRecommendations(request: AiRequest, models: Seq[ClaudeModel], context: Option[String] = None)(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[Recommendation]]] = {
    chatCompletion[RecommendationResponse](request, ClaudeOutputFormats.RecommendationsResponse, models, context)(using
      ec
    )
      .map(_.map(_.content.recommendations))
  }

  def chatInsight(request: AiRequest, models: Seq[ClaudeModel], context: Option[String] = None)(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[String]]] = {
    chatComments(request, models, context)(using ec)
  }

  def chatSingleInsight(request: AiRequest, models: Seq[ClaudeModel], context: Option[String] = None)(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, String]] = {
    chatCompletion[SingleInsightResponse](request, ClaudeOutputFormats.SingleInsight, models, context)(using ec)
      .map(_.map(_.content.insight))
  }

  def chatInsightSections(request: AiRequest, models: Seq[ClaudeModel], context: Option[String] = None)(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, Seq[InsightSection]]] = {
    chatCompletion[InsightSectionsResponse](request, ClaudeOutputFormats.InsightSectionsResponse, models, context)(using
      ec
    )
      .map(_.map(_.content.sections))
  }

  def chatText(request: AiRequest, models: Seq[ClaudeModel], context: Option[String] = None)(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[String]]] = {
    tryModels(models) { model =>
      chatTextSingle(request.toClaudeRequest(model), context)
    }
  }

  def chatCompletion[T](
    request: AiRequest,
    outputFormat: ClaudeOutputFormat,
    models: Seq[ClaudeModel],
    context: Option[String] = None
  )(implicit
    ec: ExecutionContext,
    reads: Reads[T]
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]] = {
    tryModels(models) { model =>
      chatCompletionSingle(request.toClaudeRequest(model), outputFormat, context)
    }
  }

  /** Agentic tool loop. Sends `request` with `tools` and lets the model call them; every `tool_use` block is executed
    * via `execute`, and all results for a turn are returned in ONE user message (splitting degrades parallel tool
    * calling). Budget-capped at `maxCalls` tool executions; a run of [[ClaudeClient.MaxConsecutiveFailedTurns]]
    * consecutive fully-failed tool turns aborts. The final turn forces `tool_choice: none` + structured output so the
    * answer parses into `T` without regex-and-nudge. Every request/response is journaled via the store with `context`.
    */
  def runToolLoop[T](
    request: AiRequest,
    tools: Seq[ClaudeTool],
    models: Seq[ClaudeModel],
    maxCalls: Int,
    finalFormat: ClaudeOutputFormat,
    context: Option[String] = None
  )(execute: ClaudeToolUse => Future[ClaudeToolOutput])(implicit
    ec: ExecutionContext,
    reads: Reads[T]
  ): Future[ValidatedNec[ClaudeError, ClaudeToolLoopResult[T]]] = {
    tryModels(models) { model =>
      val base = request.toClaudeRequest(model)

      def finalize(
        messages: Seq[ClaudeMessage],
        invocations: Seq[ClaudeToolInvocation],
        turns: Int
      ): Future[ValidatedNec[ClaudeError, ClaudeToolLoopResult[T]]] = {
        val req = base.copy(
          messages = messages,
          tools = None,
          toolChoice = Some(ClaudeToolChoice(ClaudeToolChoiceType.None)),
          outputConfig = Some(mergeFormat(base.outputConfig, finalFormat))
        )
        sendAndStore[T](req, structuredHeaders(finalFormat), context)((rm, resp) => parseText[T](rm, resp))
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
            messages = messages,
            tools = Some(tools),
            toolChoice = Some(ClaudeToolChoice(ClaudeToolChoiceType.Auto)),
            outputConfig = base.outputConfig
          )
          sendRaw(req, defaultHeaders, context).flatMap {
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
                // Model stopped asking for tools; force a structured final answer. The finalize request
                // must end in a user turn — a trailing assistant turn is an assistant prefill, which modern
                // models reject with a 400 — so echo the model's turn and add an explicit user instruction.
                val done = messages :+
                  ClaudeMessage(ClaudeRole.Assistant, rm.response.content) :+
                  finalizeInstruction
                finalize(done, invocations, turns)
              }
          }
        }
      }

      loop(base.messages, 0, 0, Nil, 0)
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
    existing.getOrElse(ClaudeOutputConfig(effort = None, format = None)).copy(format = Some(format.toApi))

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

  /** Retry the given HTTP attempt, honoring `Retry-After` on 429 and using jittered backoff on 5xx and transient
    * transport failures (read/request timeouts, connection resets). Non-retryable failures (or exhausted attempts)
    * propagate the original exception.
    */
  private def withRetries(attempt: => Future[ClaudeResponse])(implicit ec: ExecutionContext): Future[ClaudeResponse] = {
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
  private def send(request: ClaudeRequest, headers: Seq[(String, String)], context: Option[String])(implicit
    ec: ExecutionContext
  ): Future[(ClaudeRequestMetadata, ValidatedNec[ClaudeError, ClaudeResponse])] = {
    val rm = ClaudeRequestMetadata(client, randomId("req"), request, context)
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

  private def sendAndStore[T](request: ClaudeRequest, headers: Seq[(String, String)], context: Option[String])(
    parse: (ClaudeRequestMetadata, ClaudeResponse) => ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]
  )(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]] = {
    send(request, headers, context).map { case (rm, envelope) =>
      val result = envelope.andThen(resp => parse(rm, resp))
      storeResponse(rm, result)
      result
    }
  }

  /** Send that persists (and returns) the raw response — used for the intermediate tool-loop turns. */
  private def sendRaw(request: ClaudeRequest, headers: Seq[(String, String)], context: Option[String])(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[ClaudeResponse]]] =
    sendAndStore(request, headers, context)((rm, resp) => ClaudeResponseMetadata(rm, resp, resp).validNec)

  private def chatCompletionSingle[T](
    originalRequest: ClaudeRequest,
    outputFormat: ClaudeOutputFormat,
    context: Option[String]
  )(implicit
    ec: ExecutionContext,
    reads: Reads[T]
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[T]]] = {
    val request = originalRequest.copy(
      outputConfig = Some(mergeFormat(originalRequest.outputConfig, outputFormat))
    )
    sendAndStore[T](request, structuredHeaders(outputFormat), context)((rm, resp) => parseText[T](rm, resp))
  }

  private def chatTextSingle(originalRequest: ClaudeRequest, context: Option[String])(implicit
    ec: ExecutionContext
  ): Future[ValidatedNec[ClaudeError, ClaudeResponseMetadata[String]]] = {
    sendAndStore[String](originalRequest, defaultHeaders, context) { (rm, resp) =>
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
