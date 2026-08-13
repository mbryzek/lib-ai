package com.bryzek.ai.claude

import cats.data.ValidatedNec
import cats.implicits.*
import com.bryzek.claude.client.IClient
import com.bryzek.claude.models.*
import com.bryzek.claude.models.json.*
import com.google.inject.ImplementedBy
import generated.errors.{ApiException, ClaudeErrorResponseResponse}
import play.api.libs.json.{Json, Reads}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.Try

/** Fetching a batch's results.
  *
  * Hand-written because it is the one batch operation the apibuilder spec cannot express: `GET
  * /v1/messages/batches/:id/results` answers `application/x-jsonl` -- one JSON object per line, not a JSON document --
  * and apibuilder has no response type for a newline-delimited stream. Everything else about batches IS in the spec and
  * IS generated; this is the exception, and it is deliberately the smallest possible one: it returns the raw lines and
  * lets [[ClaudeBatchClient]] parse them into the generated [[ClaudeBatchResult]], so the wire contract still lives in
  * `spec/claude.json` and only the transport is hand-rolled.
  */
trait ClaudeBatchResults {

  /** The results body's lines, in whatever order the API returned them (which is NOT submission order). */
  def fetchBatchResults(batch: ClaudeBatch, requestHeaders: Seq[(String, String)] = Nil): Future[Seq[String]]
}

/** One request to run in a batch.
  *
  * `customId` is the handle: results come back keyed by it and in arbitrary order, so it has to mean something to the
  * caller across the gap between submitting and reconciling -- which may be hours and a different process. Use an id
  * the caller can look up in its own registry (a stage-attempt id), never an array index.
  *
  * `model` is singular, unlike [[ClaudeClient]]'s `models: Seq[ClaudeModel]`: the client-side fallback chain works by
  * re-sending a failed request to the next model, and there is no re-sending inside a batch. A caller that wants
  * fallback resubmits the failed custom ids in a second batch. Same for [[ClaudeClient]]'s effort step-down.
  */
case class ClaudeBatchItem(
  customId: String,
  request: AiRequest,
  model: ClaudeModel,
  format: Option[ClaudeOutputFormat] = None
) {
  private[claude] def toRequestItem: ClaudeBatchRequestItem = {
    val base = request.toClaudeRequest(model)
    ClaudeBatchRequestItem(
      customId = customId,
      params = format.fold(base)(f =>
        base.copy(
          outputConfig = Some(
            base.outputConfig
              .getOrElse(ClaudeOutputConfig(effort = None, format = None, taskBudget = None))
              .copy(format = Some(f.toApi))
          )
        )
      )
    )
  }
}

/** What one `custom_id` came back as.
  *
  * Every terminal state other than `succeeded` is folded into the error channel, because from the caller's side there
  * is exactly one useful question -- did this request produce an answer -- and `canceled` / `expired` are answers of
  * "no" that a reconciler must handle identically to an API error. The distinction survives in the message text.
  */
case class ClaudeBatchOutcome(customId: String, response: ValidatedNec[ClaudeError, ClaudeResponse]) {

  private def err(msg: String, raw: Option[String]): ClaudeError =
    ClaudeError(message = s"$msg [custom_id: $customId]", raw = raw)

  /** The response's text, parsed as `T` -- byte for byte the rules [[ClaudeClient.chatCompletion]] applies, including
    * the error wordings the pipeline's budget-exhaustion recovery matches on.
    */
  def parse[T](implicit reads: Reads[T]): ValidatedNec[ClaudeError, T] =
    response.andThen(r => ClaudeContent.parse[T](err, r))

  /** The response's text, for a caller that asked for prose rather than a schema. */
  def text: ValidatedNec[ClaudeError, String] =
    response.andThen(r => ClaudeContent.requireText(err, r))
}

/** Hands out a [[ClaudeBatchClient]], the way [[ClaudeClientFactory]] hands out a [[ClaudeClient]].
  *
  * A SEPARATE factory rather than a method on that one, deliberately. Batching needs a transport that also implements
  * [[ClaudeBatchResults]], and widening `ClaudeClientFactory.getClient` to demand that would break every consumer that
  * supplies its own factory -- both of the two that do (platform's rallyd subproject and acumen) route Claude through
  * another service and cannot run a batch at all. There is no reason to make a repo that will never batch implement a
  * batch transport to keep compiling, and every reason for "can this run batches" to be a type a caller opts into.
  */
@ImplementedBy(classOf[ClaudeBatchClientFactoryImpl])
trait ClaudeBatchClientFactory {
  final def instance(env: ClaudeEnvironment, apiKey: String): ClaudeBatchClient =
    ClaudeBatchClient(getBatchClient(env), ClaudeConfig(apiKey))

  def getBatchClient(env: ClaudeEnvironment): IClient & ClaudeBatchResults
}

class ClaudeBatchClientFactoryImpl @Inject() (
  productionClaudeClient: ProductionClaudeClient,
  testClaudeClient: TestClaudeClient
) extends ClaudeBatchClientFactory {
  override def getBatchClient(env: ClaudeEnvironment): IClient & ClaudeBatchResults = {
    env match {
      case ClaudeEnvironment.Production => productionClaudeClient
      case ClaudeEnvironment.Sandbox => testClaudeClient
    }
  }
}

object ClaudeBatchClient {

  /** The API's own ceiling: a batch takes at most this many requests (or 256 MB, whichever binds first). Checked here
    * so an oversized batch fails before it is serialized and uploaded.
    */
  val MaxRequestsPerBatch: Int = 100000

  /** Turns one JSONL line's parsed result into an outcome. `succeeded` is the only type carrying a message; the other
    * three are matched exhaustively (never a wildcard) so that a result type added to the API surfaces as an explicit
    * unrecognized-value error rather than being silently swallowed by a catch-all arm.
    */
  private[claude] def toOutcome(result: ClaudeBatchResult): ClaudeBatchOutcome = {
    def error(msg: String): ValidatedNec[ClaudeError, ClaudeResponse] =
      ClaudeError(message = s"$msg [custom_id: ${result.customId}]").invalidNec

    val response = KnownClaudeBatchResultType.validate(result.result.`type`).toOption match {
      case Some(KnownClaudeBatchResultType.Succeeded) =>
        result.result.message
          .map(_.validNec)
          .getOrElse(error("Batch result is succeeded but carries no message"))
      case Some(KnownClaudeBatchResultType.Errored) =>
        error(
          result.result.error
            .map(e => s"Batch request failed: ${e.error.message}")
            .getOrElse("Batch request failed with no error detail")
        )
      case Some(KnownClaudeBatchResultType.Canceled) =>
        error("Batch request was canceled before it ran")
      case Some(KnownClaudeBatchResultType.Expired) =>
        error("Batch request expired: the batch hit its 24-hour ceiling before this request ran")
      case None =>
        error(s"Unrecognized batch result type '${result.result.`type`}'")
    }
    ClaudeBatchOutcome(result.customId, response)
  }
}

/** The Message Batches API: the same Messages surface at 50% of the token price, run asynchronously.
  *
  * ==What this is for, and what it is not for==
  *
  * A batch trades latency for money. Requests are submitted together, run whenever the API gets to them -- usually
  * within an hour, with a hard 24-hour ceiling -- and the results are collected afterwards. So it fits work that is
  * scheduled and wide (a nightly fan-out across clubs, a backfill, a re-scoring sweep) and does not fit anything with
  * somebody waiting on it. There is no partial delivery: results become readable only once the whole batch has ended.
  *
  * ==How it differs from [[ClaudeClient]], and why==
  *
  * Three of that client's recoveries cannot exist here, and this class does not pretend otherwise:
  *
  *   - '''No model fallback.''' [[ClaudeClient]] re-sends a 529/404/refusal to the next model in the chain. A batched
  *     request names one model and is run once. Resubmit the failed custom ids in a second batch.
  *   - '''No effort step-down.''' Same reason: [[ClaudeClient.withEffortStepDown]] works by re-sending.
  *   - '''No streaming idle-timeout signal.''' A batch is not a connection, so the question
  *     [[ClaudeStreamingClient.IdleTimeout]] answers ("is anything still coming?") has no analogue. What replaces it is
  *     `expires_at`, 24 hours out, which is a far blunter instrument -- a caller must be able to survive a whole batch
  *     coming back `expired`.
  *
  * The retry policy on the batch operations themselves ([[ClaudeRetries]]) and the response parsing ([[ClaudeContent]])
  * are shared with the synchronous path, so a request answers identically whichever carried it.
  *
  * ==Journaling==
  *
  * Unlike [[ClaudeClient]] this takes no [[ClaudeStore]], and that is deliberate rather than an omission. A journal
  * entry pairs a request with its response; a batch separates those by hours and, in general, by process, so nothing in
  * this class is alive to write the second half. The caller already owns a registry mapping custom id to whatever
  * submitted it -- that registry is what spans the gap, so the caller writes both halves from it, using the `usage` on
  * each outcome's response. Note that usage carries RAW token counts with `service_tier = batch`; see
  * [[ClaudeServiceTier]] before pricing them.
  */
case class ClaudeBatchClient(
  client: IClient & ClaudeBatchResults,
  config: ClaudeConfig
) {

  /** The beta headers are carried onto batches too, and that is load-bearing rather than incidental: structured outputs
    * and task budgets are both gated behind one, and a batch that omits them has its `output_config` rejected
    * -- hours later, as a whole batch of errored results, rather than immediately.
    */
  private val defaultHeaders: Seq[(String, String)] = config.headers

  /** Submits `items` as one batch. Verified against the live API on 2026-08-11: structured outputs
    * (`output_config.format`), task budgets and 1-hour cache control are all accepted and honored inside a batch, so an
    * [[AiRequest]] does not have to be reshaped to be batched.
    *
    * Duplicate custom ids are rejected here rather than at the API. They are not a wire error -- the API accepts them
    * -- they are a reconciliation error: results are keyed by custom id, so two requests sharing one make it impossible
    * to say which result belongs to which caller, and the damage shows up hours later at reconcile time rather than
    * here.
    */
  def submit(
    items: Seq[ClaudeBatchItem]
  )(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, ClaudeBatch]] = {
    validate(items) match {
      case Some(error) => Future.successful(error.invalidNec)
      case None =>
        call(client.createClaudeBatch(ClaudeBatchForm(requests = items.map(_.toRequestItem)), defaultHeaders))
    }
  }

  /** The batch's current state. Poll this until `processingStatus` is `ended`; there is no completion callback and no
    * partial read.
    */
  def status(batchId: String)(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, ClaudeBatch]] =
    call(client.getClaudeBatchById(batchId, defaultHeaders))

  /** Asks the API to stop running the batch. Best-effort: requests already in flight finish, so the batch generally
    * ends with a mix of `succeeded` and `canceled` results. Cancellation is not instant either -- the batch moves to
    * `canceling` first and reaches `ended` on its own.
    */
  def cancel(batchId: String)(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, ClaudeBatch]] =
    call(client.cancelClaudeBatchById(batchId, defaultHeaders))

  /** Every request's outcome, keyed by custom id.
    *
    * Refuses a batch that has not ended rather than returning an empty list: `results_url` is null until then, and a
    * caller that read "no results" as "nothing succeeded" would discard a batch that is merely still running.
    *
    * A line that will not parse fails the whole call. That is safe precisely because this fetch is idempotent and the
    * results body stays retrievable for 29 days -- nothing is re-run by asking again -- so failing loudly on a payload
    * we do not understand costs a retry, where skipping the line would silently drop a club's answer.
    */
  def results(
    batch: ClaudeBatch
  )(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, Seq[ClaudeBatchOutcome]]] = {
    KnownClaudeBatchProcessingStatus.validate(batch.processingStatus).toOption match {
      case Some(KnownClaudeBatchProcessingStatus.Ended) =>
        ClaudeRetries
          .withRetries(client.fetchBatchResults(batch, defaultHeaders))
          .map(parseLines)
          .recover(toError)
      case Some(KnownClaudeBatchProcessingStatus.InProgress | KnownClaudeBatchProcessingStatus.Canceling) | None =>
        Future.successful(
          ClaudeError(
            message =
              s"Batch ${batch.id} has not ended (processing_status=${batch.processingStatus}); its results are not readable yet"
          ).invalidNec
        )
    }
  }

  private def validate(items: Seq[ClaudeBatchItem]): Option[ClaudeError] = {
    val duplicates = items.groupBy(_.customId).collect { case (id, all) if all.size > 1 => id }.toSeq.sorted
    if (items.isEmpty) {
      Some(ClaudeError(message = "Cannot submit an empty batch"))
    } else if (duplicates.nonEmpty) {
      Some(ClaudeError(message = s"Duplicate custom ids in batch: ${duplicates.mkString(", ")}"))
    } else if (items.size > ClaudeBatchClient.MaxRequestsPerBatch) {
      Some(
        ClaudeError(message =
          s"Batch of ${items.size} requests exceeds the API limit of ${ClaudeBatchClient.MaxRequestsPerBatch}"
        )
      )
    } else {
      None
    }
  }

  private def parseLines(lines: Seq[String]): ValidatedNec[ClaudeError, Seq[ClaudeBatchOutcome]] =
    lines.zipWithIndex
      .filter(_._1.trim.nonEmpty)
      .toList
      .traverse { case (line, index) =>
        Try(Json.parse(line)).toOption
          .flatMap(_.asOpt[ClaudeBatchResult])
          .map(r => ClaudeBatchClient.toOutcome(r).validNec)
          .getOrElse(
            ClaudeError(message = s"Could not parse batch result line ${index + 1}", raw = Some(line)).invalidNec
          )
      }

  private def call[T](
    attempt: => Future[T]
  )(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, T]] =
    ClaudeRetries.withRetries(attempt).map(_.validNec).recover(toError)

  /** A non-2xx body parsed into a [[ClaudeError]]; a malformed body (an HTML error page from a proxy, say) throws
    * inside the generated wrapper, so fall back to the status rather than letting it escape the Validated channel.
    *
    * Names Anthropic's own `request-id` when the response carried one -- a batch operation has no per-request
    * correlation id of this library's own (the handle is `custom_id`), so it is the only id there is to quote.
    */
  private def toError[T]: PartialFunction[Throwable, ValidatedNec[ClaudeError, T]] = {
    case r: ClaudeErrorResponseResponse =>
      val message = Try(r.claudeErrorResponse.error.message).getOrElse(s"HTTP ${r.response.status}: ${r.getMessage}")
      ClaudeError(message = message + ClaudeErrorLabels.providerSuffix(r.response.headers)).invalidNec
    case a: ApiException => ClaudeError(message = a.getMessage).invalidNec
    case NonFatal(e) => ClaudeError(message = e.getMessage).invalidNec
  }
}
