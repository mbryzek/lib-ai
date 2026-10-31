package com.bryzek.ai.claude

import cats.data.ValidatedNec
import cats.implicits.*
import com.bryzek.ai.{AiFeature, AiModelTrial, AiModels, NoAiModels}
import com.bryzek.claude.client.IClient
import com.bryzek.claude.models.*
import com.bryzek.claude.models.json.*
import com.google.inject.ImplementedBy
import generated.errors.{ApiException, ClaudeErrorResponseResponse}
import play.api.libs.json.{Json, Reads}

import java.util.UUID
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
  *
  * `feature` is the [[AiFeature]] the request is for. With one, [[ClaudeBatchClient.submitResolved]] sends the feature's
  * model in force in `model`'s family ([[AiModels.resolve]]) and [[ClaudeBatchClient.trialItems]] samples its results
  * for the feature's open trials; without one, `model` goes out as named and nothing is sampled, exactly as before
  * items named a feature.
  */
case class ClaudeBatchItem(
  customId: String,
  request: AiRequest,
  model: ClaudeModel,
  format: Option[ClaudeOutputFormat] = None,
  feature: Option[AiFeature] = None
) {

  /** The Messages request this item runs: `request` for `model`, asking for `format` when there is one. */
  def toClaudeRequest: ClaudeRequest = {
    val base = request.toClaudeRequest(model)
    format.fold(base)(f =>
      base.copy(
        outputConfig = Some(
          base.outputConfig
            .getOrElse(ClaudeOutputConfig(effort = None, format = None, taskBudget = None))
            .copy(format = Some(f.toApi))
        )
      )
    )
  }

  private[claude] def toRequestItem: ClaudeBatchRequestItem =
    ClaudeBatchRequestItem(customId = customId, params = toClaudeRequest)
}

/** A submitted batch and its items as they were sent: each item's `model` is the model it RESOLVED to, which is the one
  * to record against the custom id and to journal the result under.
  */
case class ClaudeBatchSubmission(batch: ClaudeBatch, items: Seq[ClaudeBatchItem])

/** One sampled batch result of a feature with an open trial: `used` is the item as submitted, `answer` the response it
  * succeeded with, and `rerun` the same request for the trial's new model, charged to the trial's own feature, to be
  * submitted in the trial's own batch. When `rerun`'s outcome comes back, [[pair]] makes the pair the app's judge reads.
  */
case class ClaudeBatchTrialItem(
  trial: AiModelTrial,
  feature: AiFeature,
  used: ClaudeBatchItem,
  answer: ClaudeResponse,
  rerun: ClaudeBatchItem
) {

  /** The pair, from `outcome` -- the result of submitting [[rerun]]. An outcome that did not succeed makes no pair, and
    * neither does one for another custom id. Both answers are priced from their own usage, so at batch rates.
    */
  def pair(outcome: ClaudeBatchOutcome): ValidatedNec[ClaudeError, ClaudeTrialPair] =
    if (outcome.customId != rerun.customId) {
      ClaudeError(
        message = s"Batch outcome[${outcome.customId}] is not trial[${trial.id}]'s re-run[${rerun.customId}]"
      ).invalidNec
    } else {
      outcome.response.map { candidate =>
        ClaudeTrialPair(
          trial = trial,
          feature = feature,
          context = None,
          usedId = used.customId,
          request = used.toClaudeRequest,
          used = answer,
          usedCostUsd = ClaudeResponseMetadata.costOf(used.model, answer),
          candidateId = rerun.customId,
          candidateRequest = rerun.toClaudeRequest,
          candidate = candidate,
          candidateCostUsd = ClaudeResponseMetadata.costOf(rerun.model, candidate)
        )
      }
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
    instance(env, apiKey, NoAiModels)

  /** A client resolving each item's model through `models`, and sampling for its trials. */
  final def instance(env: ClaudeEnvironment, apiKey: String, models: AiModels): ClaudeBatchClient =
    ClaudeBatchClient(getBatchClient(env), ClaudeConfig(apiKey), models)

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

  /** The custom id a trial re-run is submitted under: fresh, because it is a request of its own in a batch of its own,
    * and inside the API's 64-character `[a-zA-Z0-9_-]` rule.
    */
  private[claude] def trialCustomId(): String = "trial_" + UUID.randomUUID().toString.replaceAll("-", "")

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
  config: ClaudeConfig,
  models: AiModels = NoAiModels
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
  )(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, ClaudeBatch]] =
    submitResolved(items).map(_.map(_.batch))

  /** [[submit]], answering with the items as they were sent: each item naming a feature is resolved to that feature's
    * model in force when the batch is submitted, so a move or an Undo reaches batched traffic the next time it is
    * submitted. Record the returned items, not the ones passed in -- their `model` is the one that runs.
    */
  def submitResolved(
    items: Seq[ClaudeBatchItem]
  )(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, ClaudeBatchSubmission]] = {
    validate(items) match {
      case Some(error) => Future.successful(error.invalidNec)
      case None =>
        val resolved = items.map(resolve)
        call(client.createClaudeBatch(ClaudeBatchForm(requests = resolved.map(_.toRequestItem)), defaultHeaders))
          .map(_.map(ClaudeBatchSubmission(_, resolved)))
    }
  }

  /** `item` on its feature's model in force; an item naming no feature as it stands. */
  def resolve(item: ClaudeBatchItem): ClaudeBatchItem =
    item.feature.fold(item)(f => item.copy(model = models.resolve(f, item.model)))

  /** The trial re-runs owed by a reconciled batch: for each succeeded outcome of an item whose feature has an open trial
    * on the model it ran on, one of that trial's daily samples is claimed ([[AiModels.claimTrialSample]], the same five
    * a day the synchronous client counts against) and the item comes back as a [[ClaudeBatchTrialItem]] whose `rerun`
    * the caller submits in the trial's own batch.
    *
    * `submitted` are the items as [[submitResolved]] returned them, so each `model` is the one that answered. An outcome
    * for a custom id not among them, or one that did not succeed, is never sampled.
    */
  def trialItems(submitted: Seq[ClaudeBatchItem], outcomes: Seq[ClaudeBatchOutcome]): Seq[ClaudeBatchTrialItem] = {
    val byId = submitted.map(i => i.customId -> i).toMap
    outcomes.flatMap { outcome =>
      for {
        answer <- outcome.response.toOption.toSeq
        item <- byId.get(outcome.customId).toSeq
        feature <- item.feature.toSeq
        trial <- models.claimTrialSample(feature, item.model).toSeq
      } yield ClaudeBatchTrialItem(
        trial = trial,
        feature = feature,
        used = item,
        answer = answer,
        rerun = item.copy(
          customId = ClaudeBatchClient.trialCustomId(),
          model = trial.toModel,
          feature = Some(trial.chargedTo)
        )
      )
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
          .withRetries(config.backoff)(client.fetchBatchResults(batch, defaultHeaders))
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
    ClaudeRetries.withRetries(config.backoff)(attempt).map(_.validNec).recover(toError)

  private def toError[T]: PartialFunction[Throwable, ValidatedNec[ClaudeError, T]] = ClaudeApiErrors.toError
}

/** How a failed call that is not a message becomes a [[ClaudeError]] -- a batch operation, a model-list read. */
private[claude] object ClaudeApiErrors {

  /** A non-2xx body parsed into a [[ClaudeError]]; a malformed body (an HTML error page from a proxy, say) throws
    * inside the generated wrapper, so fall back to the status rather than letting it escape the Validated channel.
    *
    * Names Anthropic's own `request-id` when the response carried one -- these operations have no per-request
    * correlation id of this library's own (a batch's handle is `custom_id`), so it is the only id there is to quote.
    */
  def toError[T]: PartialFunction[Throwable, ValidatedNec[ClaudeError, T]] = {
    case r: ClaudeErrorResponseResponse =>
      val message = Try(r.claudeErrorResponse.error.message).getOrElse(s"HTTP ${r.response.status}: ${r.getMessage}")
      ClaudeError(message = message + ClaudeErrorLabels.providerSuffix(r.response.headers)).invalidNec
    case a: ApiException => ClaudeError(message = a.getMessage).invalidNec
    case NonFatal(e) => ClaudeError(message = e.getMessage).invalidNec
  }
}
