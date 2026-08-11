package com.bryzek.ai.claude

import com.bryzek.claude.models.{
  ClaudeBatch,
  ClaudeBatchProcessingStatus,
  ClaudeBatchRequestCounts,
  ClaudeBatchResultType,
  ClaudeModel,
  ClaudeRole,
  ClaudeServiceTier,
  ClaudeTaskBudgetType
}
import com.bryzek.claude.response.models.SingleInsightResponse
import com.bryzek.claude.response.models.json.*
import helpers.FutureHelpers
import org.apache.pekko.util.Timeout
import org.joda.time.DateTime
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, SECONDS}

class ClaudeBatchClientSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with FutureHelpers {

  private implicit val timeout: Timeout = FiniteDuration(30, SECONDS)

  private def client(transport: TestClaudeClient = new TestClaudeClient()): ClaudeBatchClient =
    ClaudeBatchClient(transport, ClaudeConfig("test-api-key"))

  private def item(customId: String, text: String = "hello", format: Option[ClaudeOutputFormat] = None) =
    ClaudeBatchItem(
      customId = customId,
      request = AiRequest(messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, text))),
      model = ClaudeModel.ClaudeSonnet5,
      format = format
    )

  private def endedBatch(id: String = "msgbatch_test_1"): ClaudeBatch = {
    val now = DateTime.now()
    ClaudeBatch(
      id = id,
      processingStatus = ClaudeBatchProcessingStatus.Ended,
      requestCounts = ClaudeBatchRequestCounts(processing = 0, succeeded = 1, errored = 0, canceled = 0, expired = 0),
      createdAt = now,
      expiresAt = now.plusHours(24)
    ).copy(endedAt = Some(now))
  }

  /** A transport whose results body is exactly `lines`, for driving the JSONL parse from captured wire bytes. */
  private def clientReturning(lines: Seq[String]): ClaudeBatchClient = {
    val transport = new TestClaudeClient {
      override def fetchBatchResults(batch: ClaudeBatch, requestHeaders: Seq[(String, String)]): Future[Seq[String]] =
        Future.successful(lines)
    }
    client(transport)
  }

  "ClaudeBatchClientFactory" should {

    "resolve through Guice and hand back a working sandbox client" in {
      val fromFactory = app.injector
        .instanceOf[ClaudeBatchClientFactory]
        .instance(ClaudeEnvironment.Sandbox, "test-api-key")
      await(fromFactory.submit(Seq(item("alpha")))).toOption.get.requestCounts.succeeded mustBe 1L
    }
  }

  "submit" should {

    "accept every request in the batch" in {
      val batch = await(client().submit(Seq(item("alpha"), item("beta"), item("gamma")))).toOption.get
      batch.processingStatus mustBe ClaudeBatchProcessingStatus.Ended
      batch.requestCounts.succeeded mustBe 3L
      batch.requestCounts.errored mustBe 0L
    }

    "reject a batch with duplicate custom ids" in {
      // Not a wire error -- the API accepts duplicates -- but a reconciliation one: results are keyed by custom id,
      // so two requests sharing one cannot be told apart hours later.
      val result = await(client().submit(Seq(item("dup"), item("dup"), item("other"))))
      result.isInvalid mustBe true
      result.swap.toOption.get.toNonEmptyList.head.message must include("Duplicate custom ids in batch: dup")
    }

    "reject an empty batch" in {
      val result = await(client().submit(Nil))
      result.isInvalid mustBe true
      result.swap.toOption.get.toNonEmptyList.head.message must include("empty batch")
    }

    "carry the caller's structured output format and task budget into the request params" in {
      val params = item("alpha", format = Some(ClaudeOutputFormats.SingleInsight)).toRequestItem.params
      params.outputConfig.flatMap(_.format).map(_.schema) mustBe Some(ClaudeOutputFormats.SingleInsight.toApi.schema)
      // Everything AiRequest normally puts on a request survives being batched -- verified against the live API on
      // 2026-08-11, which accepted and honored all three inside a batch.
      params.outputConfig.flatMap(_.taskBudget).map(_.`type`) mustBe Some(ClaudeTaskBudgetType.Tokens)
      params.thinking.map(_.`type`) mustBe Some(com.bryzek.claude.models.ClaudeThinkingType.Adaptive)
    }
  }

  "a submit / poll / reconcile round trip" should {

    "key every result by custom id even though they arrive in a different order" in {
      val transport = new TestClaudeClient()
      val batchClient = client(transport)
      val submitted = Seq(item("alpha"), item("beta"), item("gamma"))

      val batch = await(batchClient.submit(submitted)).toOption.get
      val polled = await(batchClient.status(batch.id)).toOption.get
      polled.processingStatus mustBe ClaudeBatchProcessingStatus.Ended

      val outcomes = await(batchClient.results(polled)).toOption.get
      outcomes.map(_.customId).sorted mustBe Seq("alpha", "beta", "gamma")
      // The point of the test: results do NOT come back in submission order, so anything keying by position is wrong.
      outcomes.map(_.customId) must not be submitted.map(_.customId)
      outcomes.foreach(_.response.isValid mustBe true)
    }

    "parse a structured answer with the same rules as the synchronous client" in {
      val transport = new TestClaudeClient()
      val batchClient = client(transport)
      val batch =
        await(batchClient.submit(Seq(item("alpha", format = Some(ClaudeOutputFormats.SingleInsight))))).toOption.get

      val outcome = await(batchClient.results(batch)).toOption.get.head
      outcome.parse[SingleInsightResponse].toOption.map(_.insight) mustBe Some("You are doing amazing")
    }

    "report a batched response's usage as service_tier = batch" in {
      // Load-bearing for cost: a batch reports RAW token counts, so anything pricing them without reading this
      // over-reports by 2x -- which would feed both the usage rollup and the weekly spend breaker.
      val transport = new TestClaudeClient()
      val batchClient = client(transport)
      val batch = await(batchClient.submit(Seq(item("alpha")))).toOption.get

      val outcome = await(batchClient.results(batch)).toOption.get.head
      outcome.response.toOption.get.usage.serviceTier mustBe Some(ClaudeServiceTier.Batch)
    }

    "surface a request the API rejected as a failed outcome, leaving its siblings intact" in {
      val transport = new TestClaudeClient()
      val batchClient = client(transport)
      // An empty message list is rejected per-request at execution time, not at submit -- verified against the live
      // API on 2026-08-11, which accepted this batch and then errored exactly this one request.
      val broken = ClaudeBatchItem(
        customId = "broken",
        request = AiRequest(messages = Nil),
        model = ClaudeModel.ClaudeSonnet5
      )
      val batch = await(batchClient.submit(Seq(item("alpha"), broken))).toOption.get
      batch.requestCounts.succeeded mustBe 1L
      batch.requestCounts.errored mustBe 1L

      val byId = await(batchClient.results(batch)).toOption.get.map(o => o.customId -> o).toMap
      byId("alpha").response.isValid mustBe true
      byId("broken").response.isInvalid mustBe true
      byId("broken").response.swap.toOption.get.toNonEmptyList.head.message must include("custom_id: broken")
    }

    "cancel a batch" in {
      val transport = new TestClaudeClient()
      val batchClient = client(transport)
      val batch = await(batchClient.submit(Seq(item("alpha")))).toOption.get

      await(batchClient.cancel(batch.id)).toOption.get.cancelInitiatedAt must not be None
    }

    "report an unknown batch id as an error rather than an empty result" in {
      await(client().status("msgbatch_nope")).isInvalid mustBe true
    }
  }

  "results" should {

    "refuse a batch that has not ended" in {
      // results_url is null until then, so returning an empty list would read as "nothing succeeded" for a batch
      // that is merely still running.
      val running = endedBatch().copy(processingStatus = ClaudeBatchProcessingStatus.InProgress)
      val result = await(client().results(running))
      result.isInvalid mustBe true
      result.swap.toOption.get.toNonEmptyList.head.message must include("has not ended")
    }

    "parse the wire format the live API actually returns" in {
      // Captured verbatim from api.anthropic.com on 2026-08-11 (a 3-request batch, one of them invalid). Two things
      // in here are not obvious from the docs and are exactly what a hand-rolled parser gets wrong: results come back
      // in an order unrelated to submission (submitted alpha, beta, bad -- returned bad, beta, alpha), and an errored
      // result's `error` is a DOUBLY nested envelope, {"type":"error","error":{...}}.
      val outcomes = await(clientReturning(ClaudeBatchClientSpec.LiveResults).results(endedBatch())).toOption.get

      outcomes.map(_.customId) mustBe Seq("probe-bad", "probe-beta", "probe-alpha")

      val byId = outcomes.map(o => o.customId -> o).toMap
      byId("probe-alpha").text.toOption mustBe Some("ALPHA")
      byId("probe-beta").text.toOption mustBe Some("BETA")
      byId("probe-alpha").response.toOption.get.usage.serviceTier mustBe Some(ClaudeServiceTier.Batch)

      val failure = byId("probe-bad").response.swap.toOption.get.toNonEmptyList.head.message
      failure must include("messages: at least one message is required")
      failure must include("custom_id: probe-bad")
    }

    "ignore blank lines in the results body" in {
      val outcomes =
        await(clientReturning(ClaudeBatchClientSpec.LiveResults :+ "").results(endedBatch())).toOption.get
      outcomes.size mustBe 3
    }

    "fail the whole fetch on a line it cannot parse" in {
      // Rather than skipping it. Safe because fetching results is idempotent and the body stays retrievable for 29
      // days, so failing costs a retry -- where skipping would silently drop one club's answer.
      val result =
        await(clientReturning(Seq(ClaudeBatchClientSpec.LiveResults.head, "{ not json")).results(endedBatch()))
      result.isInvalid mustBe true
      result.swap.toOption.get.toNonEmptyList.head.message must include("Could not parse batch result line 2")
    }
  }

  "toOutcome" should {

    "turn every non-succeeded terminal state into a named error" in {
      def messageFor(resultType: ClaudeBatchResultType): String = {
        val result = com.bryzek.claude.models.ClaudeBatchResult(
          customId = "alpha",
          result = com.bryzek.claude.models.ClaudeBatchResultDetail(resultType)
        )
        ClaudeBatchClient.toOutcome(result).response.swap.toOption.get.toNonEmptyList.head.message
      }

      messageFor(ClaudeBatchResultType.Canceled) must include("canceled")
      // The one a caller is most likely to forget: a request that never ran inside the 24-hour ceiling.
      messageFor(ClaudeBatchResultType.Expired) must include("24-hour ceiling")
      messageFor(ClaudeBatchResultType.Succeeded) must include("carries no message")
      messageFor(ClaudeBatchResultType.UNDEFINED("something_new")) must include("Unrecognized batch result type")
    }
  }
}

object ClaudeBatchClientSpec {

  /** Three JSONL lines captured verbatim from `GET /v1/messages/batches/:id/results` against api.anthropic.com on
    * 2026-08-11. A fixture rather than something hand-written: every field this library reads off a batch result -- the
    * `result.type` discriminator, the nested error envelope, `service_tier` -- is here as the API really sends it, so a
    * spec change that stops matching the wire fails here instead of in production.
    */
  val LiveResults: Seq[String] = Seq(
    """{"custom_id":"probe-bad","result":{"type":"errored","error":{"type":"error","error":{"details":{"error_visibility":"user_facing"},"type":"invalid_request_error","message":"messages: at least one message is required"},"request_id":null}}}""",
    """{"custom_id":"probe-beta","result":{"type":"succeeded","message":{"model":"claude-haiku-4-5-20251001","id":"msg_011CdvzDZWTGo2WQ1dBtcJgs","type":"message","role":"assistant","content":[{"type":"text","text":"BETA"}],"stop_reason":"end_turn","stop_sequence":null,"stop_details":null,"usage":{"input_tokens":14,"cache_creation_input_tokens":0,"cache_read_input_tokens":0,"cache_creation":{"ephemeral_5m_input_tokens":0,"ephemeral_1h_input_tokens":0},"output_tokens":5,"service_tier":"batch","inference_geo":"not_available"}}}}""",
    """{"custom_id":"probe-alpha","result":{"type":"succeeded","message":{"model":"claude-haiku-4-5-20251001","id":"msg_011CdvzDZLnP6anCfgdRaJYa","type":"message","role":"assistant","content":[{"type":"text","text":"ALPHA"}],"stop_reason":"end_turn","stop_sequence":null,"stop_details":null,"usage":{"input_tokens":14,"cache_creation_input_tokens":0,"cache_read_input_tokens":0,"cache_creation":{"ephemeral_5m_input_tokens":0,"ephemeral_1h_input_tokens":0},"output_tokens":5,"service_tier":"batch","inference_geo":"not_available"}}}}"""
  )
}
