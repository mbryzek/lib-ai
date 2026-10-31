package com.bryzek.ai.claude

import com.bryzek.ai.{AiModelCache, AiModelReading, AiModelTrial, FakeAiModelSource, MutableClock}
import com.bryzek.claude.models.{ClaudeEffort, ClaudeModel, ClaudeRole}
import helpers.{FutureHelpers, TestAiFeature, TestTrialFeature}
import org.apache.pekko.util.Timeout
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{FiniteDuration, SECONDS}

/** The model in force and the trial hook through [[ClaudeBatchClient]]: each item naming a feature is resolved when the
  * batch is submitted, and a reconciled result of a feature with an open trial yields the item to re-submit on the new
  * model.
  */
class ClaudeBatchClientModelsSpec extends AnyWordSpec with Matchers with FutureHelpers {

  private implicit val timeout: Timeout = FiniteDuration(30, SECONDS)

  private val trial = AiModelTrial(
    id = "trial-1",
    family = ClaudeModelFamily.Haiku,
    fromModel = ClaudeModel.ClaudeHaiku45,
    toModel = ClaudeModel.ClaudeHaiku55,
    chargedTo = TestTrialFeature
  )

  private def client(reading: AiModelReading): (FakeAiModelSource, ClaudeBatchClient) = {
    val source = new FakeAiModelSource(reading)
    val models = new AiModelCache(source, new MutableClock(Instant.parse("2026-10-14T12:00:00Z")))
    (source, ClaudeBatchClient(new TestClaudeClient(), ClaudeConfig("test"), models))
  }

  private def item(customId: String, model: ClaudeModel = ClaudeModel.ClaudeHaiku45, featured: Boolean = true) =
    ClaudeBatchItem(
      customId = customId,
      request = AiRequest(messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, "hello"))),
      model = model,
      feature = Option.when(featured)(TestAiFeature)
    )

  "submitResolved" must {
    "send each item on its feature's model in force and say which model that was" in {
      val (_, batchClient) = client(AiModelReading(inForce = Map(ClaudeModelFamily.Haiku -> ClaudeModel.ClaudeHaiku55)))
      val submission =
        await(batchClient.submitResolved(Seq(item("alpha"), item("beta", featured = false)))).toOption.get
      submission.items.map(i => i.customId -> i.model) mustBe
        Seq("alpha" -> ClaudeModel.ClaudeHaiku55, "beta" -> ClaudeModel.ClaudeHaiku45)
      submission.batch.requestCounts.succeeded mustBe 2L
    }

    "send each model as named on a client built without one" in {
      val batchClient = ClaudeBatchClient(new TestClaudeClient(), ClaudeConfig("test"))
      batchClient.resolve(item("alpha")).model mustBe ClaudeModel.ClaudeHaiku45
    }

    "rebuild the request under the resolved model's own rules" in {
      val (_, batchClient) = client(AiModelReading(inForce = Map(ClaudeModelFamily.Haiku -> ClaudeModel.ClaudeHaiku55)))
      batchClient.resolve(item("alpha")).toClaudeRequest.outputConfig.flatMap(_.effort) mustBe Some(ClaudeEffort.High)
    }
  }

  "trialItems" must {
    "yield each sampled result's re-run on the new model, charged to the trial's feature" in {
      val (_, batchClient) = client(AiModelReading(trials = Seq(trial)))
      val submission = await(batchClient.submitResolved(Seq(item("alpha")))).toOption.get
      val outcomes = await(batchClient.results(submission.batch)).toOption.get

      val sampled = batchClient.trialItems(submission.items, outcomes)
      sampled.map(_.used.customId) mustBe Seq("alpha")
      val rerun = sampled.head.rerun
      rerun.model mustBe ClaudeModel.ClaudeHaiku55
      rerun.feature mustBe Some(TestTrialFeature)
      rerun.customId must not be "alpha"
      rerun.customId must fullyMatch regex "[a-zA-Z0-9_-]{1,64}"
      rerun.request mustBe submission.items.head.request
    }

    "make the pair from the re-run's outcome, priced on both sides" in {
      val (_, batchClient) = client(AiModelReading(trials = Seq(trial)))
      val submission = await(batchClient.submitResolved(Seq(item("alpha")))).toOption.get
      val outcomes = await(batchClient.results(submission.batch)).toOption.get
      val sampled = batchClient.trialItems(submission.items, outcomes).head

      val trialBatch = await(batchClient.submitResolved(Seq(sampled.rerun))).toOption.get
      trialBatch.items.map(_.model) mustBe Seq(ClaudeModel.ClaudeHaiku55)
      val rerunOutcome = await(batchClient.results(trialBatch.batch)).toOption.get.head
      val pair = sampled.pair(rerunOutcome).toOption.get
      pair.trial mustBe trial
      pair.feature mustBe TestAiFeature
      pair.usedId mustBe "alpha"
      pair.candidateId mustBe sampled.rerun.customId
      pair.request.model mustBe ClaudeModel.ClaudeHaiku45
      pair.candidateRequest.model mustBe ClaudeModel.ClaudeHaiku55
      pair.usedCostUsd must be > BigDecimal(0)
      pair.candidateCostUsd must be > BigDecimal(0)

      sampled.pair(outcomes.head).isInvalid mustBe true
    }

    "count against the same five a day" in {
      val (source, batchClient) = client(AiModelReading(trials = Seq(trial)))
      val items = (1 to 7).map(i => item(s"item$i"))
      val submission = await(batchClient.submitResolved(items)).toOption.get
      val outcomes = await(batchClient.results(submission.batch)).toOption.get
      batchClient.trialItems(submission.items, outcomes).size mustBe 5
      source.claims.size mustBe 5
    }

    "not sample an item with no feature, on another model, or that did not succeed" in {
      val (source, batchClient) = client(AiModelReading(trials = Seq(trial)))
      val submission = await(
        batchClient.submitResolved(
          Seq(item("unfeatured", featured = false), item("sonnet", model = ClaudeModel.ClaudeSonnet5))
        )
      ).toOption.get
      val outcomes = await(batchClient.results(submission.batch)).toOption.get
      val failed = ClaudeBatchOutcome(
        "failed",
        cats.data.Validated.invalidNec(com.bryzek.claude.models.ClaudeError(message = "Batch request expired"))
      )
      batchClient.trialItems(submission.items :+ item("failed"), outcomes :+ failed) mustBe empty
      source.claims mustBe empty
    }
  }
}
