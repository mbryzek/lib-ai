package com.bryzek.ai.claude

import com.bryzek.ai.{AiModelCache, AiModelReading, AiModelTrial, FakeAiModelSource, MutableClock, UnlimitedAiBudget}
import com.bryzek.claude.models.{
  ClaudeContentType,
  ClaudeEffort,
  ClaudeError,
  ClaudeModel,
  ClaudeRequest,
  ClaudeResponse,
  ClaudeRole
}
import com.bryzek.claude.response.models.SingleInsightResponse
import com.bryzek.claude.response.models.json.*
import helpers.{FutureHelpers, TestAiFeature, TestTrialFeature}
import org.apache.pekko.util.Timeout
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json

import java.time.{Instant, LocalDate}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.jdk.CollectionConverters.*

/** The model in force and the trial sampler, through [[ClaudeClient]]: every requested model is resolved before it is
  * sent, and a successful call of a feature with an open trial is re-sent to the new model after the caller has its
  * answer, with the pair handed to the store.
  */
class ClaudeClientModelsSpec extends AnyWordSpec with Matchers with FutureHelpers {

  private implicit val timeout: Timeout = FiniteDuration(10, SECONDS)

  private val Noon = Instant.parse("2026-10-14T12:00:00Z")

  /** Records every request it answers, then answers as [[TestClaudeClient]] does. */
  private class Recording extends TestClaudeClient {
    val sent = new ConcurrentLinkedQueue[ClaudeRequest]()
    override def createMessage(body: ClaudeRequest, requestHeaders: Seq[(String, String)]): Future[ClaudeResponse] = {
      sent.add(body)
      super.createMessage(body, requestHeaders)
    }
    def models: Seq[ClaudeModel] = sent.asScala.toSeq.map(_.model)
  }

  private final class Store extends ClaudeStore {
    val requests = new ConcurrentLinkedQueue[ClaudeRequestMetadata]()
    val pairs = new ConcurrentLinkedQueue[ClaudeTrialPair]()
    override def storeRequest(request: ClaudeRequestMetadata): Unit = { requests.add(request); () }
    override def storeResponseError(
      request: ClaudeRequestMetadata,
      errors: Seq[ClaudeError],
      charged: Option[ClaudeResponseMetadata[ClaudeResponse]]
    ): Unit = ()
    override def storeResponseSuccess[T](response: ClaudeResponseMetadata[T]): Unit = ()
    override def storeTrialPair(pair: ClaudeTrialPair): Unit = { pairs.add(pair); () }
  }

  private val trial = AiModelTrial(
    id = "trial-1",
    family = ClaudeModelFamily.Haiku,
    fromModel = ClaudeModel.ClaudeHaiku45,
    toModel = ClaudeModel.ClaudeHaiku55,
    chargedTo = TestTrialFeature
  )

  private case class Fixture(transport: Recording, store: Store, source: FakeAiModelSource, client: ClaudeClient)

  private def fixture(reading: AiModelReading): Fixture = {
    val transport = new Recording
    val store = new Store
    val source = new FakeAiModelSource(reading)
    val models = new AiModelCache(source, new MutableClock(Noon))
    Fixture(
      transport,
      store,
      source,
      ClaudeClient(transport, ClaudeConfig("test"), store, new UnlimitedAiBudget, models)
    )
  }

  private val request = AiRequest(messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, "hello")))

  "the model in force" must {
    "go out wherever the code names any version of its family" in {
      val f = fixture(AiModelReading(inForce = Map(ClaudeModelFamily.Haiku -> ClaudeModel.ClaudeHaiku55)))
      val answer = await(f.client.chatText(request, Seq(ClaudeModel.ClaudeHaiku45), TestAiFeature))
      answer.isValid mustBe true
      f.transport.models mustBe Seq(ClaudeModel.ClaudeHaiku55)
      answer.toOption.get.request.request.model mustBe ClaudeModel.ClaudeHaiku55
    }

    "resolve each element of a fallback list, keeping its order" in {
      val f = fixture(AiModelReading(inForce = Map(ClaudeModelFamily.Sonnet -> ClaudeModel.ClaudeSonnet55)))
      val overloaded = new Recording {
        override def createMessage(
          body: ClaudeRequest,
          requestHeaders: Seq[(String, String)]
        ): Future[ClaudeResponse] =
          if (body.model == ClaudeModel.ClaudeHaiku45) {
            sent.add(body)
            Future.failed(new RuntimeException("POST /v1/messages failed with status 529"))
          } else super.createMessage(body, requestHeaders)
      }
      val client =
        f.client.copy(client = overloaded, config = ClaudeConfig("test").copy(backoff = ClaudeBackoff.Immediate))
      val answer =
        await(client.chatText(request, Seq(ClaudeModel.ClaudeHaiku45, ClaudeModel.ClaudeSonnet5), TestAiFeature))
      answer.isValid mustBe true
      overloaded.models mustBe Seq(ClaudeModel.ClaudeHaiku45, ClaudeModel.ClaudeSonnet55)
    }

    "rebuild the request under the set model's own rules" in {
      val f = fixture(AiModelReading(inForce = Map(ClaudeModelFamily.Haiku -> ClaudeModel.ClaudeHaiku55)))
      await(f.client.chatText(request, Seq(ClaudeModel.ClaudeHaiku45), TestAiFeature)).isValid mustBe true
      // Haiku 4.5 takes no effort; Haiku 5.5's API default is medium, so the library says high out loud there.
      f.transport.sent.asScala.head.outputConfig.flatMap(_.effort) mustBe Some(ClaudeEffort.High)
    }

    "leave every model as named on a client built without one" in {
      val transport = new Recording
      val client = ClaudeClient(transport, ClaudeConfig("test"), NoopClaudeStore, new UnlimitedAiBudget)
      await(client.chatText(request, Seq(ClaudeModel.ClaudeHaiku45), TestAiFeature)).isValid mustBe true
      transport.models mustBe Seq(ClaudeModel.ClaudeHaiku45)
    }
  }

  "a trial" must {
    "re-send a successful call to the new model and hand the pair to the store" in {
      val f = fixture(AiModelReading(trials = Seq(trial)))
      val answer = await(f.client.chatText(request, Seq(ClaudeModel.ClaudeHaiku45), TestAiFeature, Some("ctx-1")))
      answer.isValid mustBe true
      answer.toOption.get.request.request.model mustBe ClaudeModel.ClaudeHaiku45

      val pair = eventuallyInNSeconds() {
        f.store.pairs.size mustBe 1
        f.store.pairs.peek()
      }
      pair.trial mustBe trial
      pair.feature mustBe TestAiFeature
      pair.context mustBe Some("ctx-1")
      pair.usedId mustBe answer.toOption.get.request.id
      pair.request.model mustBe ClaudeModel.ClaudeHaiku45
      pair.candidateRequest.model mustBe ClaudeModel.ClaudeHaiku55
      pair.candidateRequest.outputConfig.flatMap(_.effort) mustBe Some(ClaudeEffort.High)
      pair.request.messages mustBe pair.candidateRequest.messages
      pair.usedCostUsd must be > BigDecimal(0)
      pair.candidateCostUsd must be > BigDecimal(0)
      f.transport.models mustBe Seq(ClaudeModel.ClaudeHaiku45, ClaudeModel.ClaudeHaiku55)
    }

    "charge the re-run to the trial's own feature" in {
      val f = fixture(AiModelReading(trials = Seq(trial)))
      await(f.client.chatText(request, Seq(ClaudeModel.ClaudeHaiku45), TestAiFeature)).isValid mustBe true
      eventuallyInNSeconds()(f.store.pairs.size mustBe 1)
      val byFeature = f.store.requests.asScala.toSeq.map(r => r.feature -> r.request.model)
      byFeature mustBe Seq(TestAiFeature -> ClaudeModel.ClaudeHaiku45, TestTrialFeature -> ClaudeModel.ClaudeHaiku55)
      f.store.pairs.peek().candidateId mustBe f.store.requests.asScala.toSeq(1).id
    }

    "re-send a structured call with the same output format" in {
      val f = fixture(AiModelReading(trials = Seq(trial)))
      val answer = await(
        f.client.chatCompletion[SingleInsightResponse](
          request,
          ClaudeOutputFormats.SingleInsight,
          Seq(ClaudeModel.ClaudeHaiku45),
          TestAiFeature
        )
      )
      answer.isValid mustBe true
      val pair = eventuallyInNSeconds() {
        f.store.pairs.size mustBe 1
        f.store.pairs.peek()
      }
      pair.candidateRequest.outputConfig.flatMap(_.format).map(_.schema) mustBe
        Some(ClaudeOutputFormats.SingleInsight.toApi.schema)
      Json.parse(ClaudeContent.text(pair.candidate)).asOpt[SingleInsightResponse] mustBe defined
    }

    "sample at most five a UTC day" in {
      val f = fixture(AiModelReading(trials = Seq(trial)))
      (1 to 7).foreach(_ => await(f.client.chatText(request, Seq(ClaudeModel.ClaudeHaiku45), TestAiFeature)))
      eventuallyInNSeconds()(f.store.pairs.size mustBe 5)
      f.source.claims.map(_._1).distinct.toSeq mustBe Seq(LocalDate.parse("2026-10-14"))
      f.source.claims.size mustBe 5
    }

    "not sample a call answered by a model the trial is not from" in {
      val f = fixture(AiModelReading(trials = Seq(trial)))
      await(f.client.chatText(request, Seq(ClaudeModel.ClaudeSonnet5), TestAiFeature)).isValid mustBe true
      f.source.claims mustBe empty
      f.transport.models mustBe Seq(ClaudeModel.ClaudeSonnet5)
    }

    "not sample a call that failed" in {
      val f = fixture(AiModelReading(trials = Seq(trial)))
      val failing = new Recording {
        override def createMessage(
          body: ClaudeRequest,
          requestHeaders: Seq[(String, String)]
        ): Future[ClaudeResponse] = Future.failed(new RuntimeException("POST /v1/messages failed with status 400"))
      }
      val client =
        f.client.copy(client = failing, config = ClaudeConfig("test").copy(backoff = ClaudeBackoff.Immediate))
      await(client.chatText(request, Seq(ClaudeModel.ClaudeHaiku45), TestAiFeature)).isValid mustBe false
      f.source.claims mustBe empty
    }

    "keep no pair when the re-run fails, and leave the caller's answer alone" in {
      val f = fixture(AiModelReading(trials = Seq(trial)))
      val candidateFails = new Recording {
        override def createMessage(
          body: ClaudeRequest,
          requestHeaders: Seq[(String, String)]
        ): Future[ClaudeResponse] =
          if (body.model == ClaudeModel.ClaudeHaiku55) {
            sent.add(body)
            Future.failed(new RuntimeException("POST /v1/messages failed with status 400"))
          } else super.createMessage(body, requestHeaders)
      }
      val client =
        f.client.copy(client = candidateFails, config = ClaudeConfig("test").copy(backoff = ClaudeBackoff.Immediate))
      await(client.chatText(request, Seq(ClaudeModel.ClaudeHaiku45), TestAiFeature)).isValid mustBe true
      eventuallyInNSeconds()(candidateFails.models mustBe Seq(ClaudeModel.ClaudeHaiku45, ClaudeModel.ClaudeHaiku55))
      f.source.claims.size mustBe 1
      f.store.pairs mustBe empty
    }

    "sample a tool loop's opening turn and never run the re-run's tool calls" in {
      val f = fixture(AiModelReading(trials = Seq(trial)))
      val tool = ClaudeTools.custom(
        name = "get_metric",
        description = "Return a metric",
        inputSchema = Json.obj("type" -> "object", "properties" -> Json.obj(), "additionalProperties" -> false)
      )
      val executed = new java.util.concurrent.atomic.AtomicInteger(0)
      val answer = await(
        f.client.runToolLoopText(request, Seq(tool), Seq(ClaudeModel.ClaudeHaiku45), maxCalls = 5, TestAiFeature) { _ =>
          executed.incrementAndGet()
          Future.successful(ClaudeToolOutput(content = """{"total": 42}"""))
        }
      )
      answer.isValid mustBe true
      val pair = eventuallyInNSeconds() {
        f.store.pairs.size mustBe 1
        f.store.pairs.peek()
      }
      pair.used.content.map(_.`type`) mustBe Seq(ClaudeContentType.ToolUse)
      pair.candidate.content.map(_.`type`) mustBe Seq(ClaudeContentType.ToolUse)
      pair.candidateRequest.tools.map(_.map(_.name)) mustBe Some(Seq("get_metric"))
      pair.candidateRequest.messages.flatMap(_.content).map(_.`type`) must not contain ClaudeContentType.ToolResult
      executed.get mustBe 1
    }
  }
}
