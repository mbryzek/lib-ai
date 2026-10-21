package com.bryzek.ai.claude

import cats.data.Validated.{Invalid, Valid}
import com.bryzek.ai.{
  AiBudgetCache,
  AiBudgetExceeded,
  AiBudgetReading,
  AiBudgetRefusal,
  AiBudgetStanding,
  AiBudgetUnavailable,
  FakeAiBudgetSource,
  MutableClock
}
import com.bryzek.claude.client.IClient
import com.bryzek.claude.models.{
  ClaudeBatch,
  ClaudeBatchForm,
  ClaudeContentBlock,
  ClaudeContentType,
  ClaudeError,
  ClaudeModel,
  ClaudeRequest,
  ClaudeResponse,
  ClaudeRole,
  ClaudeStopReason,
  ClaudeUsage
}
import helpers.TestAiFeature
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json

import java.time.{Instant, ZoneOffset}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.concurrent.{Await, Future}

/** The pre-call check: [[ClaudeClient]] asks the feature's budget before every request it sends, refuses with a typed
  * failure when nothing is left, and hands every response's cost to the budget.
  */
class ClaudeClientBudgetSpec extends AnyWordSpec with Matchers {

  private val Noon = Instant.parse("2026-10-14T12:00:00Z")
  private val Wait = FiniteDuration(10, SECONDS)

  /** $2 in and $10 out per million on Sonnet 5, so 1,000 of each costs $0.012. */
  private val PerCallUsd = BigDecimal("0.012")

  private def answer(model: ClaudeModel = ClaudeModel.ClaudeSonnet5): ClaudeResponse = ClaudeResponse(
    id = "msg_answer",
    `type` = "message",
    role = ClaudeRole.Assistant,
    content = Seq(ClaudeClient.textBlock("an answer")),
    model = model,
    stopReason = ClaudeStopReason.EndTurn,
    usage = ClaudeUsage(inputTokens = 1000, outputTokens = 1000)
  )

  private val toolTurn: ClaudeResponse = answer().copy(
    content = Seq(
      ClaudeContentBlock(ClaudeContentType.ToolUse)
        .copy(id = Some("tu_1"), name = Some("look"), input = Some(Json.obj()))
    ),
    stopReason = ClaudeStopReason.ToolUse
  )

  /** A paused server-tool turn, resumed by the client. Same usage as [[answer]], so it costs [[PerCallUsd]]. */
  private val pausedTurn: ClaudeResponse = answer().copy(
    content = Seq(
      ClaudeClient.textBlock("I searched and found"),
      ClaudeContentBlock(ClaudeContentType.ServerToolUse)
        .copy(id = Some("srvtoolu_1"), name = Some("web_search"), input = Some(Json.obj("query" -> "q")))
    ),
    stopReason = ClaudeStopReason.PauseTurn
  )

  /** Answers each call with the next scripted response, or fails it with the next scripted throwable. `onCall` runs
    * before each answer -- a test uses it to spend the budget while a call is in flight.
    */
  private final class Transport(responses: Either[Throwable, ClaudeResponse]*) extends IClient {
    val calls = new AtomicInteger(0)
    var onCall: () => Unit = () => ()
    override def createMessage(body: ClaudeRequest, requestHeaders: Seq[(String, String)]): Future[ClaudeResponse] = {
      val i = calls.getAndIncrement()
      onCall()
      responses(math.min(i, responses.size - 1)).fold(Future.failed, Future.successful)
    }
    private def unused[T]: Future[T] = Future.failed(new UnsupportedOperationException("not part of this test double"))
    override def createClaudeBatch(body: ClaudeBatchForm, requestHeaders: Seq[(String, String)]): Future[ClaudeBatch] =
      unused
    override def getClaudeBatchById(id: String, requestHeaders: Seq[(String, String)]): Future[ClaudeBatch] = unused
    override def cancelClaudeBatchById(id: String, requestHeaders: Seq[(String, String)]): Future[ClaudeBatch] = unused
  }

  private final class RecordingStore extends ClaudeStore {
    val requests: mutable.Buffer[ClaudeRequestMetadata] = mutable.Buffer.empty
    val successes: mutable.Buffer[ClaudeResponseMetadata[?]] = mutable.Buffer.empty
    val errors: mutable.Buffer[Seq[ClaudeError]] = mutable.Buffer.empty
    val chargedErrors: mutable.Buffer[ClaudeResponseMetadata[ClaudeResponse]] = mutable.Buffer.empty
    override def storeRequest(request: ClaudeRequestMetadata): Unit = synchronized(requests += request)
    override def storeResponseError(
      request: ClaudeRequestMetadata,
      e: Seq[ClaudeError],
      charged: Option[ClaudeResponseMetadata[ClaudeResponse]]
    ): Unit = synchronized {
      errors += e
      chargedErrors ++= charged
    }

    /** Every cost this store was handed: what a ledger summing it would read. */
    def costs: Seq[BigDecimal] = synchronized((successes.map(_.costUsd) ++ chargedErrors.map(_.costUsd)).toSeq)
    override def storeResponseSuccess[T](response: ClaudeResponseMetadata[T]): Unit = synchronized(
      successes += response
    )
  }

  private case class Fixture(
    client: ClaudeClient,
    transport: Transport,
    store: RecordingStore,
    source: FakeAiBudgetSource,
    budget: AiBudgetCache
  )

  private def fixture(budget: BigDecimal, spent: BigDecimal, responses: ClaudeResponse*): Fixture =
    scripted(budget, spent, (if (responses.isEmpty) Seq(answer()) else responses).map(Right(_))*)

  private def scripted(budget: BigDecimal, spent: BigDecimal, outcomes: Either[Throwable, ClaudeResponse]*): Fixture = {
    val source = new FakeAiBudgetSource(AiBudgetReading(budget, spent))
    val cache = new AiBudgetCache(source, new MutableClock(Noon))
    val transport = new Transport(outcomes*)
    val store = new RecordingStore
    val config = ClaudeConfig("test-api-key").copy(backoff = ClaudeBackoff.Immediate)
    Fixture(ClaudeClient(transport, config, store, cache), transport, store, source, cache)
  }

  private val request = AiRequest(messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, "hello")))

  private def chat(f: Fixture, models: Seq[ClaudeModel] = Seq(ClaudeModel.ClaudeSonnet5)) =
    outcome(f.client.chatText(request, models, TestAiFeature))

  private def outcome[T](future: Future[T]): Either[AiBudgetRefusal, T] =
    Await.result(future.map(Right(_)).recover { case r: AiBudgetRefusal => Left(r) }, Wait)

  private def spentNow(f: Fixture): BigDecimal = f.budget.remaining(TestAiFeature) match {
    case k: AiBudgetStanding.Known => k.spentUsd
    case AiBudgetStanding.Unknown => fail("expected a reading")
  }

  "ClaudeClient" must {
    "refuse a call for a feature over budget without sending or storing anything" in {
      val f = fixture(budget = BigDecimal(2), spent = BigDecimal("2.03"))
      chat(f) mustBe Left(
        AiBudgetExceeded(
          TestAiFeature,
          BigDecimal("2.03"),
          BigDecimal(2),
          Noon.atZone(ZoneOffset.UTC).toLocalDate.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
        )
      )
      f.transport.calls.get mustBe 0
      f.store.requests mustBe empty
      f.budget.pendingCounts(TestAiFeature).map(_.callsRefused) mustBe Seq(1L)
      f.source.overBudget.size mustBe 1
    }

    "refuse a call for a feature whose spend today has never been read" in {
      val f = fixture(budget = BigDecimal(2), spent = BigDecimal(0))
      f.source.failing = true
      chat(f) mustBe Left(AiBudgetUnavailable(TestAiFeature))
      f.transport.calls.get mustBe 0
    }

    "price each response, carry the cost on the metadata, and hand it to the budget" in {
      val f = fixture(budget = BigDecimal(2), spent = BigDecimal(0))
      chat(f) match {
        case Right(Valid(rm)) => rm.costUsd mustBe PerCallUsd
        case other => fail(s"expected an answer, got $other")
      }
      spentNow(f) mustBe PerCallUsd
      f.store.successes.map(_.costUsd) mustBe Seq(PerCallUsd)
    }

    "let the call that crosses the line complete and refuse the next one" in {
      val f = fixture(budget = BigDecimal("0.02"), spent = BigDecimal(0))
      chat(f).isRight mustBe true
      chat(f).isRight mustBe true
      spentNow(f) mustBe PerCallUsd * 2
      chat(f).left.map(_.getClass) mustBe Left(classOf[AiBudgetExceeded])
      f.transport.calls.get mustBe 2
    }

    "check the budget before every turn of a tool loop" in {
      val f = fixture(budget = BigDecimal("0.01"), spent = BigDecimal(0), toolTurn, answer())
      val tool = ClaudeTools.custom(name = "look", description = "looks", inputSchema = Json.obj("type" -> "object"))
      val result = outcome(
        f.client.runToolLoopText(request, Seq(tool), Seq(ClaudeModel.ClaudeSonnet5), maxCalls = 5, TestAiFeature) { _ =>
          Future.successful(ClaudeToolOutput("seen"))
        }
      )
      result.left.map(_.getClass) mustBe Left(classOf[AiBudgetExceeded])
      f.transport.calls.get mustBe 1
    }

    "store a resumed pause_turn turn's charge, not only the turn that settled" in {
      val f = fixture(budget = BigDecimal(2), spent = BigDecimal(0), pausedTurn, answer())
      chat(f) match {
        case Right(Valid(rm)) => rm.content mustBe "an answer"
        case other => fail(s"expected an answer, got $other")
      }
      f.store.requests.size mustBe 2
      f.store.successes.map(_.response.stopReason) mustBe Seq(ClaudeStopReason.PauseTurn, ClaudeStopReason.EndTurn)
      f.store.costs mustBe Seq(PerCallUsd, PerCallUsd)
      spentNow(f) mustBe PerCallUsd * 2
    }

    "store the charge of a response that failed to parse with its error" in {
      // No text block: the turn ended and was charged, and there is no answer in it.
      val textless = answer().copy(content = Seq.empty)
      val f = fixture(budget = BigDecimal(2), spent = BigDecimal(0), textless)
      chat(f) match {
        case Right(Invalid(_)) => ()
        case other => fail(s"expected an error, got $other")
      }
      f.store.successes mustBe empty
      f.store.errors.size mustBe 1
      f.store.costs mustBe Seq(PerCallUsd)
      spentNow(f) mustBe PerCallUsd
    }

    "store the charge of a turn still paused after the last resume with its error" in {
      val f = fixture(budget = BigDecimal(2), spent = BigDecimal(0), pausedTurn)
      chat(f) match {
        case Right(Invalid(errors)) => errors.head.message must include("after")
        case other => fail(s"expected an error, got $other")
      }
      val attempts = ClaudeClient.MaxPauseTurnResumes + 1
      f.store.requests.size mustBe attempts
      f.store.errors.size mustBe 1
      f.store.costs mustBe Seq.fill(attempts)(PerCallUsd)
    }

    "store no charge for an attempt that was never answered" in {
      val f = scripted(budget = BigDecimal(2), spent = BigDecimal(0), Left(new IllegalStateException("boom")))
      chat(f).isRight mustBe true
      f.store.errors.size mustBe 1
      f.store.costs mustBe empty
    }

    "check the budget again before a retry, and refuse it with the typed refusal" in {
      val f = scripted(
        budget = BigDecimal(2),
        spent = BigDecimal(0),
        Left(new java.io.IOException("Remotely closed")),
        Right(answer())
      )
      // Another call exhausts the feature while the first attempt is failing.
      f.transport.onCall = () => if (f.transport.calls.get == 1) f.budget.spent(TestAiFeature, BigDecimal(3))
      chat(f).left.map(_.getClass) mustBe Left(classOf[AiBudgetExceeded])
      f.transport.calls.get mustBe 1
      f.store.requests.size mustBe 1
      f.store.errors.size mustBe 1
      f.store.errors.head.head.message must include("Refused before a retry")
    }

    "retry while the budget allows it" in {
      val f = scripted(
        budget = BigDecimal(2),
        spent = BigDecimal(0),
        Left(new java.io.IOException("Remotely closed")),
        Right(answer())
      )
      chat(f).map(_.isValid) mustBe Right(true)
      f.transport.calls.get mustBe 2
    }

    "refuse a model it cannot price without sending it" in {
      val f = fixture(budget = BigDecimal(2), spent = BigDecimal(0))
      chat(f, Seq(ClaudeModel.UNDEFINED("claude-not-yet-priced"))) match {
        case Right(Invalid(errors)) => errors.head.message must include("has no price")
        case other => fail(s"expected a refusal, got $other")
      }
      f.transport.calls.get mustBe 0
      f.store.requests.size mustBe 1
      f.store.errors.size mustBe 1
      spentNow(f) mustBe BigDecimal(0)
    }

    "price a dated snapshot the API echoes back at its alias' rate" in {
      val f = fixture(
        budget = BigDecimal(2),
        spent = BigDecimal(0),
        answer(ClaudeModel.UNDEFINED("claude-sonnet-5-20261001"))
      )
      chat(f).isRight mustBe true
      spentNow(f) mustBe PerCallUsd
    }

    "price an echoed model the table does not know by the model that was asked for" in {
      val f = fixture(budget = BigDecimal(2), spent = BigDecimal(0), answer(ClaudeModel.UNDEFINED("claude-mystery")))
      chat(f).isRight mustBe true
      spentNow(f) mustBe PerCallUsd
    }
  }
}
