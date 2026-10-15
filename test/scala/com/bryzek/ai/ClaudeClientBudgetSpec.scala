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

  private final class Transport(responses: ClaudeResponse*) extends IClient {
    val calls = new AtomicInteger(0)
    override def createMessage(body: ClaudeRequest, requestHeaders: Seq[(String, String)]): Future[ClaudeResponse] = {
      val i = calls.getAndIncrement()
      Future.successful(responses(math.min(i, responses.size - 1)))
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
    override def storeRequest(request: ClaudeRequestMetadata): Unit = synchronized(requests += request)
    override def storeResponseError(request: ClaudeRequestMetadata, e: Seq[ClaudeError]): Unit =
      synchronized(errors += e)
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

  private def fixture(budget: BigDecimal, spent: BigDecimal, responses: ClaudeResponse*): Fixture = {
    val source = new FakeAiBudgetSource(AiBudgetReading(budget, spent))
    val cache = new AiBudgetCache(source, new MutableClock(Noon))
    val scripted = if (responses.isEmpty) Seq(answer()) else responses
    val transport = new Transport(scripted*)
    val store = new RecordingStore
    Fixture(ClaudeClient(transport, ClaudeConfig("test-api-key"), store, cache), transport, store, source, cache)
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
