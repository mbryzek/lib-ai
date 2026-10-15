package com.bryzek.ai

import helpers.TestAiFeature
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.{Clock, Instant, LocalDate, ZoneId, ZoneOffset}
import scala.collection.mutable

/** A clock a test moves by hand. */
final class MutableClock(var now: Instant) extends Clock {
  override def getZone: ZoneId = ZoneOffset.UTC
  override def withZone(zone: ZoneId): Clock = this
  override def instant(): Instant = now
  def advanceSeconds(seconds: Long): Unit = now = now.plusSeconds(seconds)
}

/** A ledger a test scripts: what each refresh reads, whether it fails, and everything handed to it. */
final class FakeAiBudgetSource(var reading: AiBudgetReading) extends AiBudgetSource {
  var failing: Boolean = false
  val refreshes: mutable.Buffer[(LocalDate, Seq[AiBudgetCounts])] = mutable.Buffer.empty
  val overBudget: mutable.Buffer[(LocalDate, AiBudgetExceeded)] = mutable.Buffer.empty

  override def refresh(feature: AiFeature, day: LocalDate, counts: Seq[AiBudgetCounts]): AiBudgetReading = {
    refreshes += ((day, counts))
    if (failing) throw new RuntimeException("ledger unavailable")
    reading
  }

  override def wentOverBudget(day: LocalDate, exceeded: AiBudgetExceeded): Unit = overBudget += ((day, exceeded))
}

class AiBudgetCacheSpec extends AnyWordSpec with Matchers {

  private val Day = LocalDate.parse("2026-10-14")
  private val Noon = Day.atTime(12, 0).toInstant(ZoneOffset.UTC)
  private val feature = TestAiFeature

  private def fixture(budget: BigDecimal = BigDecimal(4), spent: BigDecimal = BigDecimal(0)) = {
    val clock = new MutableClock(Noon)
    val source = new FakeAiBudgetSource(AiBudgetReading(budget, spent))
    (clock, source, new AiBudgetCache(source, clock))
  }

  private def known(standing: AiBudgetStanding): AiBudgetStanding.Known = standing match {
    case k: AiBudgetStanding.Known => k
    case AiBudgetStanding.Unknown => fail("expected a reading of today")
  }

  "remaining" must {
    "read the ledger once and answer from the cache for 60 seconds" in {
      val (clock, source, cache) = fixture(spent = BigDecimal(1))
      known(cache.remaining(feature)).remainingUsd mustBe BigDecimal(3)
      source.reading = AiBudgetReading(BigDecimal(4), BigDecimal(2))
      clock.advanceSeconds(59)
      known(cache.remaining(feature)).remainingUsd mustBe BigDecimal(3)
      source.refreshes.size mustBe 1
      clock.advanceSeconds(1)
      known(cache.remaining(feature)).remainingUsd mustBe BigDecimal(2)
      source.refreshes.size mustBe 2
    }

    "count today as the UTC day and reset at the next 00:00 UTC" in {
      val (_, _, cache) = fixture()
      val standing = known(cache.remaining(feature))
      standing.day mustBe Day
      standing.resetsAt mustBe Instant.parse("2026-10-15T00:00:00Z")
    }

    "refresh on a new UTC day even inside the 60 seconds" in {
      val (clock, source, cache) = fixture()
      clock.now = Day.atTime(23, 59, 50).toInstant(ZoneOffset.UTC)
      cache.remaining(feature)
      clock.advanceSeconds(20)
      known(cache.remaining(feature)).day mustBe Day.plusDays(1)
      source.refreshes.map(_._1) mustBe Seq(Day, Day.plusDays(1))
    }
  }

  "spent" must {
    "take the process's own calls off the cached remaining at once" in {
      val (_, source, cache) = fixture()
      cache.remaining(feature)
      cache.spent(feature, BigDecimal("1.50"))
      cache.spent(feature, BigDecimal("2.50"))
      val standing = known(cache.remaining(feature))
      standing.spentUsd mustBe BigDecimal(4)
      standing.exhausted mustBe true
      source.refreshes.size mustBe 1
    }
  }

  "a failed refresh" must {
    "refuse a feature never read: there is no fail-open on an unknown number" in {
      val (_, source, cache) = fixture()
      source.failing = true
      cache.remaining(feature) mustBe AiBudgetStanding.Unknown
      cache.check(feature) mustBe Left(AiBudgetUnavailable(feature))
    }

    "leave the last reading of today standing" in {
      val (clock, source, cache) = fixture(spent = BigDecimal(1))
      cache.remaining(feature)
      source.failing = true
      clock.advanceSeconds(120)
      known(cache.remaining(feature)).remainingUsd mustBe BigDecimal(3)
    }

    "not let yesterday's reading stand for today" in {
      val (clock, source, cache) = fixture(spent = BigDecimal(1))
      cache.remaining(feature)
      source.failing = true
      clock.now = Day.plusDays(1).atTime(0, 0, 5).toInstant(ZoneOffset.UTC)
      cache.remaining(feature) mustBe AiBudgetStanding.Unknown
    }

    "be retried after the backoff rather than on every call" in {
      val (clock, source, cache) = fixture()
      source.failing = true
      cache.remaining(feature)
      cache.remaining(feature)
      source.refreshes.size mustBe 1
      clock.advanceSeconds(AiBudgetCache.DefaultRetryAfterFailure.toSeconds)
      source.failing = false
      known(cache.remaining(feature)).remainingUsd mustBe BigDecimal(4)
      source.refreshes.size mustBe 2
    }
  }

  "check" must {
    "allow a call while anything is left" in {
      val (_, _, cache) = fixture(budget = BigDecimal(2), spent = BigDecimal("1.99"))
      cache.check(feature).isRight mustBe true
    }

    "refuse once spend reaches the budget, saying what was spent and when it resets" in {
      val (_, _, cache) = fixture(budget = BigDecimal(2), spent = BigDecimal("2.03"))
      cache.check(feature) mustBe Left(
        AiBudgetExceeded(feature, BigDecimal("2.03"), BigDecimal(2), Instant.parse("2026-10-15T00:00:00Z"))
      )
    }
  }

  "refusals and deferrals" must {
    "write the over-budget fact on the first refusal of the day only" in {
      val (_, source, cache) = fixture(spent = BigDecimal(5))
      cache.check(feature).isLeft mustBe true
      cache.check(feature).isLeft mustBe true
      cache.check(feature).isLeft mustBe true
      source.overBudget.map(_._1) mustBe Seq(Day)
      source.overBudget.head._2.spentUsd mustBe BigDecimal(5)
    }

    "be counted locally and carried to the next refresh, then cleared" in {
      val (clock, source, cache) = fixture(spent = BigDecimal(5))
      cache.check(feature).isLeft mustBe true
      cache.check(feature).isLeft mustBe true
      cache.deferred(feature)
      cache.deferred(feature)
      cache.deferred(feature)
      source.refreshes.size mustBe 1
      cache.pendingCounts(feature) mustBe Seq(AiBudgetCounts(Day, callsRefused = 2, tasksDeferred = 3))

      clock.advanceSeconds(60)
      cache.remaining(feature)
      source.refreshes.last mustBe ((Day, Seq(AiBudgetCounts(Day, 2, 3))))
      cache.pendingCounts(feature) mustBe Nil
    }

    "be kept for the refresh after one that failed" in {
      val (clock, source, cache) = fixture(spent = BigDecimal(5))
      cache.check(feature).isLeft mustBe true
      source.failing = true
      clock.advanceSeconds(60)
      cache.remaining(feature)
      cache.pendingCounts(feature) mustBe Seq(AiBudgetCounts(Day, 1, 0))

      source.failing = false
      clock.advanceSeconds(60)
      cache.remaining(feature)
      source.refreshes.last mustBe ((Day, Seq(AiBudgetCounts(Day, 1, 0))))
      cache.pendingCounts(feature) mustBe Nil
    }

    "stay on the day they happened when the refresh carrying them is tomorrow's" in {
      val (clock, source, cache) = fixture(spent = BigDecimal(5))
      cache.check(feature).isLeft mustBe true
      cache.deferred(feature)
      clock.now = Day.plusDays(1).atTime(0, 0, 30).toInstant(ZoneOffset.UTC)
      cache.remaining(feature)
      source.refreshes.last mustBe ((Day.plusDays(1), Seq(AiBudgetCounts(Day, 1, 1))))
    }

    "be flushed for a feature that stopped making calls" in {
      val (_, source, cache) = fixture(spent = BigDecimal(5))
      cache.check(feature).isLeft mustBe true
      cache.flush()
      source.refreshes.size mustBe 2
      cache.pendingCounts(feature) mustBe Nil
      cache.flush()
      source.refreshes.size mustBe 2
    }
  }
}
