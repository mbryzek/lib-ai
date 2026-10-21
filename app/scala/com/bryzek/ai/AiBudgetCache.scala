package com.bryzek.ai

import java.time.{Clock, Instant, LocalDate}
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** A feature's budget in force and its spend so far on one UTC day, as an app's ledger answers it. */
final case class AiBudgetReading(budgetUsd: BigDecimal, spentUsd: BigDecimal)

/** This process's refusals and deferrals for one feature on one UTC day, not yet added to the ledger. */
final case class AiBudgetCounts(day: LocalDate, callsRefused: Long, tasksDeferred: Long) {
  def isEmpty: Boolean = callsRefused == 0 && tasksDeferred == 0
}

/** The app's half of an [[AiBudgetCache]]: its ledger. Platform and acumen each implement it over their own tables.
  *
  * Both methods may block (they are database reads and writes) and may throw; the cache catches what they throw.
  */
trait AiBudgetSource {

  /** Records `counts` -- this process's refusals and deferrals per day since its last successful refresh, oldest day
    * first, usually one entry and often none -- onto the ledger, then reads `feature`'s budget in force and its spend
    * so far on `day`. Counts are applied before anything that reads them (a day-reset row takes its count of resumed
    * tasks from the deferrals), and a refresh that throws hands the same counts to the next one, so an implementation
    * must apply them in the same transaction as anything else it writes, or not at all.
    */
  def refresh(feature: AiFeature, day: LocalDate, counts: Seq[AiBudgetCounts]): AiBudgetReading

  /** The first refusal of `exceeded.feature` on its day in this process -- the moment the day's over-budget fact is
    * written, with the spend and the budget as they stood. Every process calls this once per feature per day until it
    * succeeds -- a write that throws is tried again on the next refusal, refresh or [[AiBudgetCache.flush]] after the
    * cache's failure backoff, on its own day -- so the write must be idempotent (a unique key on feature and day).
    */
  def wentOverBudget(day: LocalDate, exceeded: AiBudgetExceeded): Unit
}

/** The 60-second per-feature remaining cache, both apps' [[AiBudget]].
  *
  * `remaining` answers from a reading at most `ttl` old. Past that it refreshes from the [[AiBudgetSource]], and that
  * refresh carries the refusals and deferrals this process counted since the last one, so a refusing loop costs one
  * write a minute rather than one per call. Every call this process makes adds its cost to the cached spend at once
  * ([[spent]]), so a tight loop on one pod stops on time; the fuzz is only the other pods' spend during the window.
  *
  * A refresh that fails leaves the last reading standing until one succeeds (tried again after `retryAfterFailure`) -- but only a reading of TODAY: yesterday's
  * spend says nothing about today's, so after 00:00 UTC a feature whose refresh fails is [[AiBudgetStanding.Unknown]]
  * exactly like one never read, and refuses. There is no fail-open on an unknown number.
  *
  * Thread-safe. A feature's refresh runs under that feature's own lock, so concurrent calls for one feature produce one
  * read, and features never wait on each other.
  */
class AiBudgetCache(
  source: AiBudgetSource,
  clock: Clock = Clock.systemUTC(),
  ttl: FiniteDuration = AiBudgetCache.DefaultTtl,
  retryAfterFailure: FiniteDuration = AiBudgetCache.DefaultRetryAfterFailure
) extends AiBudget {

  private final class Entry {
    var reading: Option[AiBudgetStanding.Known] = None
    var readAt: Instant = Instant.EPOCH
    var failedAt: Option[Instant] = None
    var pending: Map[LocalDate, AiBudgetCounts] = Map.empty
    // The day's first refusal, held until [[AiBudgetSource.wentOverBudget]] has written it; then the day is recorded
    // as written, so later refusals that day write nothing.
    var overBudget: Map[LocalDate, AiBudgetExceeded] = Map.empty
    var overBudgetWritten: Option[LocalDate] = None
    var overBudgetFailedAt: Option[Instant] = None
  }

  private val entries = new ConcurrentHashMap[String, (AiFeature, Entry)]()

  private def entryFor(feature: AiFeature): Entry =
    entries.computeIfAbsent(feature.name, _ => (feature, new Entry))._2

  override def remaining(feature: AiFeature): AiBudgetStanding = {
    val entry = entryFor(feature)
    entry.synchronized {
      val now = clock.instant()
      val today = AiBudgetDay.today(clock)
      val fresh = entry.reading.exists(_.day == today) && now.isBefore(entry.readAt.plusMillis(ttl.toMillis))
      val backingOff = entry.failedAt.exists(at => now.isBefore(at.plusMillis(retryAfterFailure.toMillis)))
      if (!fresh && !backingOff) refreshLocked(feature, entry, today, now)
      writeOverBudgetLocked(entry, now)
      entry.reading.filter(_.day == today).getOrElse(AiBudgetStanding.Unknown)
    }
  }

  /** Adds `costUsd` to the reading of today, so this process's own calls count at once. With no reading yet there is
    * nothing to add to and nothing is kept: the next [[remaining]] reads the ledger, which a call's
    * [[com.bryzek.ai.claude.ClaudeStore]] has already written that cost to, so keeping it here too would count it
    * twice. The running total is only ever a bridge to the next refresh, which replaces it with the ledger's.
    */
  override def spent(feature: AiFeature, costUsd: BigDecimal): Unit = {
    val entry = entryFor(feature)
    entry.synchronized {
      entry.reading = entry.reading.map(r => r.copy(spentUsd = r.spentUsd + costUsd))
    }
  }

  override def refused(exceeded: AiBudgetExceeded): Unit = {
    val entry = entryFor(exceeded.feature)
    val day = AiBudgetDay.today(clock)
    entry.synchronized {
      count(entry, day, refused = 1, deferred = 0)
      if (!entry.overBudgetWritten.contains(day) && !entry.overBudget.contains(day)) {
        entry.overBudget = entry.overBudget.updated(day, exceeded)
      }
      writeOverBudgetLocked(entry, clock.instant())
    }
  }

  override def deferred(feature: AiFeature): Unit = {
    val entry = entryFor(feature)
    entry.synchronized(count(entry, AiBudgetDay.today(clock), refused = 0, deferred = 1))
  }

  /** Refreshes every feature holding counts not yet on the ledger, and retries every over-budget fact not yet written. A refusing loop that stops making calls never asks
    * [[remaining]] again, so without this its last counts would wait for the feature's next call; an app runs it on a
    * schedule (every minute is plenty) to bound that wait.
    */
  def flush(): Unit =
    entries.values().asScala.toList.foreach { case (feature, entry) =>
      entry.synchronized {
        val now = clock.instant()
        if (entry.pending.nonEmpty) refreshLocked(feature, entry, AiBudgetDay.today(clock), now)
        writeOverBudgetLocked(entry, now)
      }
    }

  /** Counts not yet on the ledger, per day -- for tests and for an app's own diagnostics. */
  def pendingCounts(feature: AiFeature): Seq[AiBudgetCounts] = {
    val entry = entryFor(feature)
    entry.synchronized(entry.pending.values.toSeq.sortBy(_.day))
  }

  private def count(entry: Entry, day: LocalDate, refused: Long, deferred: Long): Unit = {
    val current = entry.pending.getOrElse(day, AiBudgetCounts(day, 0, 0))
    entry.pending = entry.pending.updated(
      day,
      current.copy(callsRefused = current.callsRefused + refused, tasksDeferred = current.tasksDeferred + deferred)
    )
  }

  /** Over-budget facts not yet written, per day -- for tests and for an app's own diagnostics. */
  def pendingOverBudget(feature: AiFeature): Seq[LocalDate] = {
    val entry = entryFor(feature)
    entry.synchronized(entry.overBudget.keys.toSeq.sorted)
  }

  /** Called holding `entry`'s lock, so concurrent refusals make one write. Writes each held over-budget fact, oldest day
    * first, and forgets it only once the write succeeds; a write that throws leaves it, and every later day's, for the
    * next try after `retryAfterFailure`.
    */
  private def writeOverBudgetLocked(entry: Entry, now: Instant): Unit = {
    val backingOff = entry.overBudgetFailedAt.exists(at => now.isBefore(at.plusMillis(retryAfterFailure.toMillis)))
    if (entry.overBudget.nonEmpty && !backingOff) {
      // `forall` stops at the first write that throws, leaving it and every later day for the next try.
      entry.overBudget.toSeq.sortBy(_._1).forall { case (day, exceeded) =>
        try {
          source.wentOverBudget(day, exceeded)
          entry.overBudget = entry.overBudget - day
          entry.overBudgetWritten = Some(day)
          entry.overBudgetFailedAt = None
          true
        } catch {
          case NonFatal(e) =>
            entry.overBudgetFailedAt = Some(now)
            println(
              s"AiBudgetCache: recording that AI feature[${exceeded.feature.name}] went over budget on $day " +
                s"failed, will retry: $e"
            )
            false
        }
      }
      ()
    }
  }

  /** Called holding `entry`'s lock. On success the counts handed over are off the books; on failure they stay for the
    * next refresh, and so does the last reading.
    */
  private def refreshLocked(feature: AiFeature, entry: Entry, today: LocalDate, now: Instant): Unit = {
    val counts = entry.pending.values.toSeq.filterNot(_.isEmpty).sortBy(_.day)
    try {
      val reading = source.refresh(feature, today, counts)
      entry.reading = Some(AiBudgetStanding.Known(today, reading.budgetUsd, reading.spentUsd))
      entry.readAt = now
      entry.failedAt = None
      entry.pending = Map.empty
    } catch {
      case NonFatal(e) =>
        entry.failedAt = Some(now)
        println(s"AiBudgetCache: refreshing AI feature[${feature.name}] failed, keeping the last reading: $e")
    }
  }
}

object AiBudgetCache {
  val DefaultTtl: FiniteDuration = FiniteDuration(60, SECONDS)

  /** How long a failed refresh is left before the next call tries again. Shorter than the ttl, because a feature with
    * no reading of today refuses every call until one succeeds; long enough that a ledger that is down is asked a few
    * times a minute per feature rather than once per call.
    */
  val DefaultRetryAfterFailure: FiniteDuration = FiniteDuration(5, SECONDS)
}
