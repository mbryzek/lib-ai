package com.bryzek.ai

import java.time.{Clock, Instant, LocalDate, ZoneOffset}
import scala.util.control.NoStackTrace

/** The day a budget is counted over is the UTC day: Anthropic's daily cost buckets start at 00:00:00Z and its workspace
  * spend limits reset then too.
  */
object AiBudgetDay {
  def today(clock: Clock): LocalDate = LocalDate.now(clock.withZone(ZoneOffset.UTC))

  /** When `day`'s budget resets: the next 00:00 UTC. */
  def resetsAt(day: LocalDate): Instant = day.plusDays(1).atStartOfDay(ZoneOffset.UTC).toInstant
}

/** What an [[AiBudget]] knows about a feature's day right now. */
sealed trait AiBudgetStanding

object AiBudgetStanding {

  /** `spentUsd` of `budgetUsd` spent so far on `day`. Spend can exceed the budget: the call that crosses the line
    * completes, and the next one is refused.
    */
  final case class Known(day: LocalDate, budgetUsd: BigDecimal, spentUsd: BigDecimal) extends AiBudgetStanding {
    def remainingUsd: BigDecimal = budgetUsd - spentUsd
    def exhausted: Boolean = remainingUsd <= 0
    def resetsAt: Instant = AiBudgetDay.resetsAt(day)
  }

  /** Today's number has never been read in this process. A feature in this state refuses: there is no fail-open on an
    * unknown number.
    */
  case object Unknown extends AiBudgetStanding
}

/** Why a call was not made. A typed failure rather than a [[com.bryzek.ai.claude.ClaudeError]]: nothing was sent, so
  * nothing is retried, no fallback model is tried and nothing is journaled. [[com.bryzek.ai.claude.ClaudeClient]] fails
  * the call's `Future` with one, and a caller recovers it by type.
  */
sealed abstract class AiBudgetRefusal(message: String) extends RuntimeException(message) with NoStackTrace {
  def feature: AiFeature
}

/** `feature` has spent `spentUsd` of its `budgetUsd` today, so the call was refused. A background caller defers its task
  * to `resetsAt`; a live caller tells the waiting person the feature is unavailable until then.
  */
final case class AiBudgetExceeded(
  feature: AiFeature,
  spentUsd: BigDecimal,
  budgetUsd: BigDecimal,
  resetsAt: Instant
) extends AiBudgetRefusal(
    s"AI feature[${feature.name}] is over its daily budget: spent $$$spentUsd of $$$budgetUsd; resets at $resetsAt"
  )

/** `feature`'s spend today has never been read in this process (its first refresh failed), so the call was refused
  * rather than made against an unknown number. Transient: the next refresh that succeeds clears it, so a caller retries
  * on its ordinary schedule rather than waiting for the day to reset.
  */
final case class AiBudgetUnavailable(feature: AiFeature)
  extends AiBudgetRefusal(
    s"AI feature[${feature.name}] has no reading of today's spend yet, so the call was refused rather than made " +
      "against an unknown number"
  )

/** Today's spend against today's budget, per [[AiFeature]], consulted before every call.
  *
  * Each app implements it over its own ledger, normally by constructing an [[AiBudgetCache]] over an [[AiBudgetSource]]
  * that reads that ledger -- the cache is what keeps the check off the database on every call.
  */
trait AiBudget {

  /** What is known about `feature`'s day now. */
  def remaining(feature: AiFeature): AiBudgetStanding

  /** This process just spent `costUsd` on a call for `feature`. */
  def spent(feature: AiFeature, costUsd: BigDecimal): Unit

  /** This process refused a call for `exceeded.feature`. Counted locally and added to the day's ledger on the next
    * refresh, so a refusing loop costs no write per call.
    */
  def refused(exceeded: AiBudgetExceeded): Unit

  /** This process deferred a task of `feature` to the reset after a refusal. Counted the way [[refused]] is. */
  def deferred(feature: AiFeature): Unit

  /** `Right` with the standing when `feature` may make a call now, `Left` with the refusal (already recorded through
    * [[refused]]) when it may not. What [[com.bryzek.ai.claude.ClaudeClient]] asks before every request, and what a
    * caller asks before it pulls a batch of work or submits a Message Batch, so a batch is not half done when the
    * refusal comes.
    */
  final def check(feature: AiFeature): Either[AiBudgetRefusal, AiBudgetStanding.Known] =
    remaining(feature) match {
      case AiBudgetStanding.Unknown => Left(AiBudgetUnavailable(feature))
      case known: AiBudgetStanding.Known if known.exhausted =>
        val exceeded = AiBudgetExceeded(feature, known.spentUsd, known.budgetUsd, known.resetsAt)
        refused(exceeded)
        Left(exceeded)
      case known: AiBudgetStanding.Known => Right(known)
    }
}

/** A budget that never refuses: for tests and for a sandbox client whose calls cost nothing. Never the budget of a
  * client that reaches the API -- that is the fail-open this whole seam exists to remove.
  */
class UnlimitedAiBudget(clock: Clock = Clock.systemUTC()) extends AiBudget {
  override def remaining(feature: AiFeature): AiBudgetStanding =
    AiBudgetStanding.Known(AiBudgetDay.today(clock), budgetUsd = BigDecimal(Long.MaxValue), spentUsd = BigDecimal(0))
  override def spent(feature: AiFeature, costUsd: BigDecimal): Unit = ()
  override def refused(exceeded: AiBudgetExceeded): Unit = ()
  override def deferred(feature: AiFeature): Unit = ()
}
