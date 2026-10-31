package com.bryzek.ai

import com.bryzek.ai.claude.ClaudeModelFamily
import com.bryzek.claude.models.ClaudeModel

import java.time.{Clock, Instant, LocalDate}
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.util.control.NonFatal

/** The 60-second per-feature model cache, both apps' [[AiModels]], beside [[AiBudgetCache]] and shaped like it.
  *
  * [[resolve]] answers from a reading at most `ttl` old; past that it reads the [[AiModelSource]] again. So a model
  * set, a move or an Undo reaches every request of the feature within a minute, and the check stays off the database
  * on every call.
  *
  * A read that fails leaves the last reading standing until one succeeds (tried again after `retryAfterFailure`).
  * Unlike a budget, a model set does not expire with the day, so yesterday's reading is still the right answer. A
  * feature never read is answered as having no set and no trial: its code's model goes out, which is the model that
  * feature ran on before any of this existed, and still answers. Nothing is refused for want of a reading.
  *
  * Samples: a trial takes at most `samplesPerDay` a UTC day. The reading carries what every process has claimed today
  * and the cache adds its own claims since, so it asks [[AiModelSource.claimSample]] only while that count says one is
  * free; the claim is the cross-process cap, and a refused claim closes the trial's day in this process.
  *
  * Thread-safe. A feature's read and claims run under that feature's own lock, so concurrent calls for one feature
  * produce one read, and features never wait on each other.
  */
class AiModelCache(
  source: AiModelSource,
  clock: Clock = Clock.systemUTC(),
  ttl: FiniteDuration = AiModelCache.DefaultTtl,
  retryAfterFailure: FiniteDuration = AiModelCache.DefaultRetryAfterFailure,
  samplesPerDay: Long = AiModelCache.DefaultSamplesPerDay
) extends AiModels {

  private final class Entry {
    var reading: Option[(LocalDate, AiModelReading)] = None
    var readAt: Instant = Instant.EPOCH
    var failedAt: Option[Instant] = None
    // This process's claims since the last successful read, per day and trial id. A read replaces them: it counts
    // every claim already written, these included.
    var claimed: Map[(LocalDate, String), Long] = Map.empty
    // Trial days whose claim was refused by the source: full across every process, so not asked again that day.
    var full: Set[(LocalDate, String)] = Set.empty
    var claimFailedAt: Option[Instant] = None
  }

  private val entries = new ConcurrentHashMap[String, Entry]()

  private def entryFor(feature: AiFeature): Entry =
    entries.computeIfAbsent(feature.name, _ => new Entry)

  override def resolve(feature: AiFeature, model: ClaudeModel): ClaudeModel =
    ClaudeModelFamily.of(model) match {
      case None => model
      case Some(family) =>
        reading(feature).inForce
          .get(family)
          .filter(set => ClaudeModelFamily.of(set).contains(family))
          .getOrElse(model)
    }

  override def claimTrialSample(feature: AiFeature, answeredBy: ClaudeModel): Option[AiModelTrial] = {
    val entry = entryFor(feature)
    entry.synchronized {
      val now = clock.instant()
      val today = AiBudgetDay.today(clock)
      refreshIfStaleLocked(feature, entry, today, now)
      val (readDay, current) = entry.reading.getOrElse((today, AiModelReading()))
      current.trials.find(t => t.isValid && t.fromModel == answeredBy).filter { trial =>
        val key = (today, trial.id)
        val counted = (if (readDay == today) current.samplesToday.getOrElse(trial.id, 0L) else 0L) +
          entry.claimed.getOrElse(key, 0L)
        val backingOff = entry.claimFailedAt.exists(at => now.isBefore(at.plusMillis(retryAfterFailure.toMillis)))
        !entry.full.contains(key) && counted < samplesPerDay && !backingOff && claimLocked(feature, entry, trial, key)
      }
    }
  }

  /** Today's claims not yet seen in a reading, per trial id -- for tests and for an app's own diagnostics. */
  def pendingClaims(feature: AiFeature): Map[String, Long] = {
    val entry = entryFor(feature)
    val today = AiBudgetDay.today(clock)
    entry.synchronized(entry.claimed.collect { case ((day, id), n) if day == today => id -> n })
  }

  private def reading(feature: AiFeature): AiModelReading = {
    val entry = entryFor(feature)
    entry.synchronized {
      refreshIfStaleLocked(feature, entry, AiBudgetDay.today(clock), clock.instant())
      entry.reading.map(_._2).getOrElse(AiModelReading())
    }
  }

  /** Called holding `entry`'s lock. A new UTC day forces a read inside the ttl, because the day's sample counts are. */
  private def refreshIfStaleLocked(feature: AiFeature, entry: Entry, today: LocalDate, now: Instant): Unit = {
    val fresh = entry.reading.exists(_._1 == today) && now.isBefore(entry.readAt.plusMillis(ttl.toMillis))
    val backingOff = entry.failedAt.exists(at => now.isBefore(at.plusMillis(retryAfterFailure.toMillis)))
    if (!fresh && !backingOff) {
      try {
        val read = source.read(feature, today)
        read.trials.filterNot(_.isValid).foreach { t =>
          println(
            s"AiModelCache: ignoring AI feature[${feature.name}] trial[${t.id}] from ${t.fromModel} to ${t.toModel}: " +
              s"both models must be distinct versions of the ${t.family} family"
          )
        }
        entry.reading = Some((today, read))
        entry.readAt = now
        entry.failedAt = None
        entry.claimed = Map.empty
        entry.full = entry.full.filter(_._1 == today)
      } catch {
        case NonFatal(e) =>
          entry.failedAt = Some(now)
          println(s"AiModelCache: reading AI feature[${feature.name}]'s models failed, keeping the last reading: $e")
      }
    }
  }

  /** Called holding `entry`'s lock. A claim that throws takes no sample, and the next is tried after the backoff. */
  private def claimLocked(feature: AiFeature, entry: Entry, trial: AiModelTrial, key: (LocalDate, String)): Boolean =
    try {
      val granted = source.claimSample(feature, trial, key._1, samplesPerDay)
      entry.claimFailedAt = None
      if (granted) entry.claimed = entry.claimed.updated(key, entry.claimed.getOrElse(key, 0L) + 1)
      else entry.full = entry.full + key
      granted
    } catch {
      case NonFatal(e) =>
        entry.claimFailedAt = Some(clock.instant())
        println(s"AiModelCache: claiming a sample of AI feature[${feature.name}] trial[${trial.id}] failed: $e")
        false
    }
}

object AiModelCache {
  val DefaultTtl: FiniteDuration = FiniteDuration(60, SECONDS)

  /** How long a failed read or claim is left before the next call tries again. */
  val DefaultRetryAfterFailure: FiniteDuration = FiniteDuration(5, SECONDS)

  /** Samples a trial takes a UTC day, so 30 pairs span six days or more of a feature's traffic. */
  val DefaultSamplesPerDay: Long = 5L
}
