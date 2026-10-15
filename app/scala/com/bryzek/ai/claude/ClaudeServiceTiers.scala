package com.bryzek.ai.claude

import cats.data.Validated
import com.bryzek.claude.models.{ClaudeServiceTier, KnownClaudeServiceTier}

/** Lifts the tier that served a Claude request from the two shapes it arrives in -- the API's raw
  * `usage.service_tier`, and the string column `claude.responses.usage_service_tier` /
  * `claude.usage_daily_rollups.service_tier` -- into the `Known*` form everything downstream
  * matches on (`.claude/rules/scala.known.unions.mdc`).
  *
  * LENIENT, deliberately, in a direction chosen for cost. An unrecognized tier -- a value the API
  * adds before this library learns to price it -- becomes `None` rather than a failure, and
  * [[ClaudePricing.costUsd]] prices `None` at the full undiscounted rate. Over-reporting is the
  * safe error for everything that reads a cost: a daily budget refuses early rather than late, and
  * a spend report shows a cost that is too high rather than one that is silently too low. The
  * alternative -- guessing that an unknown tier is discounted -- is how a runaway spend goes
  * unnoticed.
  *
  * Logged because a tier priced as standard is
  * otherwise indistinguishable from a request that really was standard.
  */
object ClaudeServiceTiers {

  private val logger = org.slf4j.LoggerFactory.getLogger(getClass)

  /** From a Claude API response's `usage.service_tier`. */
  def fromApi(value: Option[ClaudeServiceTier]): Option[KnownClaudeServiceTier] = {
    value.flatMap { raw =>
      KnownClaudeServiceTier.validate(raw) match {
        case Validated.Valid(tier) => Some(tier)
        case Validated.Invalid(_) =>
          logger.warn(
            s"Claude reported an unrecognized usage.service_tier[$raw] - pricing this usage at the undiscounted rate"
          )
          None
      }
    }
  }

  /** From a persisted `service_tier` column. Goes through [[fromApi]] rather than duplicating the
    * lift: the column stores the raw wire value, so `ClaudeServiceTier.apply` round-trips exactly
    * what the API sent (including into `UNDEFINED`, which [[fromApi]] then declines).
    */
  def fromColumn(value: Option[String]): Option[KnownClaudeServiceTier] = {
    fromApi(value.map(ClaudeServiceTier(_)))
  }
}
