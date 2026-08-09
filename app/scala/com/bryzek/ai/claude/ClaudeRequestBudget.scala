package com.bryzek.ai.claude

import scala.concurrent.duration.{FiniteDuration, MINUTES, SECONDS}

/** How long one Claude call is allowed to take, and how many HTTP attempts that is spread across.
  *
  * Both numbers are derived from the request's own `max_tokens`, and they move in opposite directions on purpose. We
  * send non-streaming requests, so an entire generation has to land inside a single HTTP request, and how long that
  * takes is set almost entirely by `max_tokens`: at adaptive thinking + `effort: high` the model will spend most of a
  * large budget thinking before it emits any text. A fixed per-request timeout therefore cannot serve both a 4k
  * classification and a 64k drafting call.
  *
  * ISS-1229 is what that costs. The insight-synthesis draft asks for 64,000 output tokens; measured against the live
  * API on 2026-08-09 with that exact request shape (claude-sonnet-5, adaptive thinking, `effort: high`, no tools) the
  * model sustains 77.2 output tokens/second, which puts a full budget at 829s -- 13.8 minutes. The client allowed 10.
  * So the call could not succeed, and, being a timeout, it was retried as a transient transport failure: three
  * identical `Request timeout to api.anthropic.com ... after 600000 ms` failures, then four task-level retries on top,
  * all paying for a full generation and none of them able to finish.
  *
  * ==Why attempts shrink as the timeout grows==
  *
  * [[MaxAttempts]] at [[MinTimeout]] is the budget every caller was already sized against, and callers DO size against
  * it -- platform's `SynthesizeInsightProcessor` sets a 60-minute async deadline explicitly so a slow call fails in the
  * client with a clean typed error rather than being guillotined mid-retry. Simply lengthening the timeout would have
  * tripled that envelope and silently invalidated four processors' deadlines at once.
  *
  * So [[envelopeFor]] is held at or below [[Envelope]] instead: the same wall clock is spent on FEWER, BIGGER attempts.
  * A 64k request gets one 20-minute attempt rather than three 10-minute ones that cannot succeed -- less total time
  * than before, and it can actually finish. Retries are kept exactly where they still buy something: small requests,
  * where a timeout really is more likely to be a transport blip than the size of the job. Anything that genuinely needs
  * more than [[MaxTimeout]] of wall clock in one HTTP request wants streaming, not a bigger number here.
  *
  * Note the ceiling is a client-side budget only. The Play WS *idle* timeout is a separate, independently binding limit
  * -- a non-streaming turn sends no bytes at all, so whichever is smaller kills the request. Consumers must keep
  * `play.ws.timeout.idle` at or above [[MaxTimeout]] (platform's `WsIdleTimeoutSpec` asserts that floor).
  */
object ClaudeRequestBudget {

  /** Floor for the per-request timeout, and what every small request gets. Matches the Anthropic SDKs' own default, and
    * is the value this client used for every request before ISS-1229.
    */
  val MinTimeout: FiniteDuration = FiniteDuration(10, MINUTES)

  /** Ceiling for the per-request timeout. 20 minutes covers the largest budget anything here asks for (64k, measured at
    * 13.8 minutes) with ~45% headroom, and keeps a single attempt inside the tightest caller deadline in platform
    * (`PlanExecutionProcessor`, 20 minutes).
    */
  val MaxTimeout: FiniteDuration = FiniteDuration(20, MINUTES)

  /** Total HTTP attempts for a request at [[MinTimeout]] (initial call + retries). */
  val MaxAttempts: Int = 3

  /** The per-model wall-clock envelope every caller's deadline was already sized against: [[MaxAttempts]] attempts at
    * [[MinTimeout]]. [[envelopeFor]] never exceeds it, which is what makes the timeout change safe to ship without
    * touching a single caller's budget.
    */
  val Envelope: FiniteDuration = MinTimeout * MaxAttempts.toLong

  /** Deliberately pessimistic output-token rate used to turn `max_tokens` into a wall-clock budget.
    *
    * A little over half the 77.2 tok/s measured above, which leaves ~45% headroom at 64k for a loaded hour, a slower
    * model, or a mid-call fallback to a larger one. Production corroborates the measurement: the one
    * `insight_synthesize` response that did complete in the seven days to 2026-08-09 emitted 57,960 output tokens
    * inside the old 10-minute timeout.
    */
  val FloorOutputTokensPerSecond: Long = 45L

  /** Wall-clock budget for one non-streaming request generating up to `maxTokens` tokens, clamped to [[[MinTimeout]],
    * [[MaxTimeout]]].
    *
    * The clamp is applied to the SECOND COUNT, before any [[FiniteDuration]] is built: `Duration` tops out around 292
    * years in nanos, so constructing one from an unclamped `maxTokens / rate` throws on an absurd budget instead of
    * returning the ceiling.
    */
  def timeoutFor(maxTokens: Long): FiniteDuration = {
    val seconds = math.max(0L, maxTokens) / FloorOutputTokensPerSecond
    if (seconds <= MinTimeout.toSeconds) MinTimeout
    else if (seconds >= MaxTimeout.toSeconds) MaxTimeout
    else FiniteDuration(seconds, SECONDS)
  }

  /** HTTP attempts allowed for a request generating up to `maxTokens` tokens: as many as fit in [[Envelope]] at that
    * request's own timeout, never more than [[MaxAttempts]] and never fewer than one.
    */
  def attemptsFor(maxTokens: Long): Int =
    math.max(1, math.min(MaxAttempts.toLong, Envelope.toSeconds / timeoutFor(maxTokens).toSeconds).toInt)

  /** Worst-case wall clock for one call at `maxTokens`, for callers reasoning about their own deadline. */
  def envelopeFor(maxTokens: Long): FiniteDuration = timeoutFor(maxTokens) * attemptsFor(maxTokens).toLong
}
