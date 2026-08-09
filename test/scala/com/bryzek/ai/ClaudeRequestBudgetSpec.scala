package com.bryzek.ai.claude

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.duration.{FiniteDuration, MINUTES}

class ClaudeRequestBudgetSpec extends AnyWordSpec with Matchers {

  import ClaudeRequestBudget.*

  private def minutes(n: Int): FiniteDuration = FiniteDuration(n.toLong, MINUTES)

  /** Every max_tokens value any caller in the fleet actually sends, plus the edges. */
  private val AllBudgets: Seq[Long] =
    Seq(0L, 1L, 1024L, 4096L, 16000L, 30000L, 64000L, 128000L, Long.MaxValue)

  "timeoutFor" should {

    "floor small requests at the SDK-standard 10 minutes" in {
      timeoutFor(0L) must be(MinTimeout)
      timeoutFor(1024L) must be(MinTimeout)
      timeoutFor(4096L) must be(MinTimeout)
      timeoutFor(16000L) must be(MinTimeout)
      timeoutFor(MinTimeout.toSeconds * FloorOutputTokensPerSecond) must be(MinTimeout)
    }

    "scale with max_tokens above the floor" in {
      timeoutFor(30000L) must be > MinTimeout
      timeoutFor(30000L) must be < MaxTimeout
      timeoutFor(60000L) must be > timeoutFor(30000L)
    }

    "give the 64k drafting budget more wall clock than the generation actually needs" in {
      // 64,000 is SynthesizeInsightProcessor.SynthesisMaxTokens (and Investigate/Judge/PlanExecution's) --
      // the request that could not finish inside the old fixed 10 minutes (ISS-1229). Measured live at
      // 77.2 tok/s it needs ~13.8 minutes; the ceiling gives it 20.
      timeoutFor(64000L) must be(MaxTimeout)
      timeoutFor(64000L) must be > FiniteDuration(829, java.util.concurrent.TimeUnit.SECONDS)
    }

    "cap at MaxTimeout, without overflowing FiniteDuration on an absurd budget" in {
      timeoutFor(128000L) must be(MaxTimeout)
      timeoutFor(Long.MaxValue) must be(MaxTimeout)
    }

    "treat a negative budget as zero rather than producing a negative timeout" in {
      timeoutFor(-1L) must be(MinTimeout)
    }

    "never return a timeout shorter than the fixed one it replaced" in {
      AllBudgets.foreach { maxTokens =>
        withClue(s"maxTokens=$maxTokens: ") { timeoutFor(maxTokens) must be >= MinTimeout }
      }
    }
  }

  "attemptsFor" should {

    "keep the full retry count for requests that still get the baseline timeout" in {
      attemptsFor(0L) must be(MaxAttempts)
      attemptsFor(4096L) must be(MaxAttempts)
      attemptsFor(16000L) must be(MaxAttempts)
    }

    "trade attempts away as the per-request timeout grows" in {
      attemptsFor(30000L) must be < MaxAttempts
      attemptsFor(64000L) must be(1)
    }

    "never drop below one attempt" in {
      AllBudgets.foreach { maxTokens =>
        withClue(s"maxTokens=$maxTokens: ") { attemptsFor(maxTokens) must be >= 1 }
      }
    }
  }

  "envelopeFor" should {

    // This is the property that makes the timeout change safe to ship without touching a single
    // caller's async deadline: no request can now take longer per model than the 3-attempts-at-10-
    // minutes envelope every existing budget was already sized against.
    "never exceed the envelope callers were already sized against" in {
      Envelope must be(minutes(30))
      AllBudgets.foreach { maxTokens =>
        withClue(s"maxTokens=$maxTokens: ") { envelopeFor(maxTokens) must be <= Envelope }
      }
    }

    "spend less total wall clock on the 64k budget than the old three-attempt loop did" in {
      // The old behaviour: 3 x 10 minutes, none of which could finish. The new one: a single
      // 20-minute attempt that can.
      envelopeFor(64000L) must be(MaxTimeout)
      envelopeFor(64000L) must be < Envelope
    }

    "leave SynthesizeInsightProcessor's two sequential calls inside its 60-minute deadline" in {
      // 64k synthesis draft followed by the 16k action classification, on one task.
      envelopeFor(64000L) + envelopeFor(16000L) must be <= minutes(60)
    }
  }
}
