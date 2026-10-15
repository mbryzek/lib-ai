package com.bryzek.ai.claude

import com.bryzek.claude.models.{ClaudeCacheCreation, ClaudeServiceTier, ClaudeUsage, KnownClaudeServiceTier}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClaudePricingSpec extends AnyWordSpec with Matchers {

  /** Only the fields pricing reads; the rest of a response is irrelevant here. */
  private def usage(
    cacheCreationInputTokens: Option[Long] = None,
    cacheCreation: Option[ClaudeCacheCreation] = None,
    cacheReadInputTokens: Option[Long] = None,
    serviceTier: Option[ClaudeServiceTier] = None,
  ): ClaudeUsage = ClaudeUsage(
    inputTokens = 1000000,
    outputTokens = 1000000,
    cacheCreationInputTokens = cacheCreationInputTokens,
    cacheReadInputTokens = cacheReadInputTokens,
    cacheCreation = cacheCreation,
    serverToolUse = None,
    serviceTier = serviceTier,
  )

  /** [[ClaudePricing.costUsd]] for a model the table prices; every assertion below but the
    * unknown-model ones is about a priced model, so a `None` there is itself the failure.
    */
  private def cost(
    model: String,
    inputTokens: Long,
    outputTokens: Long,
    cacheCreationInputTokens: Long,
    cacheCreation1hInputTokens: Long,
    cacheReadInputTokens: Long,
    serviceTier: Option[KnownClaudeServiceTier],
  ): BigDecimal =
    ClaudePricing
      .costUsd(
        model,
        inputTokens,
        outputTokens,
        cacheCreationInputTokens,
        cacheCreation1hInputTokens,
        cacheReadInputTokens,
        serviceTier,
      )
      .getOrElse(fail(s"no price for model[$model]"))

  private def cost(model: String, usage: ClaudeUsage): BigDecimal =
    ClaudePricing.costUsd(model, usage).getOrElse(fail(s"no price for model[$model]"))

  private val Standard: Option[KnownClaudeServiceTier] = Some(KnownClaudeServiceTier.Standard)
  private val Batch: Option[KnownClaudeServiceTier] = Some(KnownClaudeServiceTier.Batch)
  private val Priority: Option[KnownClaudeServiceTier] = Some(KnownClaudeServiceTier.Priority)

  /** One million of each token type, half the cache write at the 1-hour TTL, so every multiplier
    * shows up undivided.
    */
  private def costOf(serviceTier: Option[KnownClaudeServiceTier]): BigDecimal =
    cost("claude-opus-5", 1000000, 1000000, 1000000, 500000, 1000000, serviceTier)

  "costUsd" must {
    "compute input + output + cache write + cache read cost for a known model" in {
      // Opus 5: $5/$25 per million; cache write 1.25x input at the 5-minute TTL, cache read 0.1x.
      val total = cost(
        model = "claude-opus-5",
        inputTokens = 1000000,
        outputTokens = 1000000,
        cacheCreationInputTokens = 1000000,
        cacheCreation1hInputTokens = 0,
        cacheReadInputTokens = 1000000,
        serviceTier = Some(KnownClaudeServiceTier.Standard),
      )
      total mustBe BigDecimal("5") + BigDecimal("25") + BigDecimal("6.25") + BigDecimal("0.5")
    }

    "price a 1-hour cache write at 2x input rather than 1.25x" in {
      // The whole point of ISS-1942: the same million written tokens cost $10 at the 1-hour TTL
      // against $6.25 at 5 minutes, and nothing but this column can tell the two apart.
      cost(
        model = "claude-opus-5",
        inputTokens = 0,
        outputTokens = 0,
        cacheCreationInputTokens = 1000000,
        cacheCreation1hInputTokens = 1000000,
        cacheReadInputTokens = 0,
        serviceTier = Standard,
      ) mustBe BigDecimal("10")
    }

    "treat the 1-hour count as a SUBSET of the write total, not an addition to it" in {
      // The relation the API guarantees (cache_creation_input_tokens == ephemeral_5m + ephemeral_1h),
      // and the one thing a caller could plausibly get backwards. A half-and-half million must cost
      // the average of the two rates -- if it were additive it would come to $16.25 instead.
      cost(
        model = "claude-opus-5",
        inputTokens = 0,
        outputTokens = 0,
        cacheCreationInputTokens = 1000000,
        cacheCreation1hInputTokens = 500000,
        cacheReadInputTokens = 0,
        serviceTier = Standard,
      ) mustBe BigDecimal("3.125") + BigDecimal("5")
    }

    "never price the 5-minute remainder below zero when the counts contradict each other" in {
      // Cannot happen from the API, which is exactly why it would go unnoticed: an unclamped
      // remainder would make a 1h-only write come out CHEAPER than the same tokens uncached.
      val contradictory = cost(
        model = "claude-opus-5",
        inputTokens = 0,
        outputTokens = 0,
        cacheCreationInputTokens = 0,
        cacheCreation1hInputTokens = 1000000,
        cacheReadInputTokens = 0,
        serviceTier = Standard,
      )
      contradictory mustBe BigDecimal("10")
      contradictory must be > BigDecimal(0)
    }

    "return 0 for zero usage" in {
      cost(
        model = "claude-sonnet-5",
        inputTokens = 0,
        outputTokens = 0,
        cacheCreationInputTokens = 0,
        cacheCreation1hInputTokens = 0,
        cacheReadInputTokens = 0,
        serviceTier = Some(KnownClaudeServiceTier.Standard),
      ) mustBe BigDecimal(0)
    }

    "refuse to price an unrecognized model id rather than call it $0" in {
      ClaudePricing.costUsd(
        model = "claude-not-a-real-model",
        inputTokens = 1000000,
        outputTokens = 1000000,
        cacheCreationInputTokens = 0,
        cacheCreation1hInputTokens = 0,
        cacheReadInputTokens = 0,
        serviceTier = Some(KnownClaudeServiceTier.Standard),
      ) mustBe None
      ClaudePricing.isPriced("claude-not-a-real-model") mustBe false
    }

    "price Claude Opus 5.5's cache reads at 0.05x input rather than the standard 0.1x" in {
      // The reason [[ClaudePricing.Rate]] carries the read rate outright instead of deriving it from the input
      // rate. Opus 5.5 is $4/$20 per million with cache reads at $0.20 -- 0.05x, half what every other model here
      // charges. Derived from the input rate it would have been $0.40, a silent 2x over-report in the token
      // category an agent workload spends most of, on the model this fleet's own sessions run.
      cost("claude-opus-5-5", 0, 0, 0, 0, 1000000, Standard) mustBe BigDecimal("0.20")

      val full = cost(
        model = "claude-opus-5-5",
        inputTokens = 1000000,
        outputTokens = 1000000,
        cacheCreationInputTokens = 1000000,
        cacheCreation1hInputTokens = 0,
        cacheReadInputTokens = 1000000,
        serviceTier = Standard,
      )
      // $4 input + $20 output + $5 write at 1.25x + $0.20 read.
      full mustBe BigDecimal("4") + BigDecimal("20") + BigDecimal("5") + BigDecimal("0.20")

      // The writes are still plain multiples of input; only the read departs.
      cost("claude-opus-5-5", 0, 0, 1000000, 1000000, 0, Standard) mustBe BigDecimal("8")
    }

    "price Claude Sonnet 5 at the published $2/$10, not the cancelled $3/$15 increase" in {
      // ISS-13608: the table carried $3/$15, the increase once scheduled for September 2026 and then cancelled,
      // over-reporting every claude-sonnet-5 rollup row by half again.
      cost("claude-sonnet-5", 1000000, 0, 0, 0, 0, Standard) mustBe BigDecimal("2")
      cost("claude-sonnet-5", 0, 1000000, 0, 0, 0, Standard) mustBe BigDecimal("10")
      cost("claude-sonnet-5", 0, 0, 1000000, 0, 0, Standard) mustBe BigDecimal("2.50")
      cost("claude-sonnet-5", 0, 0, 1000000, 1000000, 0, Standard) mustBe BigDecimal("4")
      cost("claude-sonnet-5", 0, 0, 0, 0, 1000000, Standard) mustBe BigDecimal("0.20")
    }

    "price Claude Fable 5.1 at $10/$50 with cache reads at 0.025x input" in {
      // Same input and output rate as claude-fable-5, but its reads are $0.25 rather than $1.00 -- derived from the
      // input rate they would be a silent 4x over-report.
      cost("claude-fable-5-1", 1000000, 1000000, 0, 0, 0, Standard) mustBe BigDecimal("60")
      cost("claude-fable-5-1", 0, 0, 0, 0, 1000000, Standard) mustBe BigDecimal("0.25")
      cost("claude-fable-5-1", 0, 0, 1000000, 1000000, 0, Standard) mustBe BigDecimal("20")
    }

    "leave every other model on the standard cache-read multiplier" in {
      // The other half of the claim above: adding a per-model read rate must not have moved anything that was
      // already priced. Each of these is 0.1x its own input rate, exactly as the derived version charged.
      Seq(
        "claude-opus-5" -> BigDecimal("0.50"),
        "claude-sonnet-5" -> BigDecimal("0.20"),
        "claude-haiku-4-5" -> BigDecimal("0.10"),
        "claude-fable-5" -> BigDecimal("1.00"),
      ).foreach { case (model, readCost) =>
        withClue(model)(cost(model, 0, 0, 0, 0, 1000000, Standard) mustBe readCost)
      }
    }

    "name every model whose cache-read multiplier is not the standard one" in {
      // What platform's core.invariants.ClaudeCacheInvariants builds its per-model comparison from. Derived from the rate
      // table rather than listed, so this asserts the derivation rather than a second copy of the list.
      ClaudePricing.NonStandardCacheReadMultipliers mustBe Map(
        "claude-opus-5-5" -> BigDecimal("0.05"),
        "claude-fable-5-1" -> BigDecimal("0.025"),
      )
    }

    "price different models differently" in {
      val opusCost = cost("claude-opus-5", 1000000, 0, 0, 0, 0, Standard)
      val haikuCost = cost("claude-haiku-4-5", 1000000, 0, 0, 0, 0, Standard)
      opusCost must be > haikuCost
    }

    "price a dated snapshot id at its alias' rate" in {
      // What the API actually echoes back for a claude-haiku-4-5 request, and what therefore lands
      // in claude.responses.model and rolls up (ISS-1925).
      cost("claude-haiku-4-5-20251001", 1000000, 1000000, 0, 0, 0, Standard) mustBe
        cost("claude-haiku-4-5", 1000000, 1000000, 0, 0, 0, Standard)
    }

    "price a dated snapshot of every priced model at its alias' rate" in {
      Seq(
        "claude-opus-5-5",
        "claude-opus-5",
        "claude-sonnet-5",
        "claude-haiku-4-5",
        "claude-fable-5-1",
        "claude-fable-5",
      ).foreach { alias =>
        withClue(alias) {
          cost(s"$alias-20260811", 1000000, 1000000, 1000000, 500000, 1000000, Standard) mustBe
            cost(alias, 1000000, 1000000, 1000000, 500000, 1000000, Standard)
          cost(alias, 1000000, 0, 0, 0, 0, Standard) must be > BigDecimal(0)
        }
      }
    }

    "price a batch request at half a standard one" in {
      // The whole point of ISS-2064. A batched response reports RAW, undiscounted token counts, so
      // the two calls below differ ONLY in the tier -- identical model, identical counts -- and
      // must still come out 2:1. Anything else means batched usage is being over-reported.
      costOf(Batch) mustBe costOf(Standard) / 2
    }

    "discount every token type for a batch request, not just input" in {
      // Anthropic's batch discount is 50% off ALL token types. Asserted one type at a time -- each
      // case zeroes the other three -- so a future multiplier applied to only some of them fails
      // here, rather than silently under-discounting a cache-heavy caller. The insight pipeline is
      // exactly that: it caches its system prefix on every stage.
      val oneMillionOf: Map[String, Option[KnownClaudeServiceTier] => BigDecimal] = Map(
        "input" -> (t => cost("claude-opus-5", 1000000, 0, 0, 0, 0, t)),
        "output" -> (t => cost("claude-opus-5", 0, 1000000, 0, 0, 0, t)),
        "cacheWrite5m" -> (t => cost("claude-opus-5", 0, 0, 1000000, 0, 0, t)),
        "cacheWrite1h" -> (t => cost("claude-opus-5", 0, 0, 1000000, 1000000, 0, t)),
        "cacheRead" -> (t => cost("claude-opus-5", 0, 0, 0, 0, 1000000, t)),
      )

      oneMillionOf.foreach { case (tokenType, priced) =>
        withClue(tokenType) {
          priced(Standard) must be > BigDecimal(0)
          priced(Batch) mustBe priced(Standard) / 2
        }
      }
    }

    "price an absent tier as standard" in {
      // Every claude.responses row written before ISS-2064 carries no tier, and so does every
      // rollup aggregated from them. Undiscounted is the safe reading: over-reporting makes the
      // weekly spend breaker trip early rather than late.
      costOf(None) mustBe costOf(Standard)
    }

    "price priority at the undiscounted rate" in {
      // Anthropic publishes no Priority Tier multiplier, so 1x is a documented under-report rather
      // than an invented number -- see ClaudePricing.tierMultiplier. Pinned so that changing it
      // has to be a deliberate edit here too.
      costOf(Priority) mustBe costOf(Standard)
    }

    "leave an unrecognized model unpriced regardless of tier" in {
      // The unknown-model branch short-circuits before the multiplier; assert the tier cannot
      // resurrect a price from a model that has none.
      ClaudePricing.costUsd("claude-not-a-real-model", 1000000, 1000000, 0, 0, 0, Batch) mustBe None
    }
  }

  "costUsd from a ClaudeUsage" must {

    "read the 1-hour split out of usage.cache_creation" in {
      cost(
        "claude-opus-5",
        usage(
          cacheCreationInputTokens = Some(1000000),
          cacheCreation = Some(ClaudeCacheCreation(ephemeral5mInputTokens = 0, ephemeral1hInputTokens = 1000000)),
        ),
      ) mustBe cost(
        model = "claude-opus-5",
        inputTokens = 1000000,
        outputTokens = 1000000,
        cacheCreationInputTokens = 1000000,
        cacheCreation1hInputTokens = 1000000,
        cacheReadInputTokens = 0,
        serviceTier = None,
      )
    }

    "charge the 5-minute rate when the response reports a write but no split" in {
      // What every response looked like before lib-ai modelled cache_creation, and what a caller
      // that has not adopted the longer TTL still gets. Absent is 0, not unknown.
      cost("claude-opus-5", usage(cacheCreationInputTokens = Some(1000000))) mustBe
        cost(
          model = "claude-opus-5",
          inputTokens = 1000000,
          outputTokens = 1000000,
          cacheCreationInputTokens = 1000000,
          cacheCreation1hInputTokens = 0,
          cacheReadInputTokens = 0,
          serviceTier = None,
        )
    }

    "charge nothing for cache when the response reports no cache activity at all" in {
      cost("claude-opus-5", usage()) mustBe
        cost("claude-opus-5", 1000000, 1000000, 0, 0, 0, None)
    }

    "price the tier the response reported, without the call site passing it" in {
      // The reason this overload exists at all now that both ISS-1942 and ISS-2064 landed: a call
      // site that hands over the live usage cannot forget the tier, so a batched response is halved
      // even though its token counts are raw and identical to a standard one's.
      cost("claude-opus-5", usage(serviceTier = Some(ClaudeServiceTier.Batch))) mustBe
        cost("claude-opus-5", usage(serviceTier = Some(ClaudeServiceTier.Standard))) / 2
    }

    "price a tier this library does not recognize at the undiscounted rate" in {
      // ClaudeServiceTiers declines an UNDEFINED tier rather than guessing, and undiscounted is the
      // direction that refuses on a budget early rather than late.
      cost("claude-opus-5", usage(serviceTier = Some(ClaudeServiceTier.UNDEFINED("flex")))) mustBe
        cost("claude-opus-5", usage(serviceTier = Some(ClaudeServiceTier.Standard)))
    }
  }

  "isPriced" must {
    "price every model the client can be asked to call" in {
      // ClaudeClient refuses a model it cannot price, so a model in the generated enum with no row
      // here is a model no caller can use.
      com.bryzek.claude.models.KnownClaudeModel.all.foreach { m =>
        withClue(m.toString)(ClaudePricing.isPriced(m.toClaudeModel) mustBe true)
      }
    }

    "price Claude Sonnet 5.5 at $2/$10 with standard cache reads" in {
      cost("claude-sonnet-5-5", 1000000, 1000000, 0, 0, 0, Standard) mustBe BigDecimal("12")
      cost("claude-sonnet-5-5", 0, 0, 0, 0, 1000000, Standard) mustBe BigDecimal("0.20")
    }
  }

  "normalize" must {
    "strip a trailing dated snapshot suffix" in {
      ClaudePricing.normalize("claude-haiku-4-5-20251001") mustBe "claude-haiku-4-5"
      ClaudePricing.normalize("claude-sonnet-4-5-20250929") mustBe "claude-sonnet-4-5"
    }

    "leave an undated id alone" in {
      ClaudePricing.normalize("claude-opus-5") mustBe "claude-opus-5"
    }

    "leave a suffix that is not an 8 digit date alone" in {
      // A future model that merely extends an existing name must stay unknown rather than inherit
      // the older model's rate.
      Seq("claude-opus-5-1", "claude-opus-5-2026081", "claude-opus-5-202608111").foreach { id =>
        withClue(id)(ClaudePricing.normalize(id) mustBe id)
      }
      ClaudePricing.isPriced("claude-opus-5-1") mustBe false
    }
  }
}
