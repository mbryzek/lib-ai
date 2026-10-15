package com.bryzek.ai.claude

import com.bryzek.claude.models.{ClaudeServiceTier, KnownClaudeServiceTier}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClaudeServiceTiersSpec extends AnyWordSpec with Matchers {

  "fromApi" must {
    "lift every tier the API can report" in {
      // KnownClaudeServiceTier.all is exhaustive by construction, so a variant added by a future
      // lib-ai fails here rather than silently falling through to the undiscounted rate.
      KnownClaudeServiceTier.all.foreach { tier =>
        withClue(tier.toString) {
          ClaudeServiceTiers.fromApi(Some(ClaudeServiceTier(tier.toString))) mustBe Some(tier)
        }
      }
    }

    "return None for a tier this library does not recognize" in {
      // What a newer lib-ai adding a tier looks like from here. None prices at the undiscounted
      // rate, which over-reports rather than under-reports -- see ClaudeServiceTiers.
      ClaudeServiceTiers.fromApi(Some(ClaudeServiceTier("flex"))) mustBe None
    }

    "return None when the response carried no tier" in {
      ClaudeServiceTiers.fromApi(None) mustBe None
    }
  }

  "fromColumn" must {
    "round-trip what DbClaudeStore persists" in {
      // The store writes `usage.serviceTier.map(_.toString)`; this is the other half of that trip.
      KnownClaudeServiceTier.all.foreach { tier =>
        withClue(tier.toString) {
          ClaudeServiceTiers.fromColumn(Some(tier.toString)) mustBe Some(tier)
        }
      }
    }

    "return None for a null column and for a value it cannot parse" in {
      // Null is every row written before ISS-2064 added the column.
      ClaudeServiceTiers.fromColumn(None) mustBe None
      ClaudeServiceTiers.fromColumn(Some("flex")) mustBe None
    }
  }
}
