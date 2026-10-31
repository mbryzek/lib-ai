package com.bryzek.ai

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AiFeatureSpec extends AnyWordSpec with Matchers {

  private class Feature(name: String, budget: BigDecimal)
    extends AiFeature(name, AiFeatureKind.Background, AiAccount.Personal, budget)

  "AiFeature" must {
    "accept a declared budget up to the $10 cap" in {
      new Feature("at_the_cap", BigDecimal(10)).declaredBudgetUsd mustBe BigDecimal(10)
      new Feature("free", BigDecimal(0)).declaredBudgetUsd mustBe BigDecimal(0)
    }

    "refuse a declared budget above the cap at construction" in {
      val e = intercept[IllegalArgumentException](new Feature("too_big", BigDecimal("10.01")))
      e.getMessage must include("too_big")
      e.getMessage must include("set by a person")
    }

    "refuse a negative declared budget" in {
      intercept[IllegalArgumentException](new Feature("negative", BigDecimal(-1)))
    }

    "refuse a name that is not lower snake case" in {
      Seq("", "Mixed", "has-dash", "1leading_digit", "has space").foreach { name =>
        withClue(name)(intercept[IllegalArgumentException](new Feature(name, BigDecimal(1))))
      }
    }

    "print as its name" in {
      new Feature("assistant_keywords", BigDecimal(1)).toString mustBe "assistant_keywords"
    }
  }

  "AiFeatureKind" must {
    "list every kind, so an app mapping them to its own enum is shown the new one" in {
      AiFeatureKind.all mustBe List(AiFeatureKind.Live, AiFeatureKind.Incoming, AiFeatureKind.Background)
    }
  }

  "AiFeatureCatalog" must {
    val a = new Feature("a", BigDecimal(1))
    val b = new Feature("b", BigDecimal(2))

    "find a feature by name" in {
      val catalog = new AiFeatureCatalog { override val all: Seq[AiFeature] = Seq(a, b) }
      catalog.validate()
      catalog.find("b") mustBe Some(b)
      catalog.find("c") mustBe None
    }

    "refuse two features sharing a name" in {
      val catalog = new AiFeatureCatalog {
        override val all: Seq[AiFeature] = Seq(a, new Feature("a", BigDecimal(3)))
      }
      intercept[IllegalArgumentException](catalog.validate()).getMessage must include("named a")
    }

    "refuse a feature over the cap when the catalog is built" in {
      intercept[IllegalArgumentException] {
        new AiFeatureCatalog {
          override val all: Seq[AiFeature] = Seq(a, new Feature("runaway", BigDecimal(540)))
        }
      }
    }
  }
}
