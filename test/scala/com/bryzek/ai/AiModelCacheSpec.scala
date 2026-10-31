package com.bryzek.ai

import com.bryzek.ai.claude.ClaudeModelFamily
import com.bryzek.claude.models.ClaudeModel
import helpers.{TestAiFeature, TestTrialFeature}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.{LocalDate, ZoneOffset}
import scala.collection.mutable

/** An app's model sets and trials a test scripts: what each read answers, whether it fails, and every claim. */
final class FakeAiModelSource(var reading: AiModelReading) extends AiModelSource {
  var failing: Boolean = false
  var claimFailing: Boolean = false

  /** Samples the source grants per trial and day across "every process"; past it a claim is refused. */
  var granted: mutable.Map[(LocalDate, String), Long] = mutable.Map.empty
  val reads: mutable.Buffer[LocalDate] = mutable.Buffer.empty
  val claims: mutable.Buffer[(LocalDate, String, Long)] = mutable.Buffer.empty

  override def read(feature: AiFeature, day: LocalDate): AiModelReading = {
    reads += day
    if (failing) throw new RuntimeException("model sets unavailable")
    reading
  }

  override def claimSample(feature: AiFeature, trial: AiModelTrial, day: LocalDate, limit: Long): Boolean = {
    claims += ((day, trial.id, limit))
    if (claimFailing) throw new RuntimeException("model sets unavailable")
    val n = granted.getOrElse((day, trial.id), 0L)
    if (n < limit) { granted((day, trial.id)) = n + 1; true }
    else false
  }
}

class AiModelCacheSpec extends AnyWordSpec with Matchers {

  private val Day = LocalDate.parse("2026-10-14")
  private val Noon = Day.atTime(12, 0).toInstant(ZoneOffset.UTC)
  private val feature = TestAiFeature

  private val haikuTrial = AiModelTrial(
    id = "trial-1",
    family = ClaudeModelFamily.Haiku,
    fromModel = ClaudeModel.ClaudeHaiku45,
    toModel = ClaudeModel.ClaudeHaiku55,
    chargedTo = TestTrialFeature
  )

  private def fixture(reading: AiModelReading = AiModelReading()) = {
    val clock = new MutableClock(Noon)
    val source = new FakeAiModelSource(reading)
    (clock, source, new AiModelCache(source, clock))
  }

  private val haikuSet = AiModelReading(inForce = Map(ClaudeModelFamily.Haiku -> ClaudeModel.ClaudeHaiku55))

  "resolve" must {
    "send the set version wherever the code names any version of the family" in {
      val (_, _, cache) = fixture(haikuSet)
      cache.resolve(feature, ClaudeModel.ClaudeHaiku45) mustBe ClaudeModel.ClaudeHaiku55
      cache.resolve(feature, ClaudeModel.ClaudeHaiku55) mustBe ClaudeModel.ClaudeHaiku55
    }

    "leave a family with no set on the code's model" in {
      val (_, _, cache) = fixture(haikuSet)
      cache.resolve(feature, ClaudeModel.ClaudeSonnet5) mustBe ClaudeModel.ClaudeSonnet5
    }

    "resolve a fallback list element by element, keeping its order and dropping a repeated attempt" in {
      val (_, _, cache) = fixture(haikuSet)
      cache.resolveAll(feature, Seq(ClaudeModel.ClaudeHaiku45, ClaudeModel.ClaudeSonnet5)) mustBe
        Seq(ClaudeModel.ClaudeHaiku55, ClaudeModel.ClaudeSonnet5)
      cache.resolveAll(
        feature,
        Seq(ClaudeModel.ClaudeHaiku45, ClaudeModel.ClaudeHaiku55, ClaudeModel.ClaudeOpus5)
      ) mustBe
        Seq(ClaudeModel.ClaudeHaiku55, ClaudeModel.ClaudeOpus5)
    }

    "never cross a family, whatever the source says" in {
      val (_, _, cache) =
        fixture(AiModelReading(inForce = Map(ClaudeModelFamily.Haiku -> ClaudeModel.ClaudeSonnet55)))
      cache.resolve(feature, ClaudeModel.ClaudeHaiku45) mustBe ClaudeModel.ClaudeHaiku45
    }

    "read the source once and answer from the cache for 60 seconds" in {
      val (clock, source, cache) = fixture()
      cache.resolve(feature, ClaudeModel.ClaudeHaiku45) mustBe ClaudeModel.ClaudeHaiku45
      source.reading = haikuSet
      clock.advanceSeconds(59)
      cache.resolve(feature, ClaudeModel.ClaudeHaiku45) mustBe ClaudeModel.ClaudeHaiku45
      source.reads.size mustBe 1
      clock.advanceSeconds(1)
      cache.resolve(feature, ClaudeModel.ClaudeHaiku45) mustBe ClaudeModel.ClaudeHaiku55
      source.reads.size mustBe 2
    }

    "answer a feature never read with its code's model rather than refusing" in {
      val (_, source, cache) = fixture(haikuSet)
      source.failing = true
      cache.resolve(feature, ClaudeModel.ClaudeHaiku45) mustBe ClaudeModel.ClaudeHaiku45
    }

    "keep the last reading standing through a failed read, across a UTC day" in {
      val (clock, source, cache) = fixture(haikuSet)
      cache.resolve(feature, ClaudeModel.ClaudeHaiku45) mustBe ClaudeModel.ClaudeHaiku55
      source.failing = true
      clock.now = Day.plusDays(1).atTime(0, 5).toInstant(ZoneOffset.UTC)
      cache.resolve(feature, ClaudeModel.ClaudeHaiku45) mustBe ClaudeModel.ClaudeHaiku55
    }

    "back off after a failed read rather than reading on every call" in {
      val (clock, source, cache) = fixture()
      source.failing = true
      cache.resolve(feature, ClaudeModel.ClaudeHaiku45)
      cache.resolve(feature, ClaudeModel.ClaudeHaiku45)
      source.reads.size mustBe 1
      clock.advanceSeconds(5)
      source.failing = false
      source.reading = haikuSet
      cache.resolve(feature, ClaudeModel.ClaudeHaiku45) mustBe ClaudeModel.ClaudeHaiku55
      source.reads.size mustBe 2
    }

    "behave as NoAiModels over NoAiModelSource" in {
      val cache = new AiModelCache(NoAiModelSource, new MutableClock(Noon))
      ClaudeModel.all.foreach(m => cache.resolve(feature, m) mustBe NoAiModels.resolve(feature, m))
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe None
    }
  }

  "claimTrialSample" must {
    "claim a sample of the open trial on the model that answered" in {
      val (_, source, cache) = fixture(AiModelReading(trials = Seq(haikuTrial)))
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe Some(haikuTrial)
      source.claims.toSeq mustBe Seq((Day, "trial-1", 5L))
      cache.pendingClaims(feature) mustBe Map("trial-1" -> 1L)
    }

    "claim nothing for a model no trial is from" in {
      val (_, source, cache) = fixture(AiModelReading(trials = Seq(haikuTrial)))
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku55) mustBe None
      cache.claimTrialSample(feature, ClaudeModel.ClaudeSonnet5) mustBe None
      source.claims mustBe empty
    }

    "take at most five a UTC day, without asking the source past its own count" in {
      val (_, source, cache) = fixture(AiModelReading(trials = Seq(haikuTrial)))
      (1 to 7).map(_ => cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45)).count(_.isDefined) mustBe 5
      source.claims.size mustBe 5
    }

    "count the samples other processes claimed today" in {
      val (_, source, cache) =
        fixture(AiModelReading(trials = Seq(haikuTrial), samplesToday = Map("trial-1" -> 4L)))
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe Some(haikuTrial)
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe None
      source.claims.size mustBe 1
    }

    "close the trial's day in this process once the source refuses a claim" in {
      val (_, source, cache) = fixture(AiModelReading(trials = Seq(haikuTrial)))
      source.granted((Day, "trial-1")) = 5L
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe None
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe None
      source.claims.size mustBe 1
    }

    "start again on the next UTC day" in {
      val (clock, source, cache) = fixture(AiModelReading(trials = Seq(haikuTrial)))
      (1 to 5).foreach(_ => cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45))
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe None
      clock.now = Day.plusDays(1).atTime(0, 1).toInstant(ZoneOffset.UTC)
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe Some(haikuTrial)
      source.reads.last mustBe Day.plusDays(1)
    }

    "not count this process's claims twice once a read has seen them" in {
      val (clock, source, cache) = fixture(AiModelReading(trials = Seq(haikuTrial)))
      (1 to 3).foreach(_ => cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45))
      source.reading = source.reading.copy(samplesToday = Map("trial-1" -> 3L))
      clock.advanceSeconds(60)
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe Some(haikuTrial)
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe Some(haikuTrial)
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe None
    }

    "take no sample when the claim fails, and back off before the next" in {
      val (clock, source, cache) = fixture(AiModelReading(trials = Seq(haikuTrial)))
      source.claimFailing = true
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe None
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe None
      source.claims.size mustBe 1
      source.claimFailing = false
      clock.advanceSeconds(5)
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe Some(haikuTrial)
    }

    "ignore a trial whose models are not two versions of its family" in {
      val acrossFamilies = haikuTrial.copy(id = "trial-2", toModel = ClaudeModel.ClaudeSonnet55)
      val ontoItself = haikuTrial.copy(id = "trial-3", toModel = ClaudeModel.ClaudeHaiku45)
      val (_, source, cache) = fixture(AiModelReading(trials = Seq(acrossFamilies, ontoItself)))
      cache.claimTrialSample(feature, ClaudeModel.ClaudeHaiku45) mustBe None
      source.claims mustBe empty
    }
  }
}
