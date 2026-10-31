package com.bryzek.ai

import com.bryzek.ai.claude.ClaudeModelFamily
import com.bryzek.claude.models.ClaudeModel

import java.time.LocalDate

/** One feature trying one newer model in a family: while it is open, a sample of the feature's successful requests is
  * sent a second time to `toModel`, after the feature has its answer from `fromModel`, and the two answers are handed
  * to the app as a [[com.bryzek.ai.claude.ClaudeTrialPair]] for its judge.
  *
  * `id` is the app's own handle for the trial (its row id), carried onto every pair so the app files the pair under the
  * right trial. `chargedTo` is the feature the re-run is counted, budgeted and journaled under -- the app's
  * `model_upgrade_trial` -- so a trial never spends the tried feature's own budget.
  */
final case class AiModelTrial(
  id: String,
  family: ClaudeModelFamily,
  fromModel: ClaudeModel,
  toModel: ClaudeModel,
  chargedTo: AiFeature
) {

  /** Both models are in `family` and they differ. A trial that is not is ignored, never acted on: a re-run onto
    * another family or onto the model that already answered would compare nothing.
    */
  def isValid: Boolean =
    fromModel != toModel &&
      ClaudeModelFamily.of(fromModel).contains(family) &&
      ClaudeModelFamily.of(toModel).contains(family)
}

/** A feature's model in force and its open trials, consulted on every request -- what [[com.bryzek.ai.claude.ClaudeClient]]
  * and [[com.bryzek.ai.claude.ClaudeBatchClient]] ask before a model is sent. Each app implements it by constructing an
  * [[AiModelCache]] over an [[AiModelSource]] that reads its own tables; [[NoAiModels]] is the answer for an app that
  * has none, and the default of both clients.
  */
trait AiModels {

  /** The model `feature` sends when its code names `model`: the model set for `feature` in `model`'s family when there
    * is one, else `model` itself. A set always names a model of the same family, so a resolution never crosses one.
    */
  def resolve(feature: AiFeature, model: ClaudeModel): ClaudeModel

  /** `models` resolved one by one, so a fallback list keeps its order. Two entries that resolve to the same model are
    * one attempt, not two: re-sending a request to the model that just failed it is not a fallback.
    */
  final def resolveAll(feature: AiFeature, models: Seq[ClaudeModel]): Seq[ClaudeModel] =
    models.map(resolve(feature, _)).distinct

  /** The open trial of `feature` whose old model is `answeredBy`, when one of today's samples is still free -- and that
    * sample is claimed by this call. `None` when there is no such trial or today's samples are spent, and then nothing
    * is re-sent.
    */
  def claimTrialSample(feature: AiFeature, answeredBy: ClaudeModel): Option[AiModelTrial]
}

/** No model set and no trial: every request goes out on the model its code names, and nothing is ever re-sent. */
case object NoAiModels extends AiModels {
  override def resolve(feature: AiFeature, model: ClaudeModel): ClaudeModel = model
  override def claimTrialSample(feature: AiFeature, answeredBy: ClaudeModel): Option[AiModelTrial] = None
}

/** What an app's tables say about one feature, read by an [[AiModelCache]].
  *
  * `inForce` is the newest set per family; a family with no set is absent, and its code's model stands. `trials` is
  * every open trial of the feature. `samplesToday` is how many samples each trial (by id) has claimed on the day read,
  * across every process; a trial absent from it has claimed none.
  */
final case class AiModelReading(
  inForce: Map[ClaudeModelFamily, ClaudeModel] = Map.empty,
  trials: Seq[AiModelTrial] = Nil,
  samplesToday: Map[String, Long] = Map.empty
)

/** The app's half of an [[AiModelCache]]. Platform and acumen each implement it over their own model sets and trials.
  *
  * Both methods may block (they are database reads and writes) and may throw; the cache catches what they throw.
  */
trait AiModelSource {

  /** `feature`'s model sets, open trials and the samples each trial has claimed on `day`. */
  def read(feature: AiFeature, day: LocalDate): AiModelReading

  /** Claims one of `trial`'s samples for `day`, answering whether it fit under `limit` a day. Atomic across processes
    * (a conditional insert or a counted row under a unique key), because it is what holds a trial to its daily cap
    * when several pods sample the same feature: the cache asks only while its own count says a sample is free, so this
    * is written at most a few times a trial a day.
    */
  def claimSample(feature: AiFeature, trial: AiModelTrial, day: LocalDate, limit: Long): Boolean
}

/** A source with no sets and no trials: an [[AiModelCache]] over it behaves exactly as [[NoAiModels]]. */
case object NoAiModelSource extends AiModelSource {
  override def read(feature: AiFeature, day: LocalDate): AiModelReading = AiModelReading()
  override def claimSample(feature: AiFeature, trial: AiModelTrial, day: LocalDate, limit: Long): Boolean = false
}
