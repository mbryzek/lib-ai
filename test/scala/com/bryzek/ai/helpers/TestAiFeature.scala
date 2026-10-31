package helpers

import com.bryzek.ai.{AiAccount, AiFeature, AiFeatureKind}

/** The feature every client spec's calls are made for. Its budget is never consulted: those specs build their client
  * over an [[com.bryzek.ai.UnlimitedAiBudget]], and the budget itself is AiBudgetCacheSpec's subject.
  */
case object TestAiFeature
  extends AiFeature("test_feature", AiFeatureKind.Background, AiAccount.Personal, BigDecimal(10))

/** The feature a trial's re-runs are charged to -- an app's `model_upgrade_trial`. */
case object TestTrialFeature
  extends AiFeature("test_model_upgrade_trial", AiFeatureKind.Background, AiAccount.Personal, BigDecimal(10))
