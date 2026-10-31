package com.bryzek.ai

/** Whether a person is waiting on each of an [[AiFeature]]'s calls, and if not, whether its work can wait. It decides
  * how going over budget is surfaced, never how the budget is counted: an incoming or background feature's work waits
  * for the next UTC day, and a live feature's caller tells the waiting person the feature is unavailable until then.
  *
  * Incoming and background are kept apart so that a backlog never shares a feature, and so a budget, with work as it
  * arrives: a backlog worked in a loop spends whatever budget it is given, and on a shared one leaves the day's incoming
  * work waiting until the reset.
  */
sealed trait AiFeatureKind

object AiFeatureKind {

  /** A person is waiting on each call (club chat, say). */
  case object Live extends AiFeatureKind

  /** Nobody is waiting on the call, but it serves work as it arrives (categorizing a new transaction, say), so a refused
    * call leaves today's work undone until tomorrow.
    */
  case object Incoming extends AiFeatureKind

  /** Works through a backlog; a refused call's task can wait for tomorrow. */
  case object Background extends AiFeatureKind

  val all: List[AiFeatureKind] = List(Live, Incoming, Background)
}

/** The provider account whose credits pay for an [[AiFeature]]'s calls. An app resolves the client it calls with from
  * this, so the account a feature is shown under and the account its calls are billed to cannot disagree.
  */
sealed trait AiAccount

object AiAccount {
  case object Personal extends AiAccount
  case object Playbook extends AiAccount

  val all: List[AiAccount] = List(Personal, Playbook)
}

/** One named kind of AI model call, declared in code, that spend is counted and budgeted by. Every call through
  * [[com.bryzek.ai.claude.ClaudeClient]] names one, so an unlabeled call does not compile.
  *
  * Declared as a case object in the app's own catalog (see [[AiFeatureCatalog]]):
  *
  * {{{
  * case object AssistantMailClassifier
  *   extends AiFeature("assistant_mail_classifier", AiFeatureKind.Background, AiAccount.Personal, BigDecimal(2))
  * }}}
  *
  * `name` is the label the app's store persists and rolls spend up by, so renaming one splits its history.
  *
  * `declaredBudgetUsd` is the most the feature may spend in one UTC day until a person sets a budget of their own, and
  * it is refused at construction above [[AiFeature.MaxDeclaredBudgetUsd]]. That cap is the review gate: a feature ships
  * at $10 a day or less with nothing but a pull request, and nothing in code can make a feature's first day cost more.
  * A higher budget is a person's recorded act, read by the app's [[AiBudgetSource]].
  */
trait AiFeature(
  val name: String,
  val kind: AiFeatureKind,
  val account: AiAccount,
  val declaredBudgetUsd: BigDecimal
) {
  require(AiFeature.isValidName(name), s"AiFeature name[$name] must be lower snake case ([a-z][a-z0-9_]*)")
  require(
    declaredBudgetUsd >= 0,
    s"AiFeature[$name] declares a negative daily budget[$declaredBudgetUsd]"
  )
  require(
    declaredBudgetUsd <= AiFeature.MaxDeclaredBudgetUsd,
    s"AiFeature[$name] declares a daily budget of $$$declaredBudgetUsd, above the $$${AiFeature.MaxDeclaredBudgetUsd} " +
      "a feature may declare in code; a higher budget is set by a person, with a reason"
  )

  override def toString: String = name
}

object AiFeature {

  /** The most a feature may declare in code, in dollars per UTC day. */
  val MaxDeclaredBudgetUsd: BigDecimal = BigDecimal(10)

  private val NamePattern = "^[a-z][a-z0-9_]*$".r

  def isValidName(name: String): Boolean = NamePattern.matches(name)
}

/** An app's list of its [[AiFeature]]s -- the way platform holds `AutonomyWorkflowCatalog`. Constructing one forces every
  * feature it lists, so a feature declaring more than [[AiFeature.MaxDeclaredBudgetUsd]] fails the catalog at
  * construction, and two features sharing a name (which would share a budget and a history) fail it too.
  */
trait AiFeatureCatalog {
  def all: Seq[AiFeature]

  private lazy val byName: Map[String, AiFeature] = {
    val duplicates = all.groupBy(_.name).collect { case (name, fs) if fs.size > 1 => name }
    require(
      duplicates.isEmpty,
      s"AiFeatureCatalog declares more than one feature named ${duplicates.toSeq.sorted.mkString(", ")}"
    )
    all.map(f => f.name -> f).toMap
  }

  /** Validates the catalog now rather than on first lookup. Call it at startup, or from a test. */
  final def validate(): Unit = {
    val _ = byName
  }

  final def find(name: String): Option[AiFeature] = byName.get(name)
}
