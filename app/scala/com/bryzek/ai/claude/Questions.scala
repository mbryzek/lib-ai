package com.bryzek.ai.claude

import com.bryzek.claude.models.ClaudeContentBlock

/** How sure the model says it is of one [[Answer]]. Three buckets, because three is what a model can say with any
  * consistency; Claude returns no token probabilities, so the confidence is the model's own word, asked in the same
  * call as the answer (DES-22644).
  */
sealed abstract class Confidence(val wireValue: String, val rank: Int) {
  override def toString: String = wireValue
}

object Confidence {
  case object Low extends Confidence("low", 0)
  case object Medium extends Confidence("medium", 1)
  case object High extends Confidence("high", 2)

  /** Lowest first -- the order the schema offers them in. */
  val all: List[Confidence] = List(Low, Medium, High)

  def fromString(value: String): Option[Confidence] = all.find(_.wireValue == value)
}

/** What a feature does with one [[Answer]], decided by its question's [[Thresholds]] from the answer's confidence. */
sealed abstract class Band(val wireValue: String) {
  override def toString: String = wireValue
}

object Band {

  /** Confident enough to act on with no person. */
  case object Used extends Band("used")

  /** Sent to a person; the feature acts on the question's fallback until one answers. */
  case object Review extends Band("review")

  /** Not confident enough to show anyone; the feature acts on the question's fallback. */
  case object Unanswered extends Band("unanswered")

  val all: List[Band] = List(Used, Review, Unanswered)

  def fromString(value: String): Option[Band] = all.find(_.wireValue == value)
}

/** The two lines a question's answers are sorted by: at or above `use` an answer is [[Band.Used]], at or above
  * `review` it is [[Band.Review]], and below that it is [[Band.Unanswered]]. They live in code beside the question they
  * govern, so a change to one is a pull request reviewed with the question.
  */
final case class Thresholds(use: Confidence, review: Confidence) {
  require(
    review.rank <= use.rank,
    s"Thresholds: review[$review] must not be above use[$use]; an answer cannot need more confidence to reach a person " +
      "than to be used without one"
  )

  def band(confidence: Confidence): Band =
    if (confidence.rank >= use.rank) Band.Used
    else if (confidence.rank >= review.rank) Band.Review
    else Band.Unanswered
}

object Thresholds {

  /** Used at high, sent to a person at medium, unanswered at low. A starting guess to be tuned per question on what
    * people replace, not a measured value.
    */
  val Default: Thresholds = Thresholds(use = Confidence.High, review = Confidence.Medium)
}

/** The three shapes a question takes. A score is a choice whose options are ordered, lowest first. */
sealed abstract class QuestionKind(val wireValue: String) {
  override def toString: String = wireValue
}

object QuestionKind {
  case object Choice extends QuestionKind("choice")
  case object Score extends QuestionKind("score")
  case object YesNo extends QuestionKind("yes_no")

  val all: List[QuestionKind] = List(Choice, Score, YesNo)
}

/** One named thing a feature asks about an input, with its allowed answers, its [[Thresholds]] and its fallback.
  *
  * Declared as a value in the feature's own code through [[Question.choice]], [[Question.score]] or [[Question.yesNo]]
  * (or the typed `choiceOf` / `scoreOf`, whose options are the feature's own values). `text` is the question as the
  * model reads it. `fallback` is the value the feature acts on whenever the answer is not [[Band.Used]] -- the rule a
  * prompt used to state as "when you cannot tell, answer X", applied by code every time instead of by the model
  * sometimes.
  */
final class Question[A] private (
  val key: String,
  val kind: QuestionKind,
  val text: String,
  val options: Seq[A],
  nameOf: A => String,
  val fallback: A,
  val thresholds: Thresholds
) {

  /** Each option's name, in declared order: the values the model may answer with. */
  val names: Seq[String] = options.map(nameOf)

  require(Question.isValidKey(key), s"Question key[$key] must be lower snake case ([a-z][a-z0-9_]*)")
  require(text.trim.nonEmpty, s"Question[$key] must say what it asks")
  require(options.size >= 2, s"Question[$key] must offer at least two options")
  require(names.forall(_.trim.nonEmpty), s"Question[$key] has an option with an empty name")
  require(
    names.distinct.size == names.size,
    s"Question[$key] names an option more than once: ${names.diff(names.distinct).distinct.mkString(", ")}"
  )
  require(options.contains(fallback), s"Question[$key]'s fallback[${nameOf(fallback)}] is not one of its options")

  def name(value: A): String = nameOf(value)

  def find(name: String): Option[A] = names.indexOf(name) match {
    case -1 => None
    case i => Some(options(i))
  }

  def band(confidence: Confidence): Band = thresholds.band(confidence)

  /** The value the feature acts on: the answer when it is [[Band.Used]], the fallback otherwise. */
  def actOn(answer: Answer[A]): A = answer.band match {
    case Band.Used => answer.value
    case Band.Review | Band.Unanswered => fallback
  }

  /** The option's position in declared order. For a score that is its level, 0 for the lowest. */
  def rank(value: A): Int = options.indexOf(value)

  def withThresholds(thresholds: Thresholds): Question[A] =
    new Question(key, kind, text, options, nameOf, fallback, thresholds)

  def withFallback(fallback: A): Question[A] =
    new Question(key, kind, text, options, nameOf, fallback, thresholds)

  override def toString: String = s"Question($key, $kind, ${names.mkString("[", ", ", "]")})"
}

object Question {

  private val KeyPattern = "^[a-z][a-z0-9_]*$".r

  /** A key is a JSON property name in the schema and half of a decision key, so it is held to one shape. */
  def isValidKey(key: String): Boolean = KeyPattern.matches(key)

  /** One of a list of named options. */
  def choice(
    key: String,
    text: String,
    options: Seq[String],
    fallback: String,
    thresholds: Thresholds = Thresholds.Default
  ): Question[String] =
    new Question(key, QuestionKind.Choice, text, options, identity, fallback, thresholds)

  /** One of the feature's own values, each named by `name`. */
  def choiceOf[A](
    key: String,
    text: String,
    options: Seq[A],
    fallback: A,
    thresholds: Thresholds = Thresholds.Default
  )(name: A => String): Question[A] =
    new Question(key, QuestionKind.Choice, text, options, name, fallback, thresholds)

  /** One of an ordered list of levels, lowest first. The model sees them shuffled like any options, so each level's
    * name has to say what it means on its own; [[Question.rank]] maps the answer back to its place.
    */
  def score(
    key: String,
    text: String,
    levels: Seq[String],
    fallback: String,
    thresholds: Thresholds = Thresholds.Default
  ): Question[String] =
    new Question(key, QuestionKind.Score, text, levels, identity, fallback, thresholds)

  /** [[score]] over the feature's own values, lowest first. */
  def scoreOf[A](
    key: String,
    text: String,
    levels: Seq[A],
    fallback: A,
    thresholds: Thresholds = Thresholds.Default
  )(name: A => String): Question[A] =
    new Question(key, QuestionKind.Score, text, levels, name, fallback, thresholds)

  /** Yes or no, answered as `true` or `false`. */
  def yesNo(
    key: String,
    text: String,
    fallback: Boolean,
    thresholds: Thresholds = Thresholds.Default
  ): Question[Boolean] =
    new Question(key, QuestionKind.YesNo, text, Seq(true, false), b => if (b) "yes" else "no", fallback, thresholds)
}

/** One question's answer: the value, how confident the model said it was, and the band its thresholds put it in. */
final case class Answer[A](value: A, confidence: Confidence, band: Band)

/** One answer in the form a ledger records it: keyed `<input key>:<question key>` so a retry re-asks under the same
  * decision, with the answer's option name rather than the feature's value.
  */
final case class RecordedAnswer(
  decisionKey: String,
  question: String,
  value: String,
  confidence: Confidence,
  band: Band
)

/** Every answer one call gave, checked against the questions it asked. Read one with `answers(question)`, typed as the
  * question is, or all of them with [[recorded]].
  */
final class QuestionAnswers private[claude] (val inputKey: String, val recorded: Seq[RecordedAnswer]) {

  private val byQuestion: Map[String, RecordedAnswer] = recorded.map(r => r.question -> r).toMap

  def get[A](question: Question[A]): Option[Answer[A]] =
    byQuestion.get(question.key).flatMap { r =>
      question.find(r.value).map(value => Answer(value, r.confidence, r.band))
    }

  /** The question's answer. Refuses a question this call did not ask, which is a programming error in the feature. */
  def apply[A](question: Question[A]): Answer[A] =
    get(question).getOrElse(
      throw new IllegalArgumentException(s"Question[${question.key}] was not asked about input[$inputKey]")
    )

  /** The value the feature acts on for `question`: its answer when used, its fallback otherwise. */
  def actOn[A](question: Question[A]): A = question.actOn(apply(question))

  override def toString: String = s"QuestionAnswers($inputKey, ${recorded.mkString(", ")})"
}

/** What a feature hands the model to answer from. `key` names the input (an email's id, an insight's id) and is half of
  * every decision key and the seed of every option shuffle, so it must be stable across retries. `instructions` is the
  * feature's standing guidance, sent as the system prompt.
  */
final case class QuestionInput(
  key: String,
  content: Seq[ClaudeContentBlock],
  instructions: Option[String] = None
) {
  require(key.trim.nonEmpty, "QuestionInput key must not be empty")
  require(content.nonEmpty, s"QuestionInput[$key] must carry something to answer from")
}

object QuestionInput {
  def text(key: String, text: String, instructions: Option[String] = None): QuestionInput =
    QuestionInput(key, Seq(ClaudeClient.textBlock(text)), instructions)
}
