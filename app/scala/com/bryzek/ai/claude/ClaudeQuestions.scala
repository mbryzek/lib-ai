package com.bryzek.ai.claude

import cats.data.ValidatedNec
import cats.implicits.*
import com.bryzek.ai.AiFeature
import com.bryzek.claude.models.{ClaudeEffort, ClaudeError, ClaudeMessage, ClaudeModel, ClaudeRole, ClaudeThinkingType}
import play.api.libs.json.{JsArray, JsError, JsObject, JsPath, JsString, JsSuccess, Json, JsonValidationError, Reads}

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}

/** Whatever answers a feature's questions. [[ClaudeQuestions]] is the one implementation; a second provider is an
  * adapter that turns the same questions into its request and its answer back into [[QuestionAnswers]].
  */
trait QuestionBackend {
  def ask(
    feature: AiFeature,
    input: QuestionInput,
    questions: Seq[Question[?]],
    context: Option[String] = None
  )(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, QuestionAnswers]]
}

/** How a question call is sent. A question's answer is a few dozen output tokens, so `maxTokens` is sized for the
  * thinking in front of it rather than for the answer.
  */
final case class QuestionSettings(
  maxTokens: Long = 16000L,
  thinking: ClaudeThinkingType = ClaudeThinkingType.Adaptive,
  effort: Option[ClaudeEffort] = None,
  cacheTtl: ClaudeCacheTtl = ClaudeCacheTtl.FiveMinutes
)

/** Asks a feature's questions about one input in one call through [[ClaudeClient.chatCompletion]], under the feature's
  * [[AiFeature]], so its budget, its cost row, its retries and its model fallback are the ones every call already has.
  *
  * The schema ([[ClaudeQuestions.format]]) holds, per question, an `answer` enum of its allowed values and a
  * `confidence` enum of low, medium and high, and nothing else: no prose field, so a decision costs a few dozen output
  * tokens. Every answer is checked against its question before the feature sees it ([[ClaudeQuestions.reads]]); a value
  * that is not an option, a question left out, or one never asked fails the call as a malformed response, exactly as any
  * structured response that does not match its schema does.
  */
final case class ClaudeQuestions(
  client: ClaudeClient,
  models: Seq[ClaudeModel],
  settings: QuestionSettings = QuestionSettings()
) extends QuestionBackend {

  override def ask(
    feature: AiFeature,
    input: QuestionInput,
    questions: Seq[Question[?]],
    context: Option[String] = None
  )(implicit ec: ExecutionContext): Future[ValidatedNec[ClaudeError, QuestionAnswers]] =
    ClaudeQuestions.validate(questions) match {
      case Some(error) => Future.successful(error.invalidNec)
      case None =>
        client
          .chatCompletion[QuestionAnswers](
            ClaudeQuestions.request(input, questions, settings),
            ClaudeQuestions.format(input.key, questions),
            models,
            feature,
            context
          )(using ec, ClaudeQuestions.reads(input.key, questions))
          .map(_.map(_.content))
    }
}

object ClaudeQuestions {

  /** The output format's name, and how [[TestClaudeClient]] recognizes a question call. */
  val FormatName: String = "questions"

  /** The decision a ledger records one answer under. A retry re-asks under the same key, so it is the same decision. */
  def decisionKey(inputKey: String, questionKey: String): String = s"$inputKey:$questionKey"

  /** The same questions as one request in a batch ([[ClaudeBatchClient.submit]]). Read its outcome back with
    * [[answers]], naming the same input key and questions.
    */
  def batchItem(
    customId: String,
    input: QuestionInput,
    questions: Seq[Question[?]],
    model: ClaudeModel,
    settings: QuestionSettings = QuestionSettings()
  ): ValidatedNec[ClaudeError, ClaudeBatchItem] =
    validate(questions) match {
      case Some(error) => error.invalidNec
      case None =>
        ClaudeBatchItem(
          customId = customId,
          request = request(input, questions, settings),
          model = model,
          format = Some(format(input.key, questions))
        ).validNec
    }

  /** A batched question call's answers, checked against its questions exactly as [[ClaudeQuestions.ask]] checks them.
    */
  def answers(
    outcome: ClaudeBatchOutcome,
    inputKey: String,
    questions: Seq[Question[?]]
  ): ValidatedNec[ClaudeError, QuestionAnswers] =
    validate(questions) match {
      case Some(error) => error.invalidNec
      case None => outcome.parse[QuestionAnswers](using reads(inputKey, questions))
    }

  /** The order the model is offered `question`'s options in about the input `inputKey`.
    *
    * Shuffled, because a model leans toward the first option it is shown: a fixed order turns that lean into a skew no
    * label reveals, where a shuffled one turns it into noise that shows up as lower confidence. Seeded by the decision
    * key, so a retry sees the same order. The shuffle is Fisher-Yates over a `java.util.Random` seeded from the key's
    * SHA-256 -- both specified to the bit -- so the order for a key does not move with a JVM or a Scala version.
    */
  def optionOrder(inputKey: String, question: Question[?]): Seq[String] =
    shuffle(question.names, decisionKey(inputKey, question.key))

  private[claude] def shuffle[T](items: Seq[T], seed: String): Seq[T] = {
    val digest = MessageDigest.getInstance("SHA-256").digest(seed.getBytes(StandardCharsets.UTF_8))
    val random = new java.util.Random(ByteBuffer.wrap(digest, 0, 8).getLong)
    val buffer = ArrayBuffer.from(items)
    (buffer.size - 1).to(1, -1).foreach { i =>
      val j = random.nextInt(i + 1)
      val tmp = buffer(i)
      buffer(i) = buffer(j)
      buffer(j) = tmp
    }
    buffer.toSeq
  }

  /** Why `questions` cannot be asked together, if they cannot. */
  private[claude] def validate(questions: Seq[Question[?]]): Option[ClaudeError] = {
    val keys = questions.map(_.key)
    if (questions.isEmpty) {
      Some(ClaudeError(message = "Cannot ask an empty list of questions"))
    } else if (keys.distinct.size != keys.size) {
      Some(ClaudeError(message = s"Questions share a key: ${keys.diff(keys.distinct).distinct.sorted.mkString(", ")}"))
    } else {
      None
    }
  }

  /** The one structured-output schema for one call: per question an object holding exactly an `answer` enum, in the
    * shuffled order, and a `confidence` enum.
    */
  def format(inputKey: String, questions: Seq[Question[?]]): ClaudeOutputFormat =
    ClaudeOutputFormats.create(
      FormatName,
      JsObject(questions.map { q =>
        q.key -> Json.obj(
          "type" -> "object",
          "properties" -> Json.obj(
            "answer" -> enumOf(optionOrder(inputKey, q)),
            "confidence" -> enumOf(Confidence.all.map(_.wireValue))
          ),
          "required" -> Json.arr("answer", "confidence"),
          "additionalProperties" -> false
        )
      }),
      questions.map(_.key)
    )

  private def enumOf(values: Seq[String]): JsObject =
    Json.obj("type" -> "string", "enum" -> JsArray(values.map(JsString.apply)))

  /** The request: the feature's instructions as the system prompt, then the input, then every question with its
    * answers in the order the schema offers them.
    */
  def request(input: QuestionInput, questions: Seq[Question[?]], settings: QuestionSettings): AiRequest =
    AiRequest(
      messages = Seq(
        ClaudeMessage(
          role = ClaudeRole.User,
          content = input.content :+ ClaudeClient.textBlock(questionText(input.key, questions))
        )
      ),
      maxTokens = settings.maxTokens,
      system = input.instructions,
      cacheSystem = input.instructions.isDefined,
      thinking = settings.thinking,
      effort = settings.effort,
      cacheTtl = settings.cacheTtl
    )

  private[claude] def questionText(inputKey: String, questions: Seq[Question[?]]): String = {
    val listed = questions.zipWithIndex.map { case (q, i) =>
      s"${i + 1}. ${q.key}: ${q.text.trim}\n   Answers: ${optionOrder(inputKey, q).mkString(", ")}"
    }
    (Seq(
      "Answer every question below about the input above. For each, give exactly one of its answers, and say how " +
        "confident you are in it: low, medium or high."
    ) ++ listed).mkString("\n\n")
  }

  /** Reads the response as the answers to `questions`, refusing anything that does not answer exactly them. */
  def reads(inputKey: String, questions: Seq[Question[?]]): Reads[QuestionAnswers] = Reads {
    case obj: JsObject =>
      val asked = questions.map(_.key).toSet
      val unasked = obj.keys.toSeq.filterNot(asked).sorted.map { k =>
        (JsPath \ k) -> s"'$k' is not a question that was asked"
      }
      val checked = questions.map(q => check(inputKey, obj, q))
      val errors = unasked ++ checked.collect { case Left(e) => e }
      if (errors.isEmpty) {
        JsSuccess(new QuestionAnswers(inputKey, checked.collect { case Right(r) => r }))
      } else {
        JsError(errors.map { case (path, message) => path -> Seq(JsonValidationError(message)) })
      }
    case other => JsError(s"expected an object of answers, got ${other.getClass.getSimpleName}")
  }

  private def check(inputKey: String, obj: JsObject, q: Question[?]): Either[(JsPath, String), RecordedAnswer] = {
    val path = JsPath \ q.key
    (obj \ q.key).toOption match {
      case None => Left(path -> s"question '${q.key}' is missing from the answer")
      case Some(answer) =>
        val value = (answer \ "answer").asOpt[String]
        val confidence = (answer \ "confidence").asOpt[String]
        (value, confidence) match {
          case (None, _) => Left((path \ "answer") -> s"question '${q.key}' has no answer")
          case (Some(v), _) if !q.names.contains(v) =>
            Left((path \ "answer") -> s"'$v' is not one of question '${q.key}''s answers (${q.names.mkString(", ")})")
          case (Some(v), c) =>
            c.flatMap(Confidence.fromString) match {
              case None =>
                Left(
                  (path \ "confidence") ->
                    s"question '${q.key}' has confidence ${c.getOrElse("(none)")}, not one of low, medium, high"
                )
              case Some(conf) => Right(RecordedAnswer(decisionKey(inputKey, q.key), q.key, v, conf, q.band(conf)))
            }
        }
    }
  }

  /** The questions a schema built by [[format]] asks, each with its allowed answers in the order offered -- or None
    * when the schema is not a question schema. How [[TestClaudeClient]] answers a question call, batched or not.
    */
  private[claude] def questionsIn(schema: JsObject): Option[Seq[(String, Seq[String])]] = {
    def enumValues(js: JsObject, field: String): Option[Seq[String]] =
      (js \ "properties" \ field \ "enum").asOpt[Seq[String]]

    (schema \ "properties").asOpt[JsObject].filter(_.fields.nonEmpty).flatMap { props =>
      props.fields.toList.traverse { case (key, js) =>
        js.asOpt[JsObject].flatMap { q =>
          val fields = (q \ "properties").asOpt[JsObject].map(_.keys.toSet)
          Option
            .when(fields.contains(Set("answer", "confidence")))(())
            .flatMap(_ => enumValues(q, "confidence").filter(_ == Confidence.all.map(_.wireValue)))
            .flatMap(_ => enumValues(q, "answer"))
            .map(key -> _)
        }
      }
    }
  }

}
