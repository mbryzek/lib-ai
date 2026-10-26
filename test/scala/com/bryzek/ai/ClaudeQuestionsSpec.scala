package com.bryzek.ai.claude

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import com.bryzek.ai.UnlimitedAiBudget
import com.bryzek.claude.models.{ClaudeError, ClaudeModel}
import helpers.{FutureHelpers, TestAiFeature}
import org.apache.pekko.util.Timeout
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{FiniteDuration, SECONDS}

/** Runs without a Play application, over an in-process [[TestClaudeClient]], as [[ClaudeBatchClientSpec]] does. */
class ClaudeQuestionsSpec extends AnyWordSpec with Matchers with FutureHelpers {

  private implicit val timeout: Timeout = FiniteDuration(30, SECONDS)

  private sealed abstract class Severity(val name: String)
  private object Severity {
    case object Minor extends Severity("minor")
    case object Major extends Severity("major")
    case object Critical extends Severity("critical")
    val all: Seq[Severity] = Seq(Minor, Major, Critical)
  }

  private val Category =
    Question.choice("category", "What is the email about?", Seq("billing", "scheduling", "other"), fallback = "other")
  private val Impact = Question.scoreOf(
    "impact",
    "How much would acting on this matter to the club?",
    Severity.all,
    fallback = Severity.Minor
  )(_.name)
  private val Receipt = Question.yesNo("is_receipt", "Is this attachment a receipt?", fallback = true)

  private val Input = QuestionInput.text("email-42", "Please move my lesson to Thursday.", Some("You triage email."))

  private def fixture(): (TestClaudeClient, ClaudeQuestions) = {
    val transport = new TestClaudeClient()
    val client = ClaudeClient(transport, ClaudeConfig("test-api-key"), NoopClaudeStore, new UnlimitedAiBudget())
    (transport, ClaudeQuestions(client, Seq(ClaudeModel.ClaudeHaiku45)))
  }

  private def valid[T](v: ValidatedNec[ClaudeError, T]): T = v match {
    case Valid(t) => t
    case Invalid(e) => fail(e.toNonEmptyList.toList.map(_.message).mkString("; "))
  }

  private def errors[T](v: ValidatedNec[ClaudeError, T]): String = v match {
    case Valid(t) => fail(s"expected a failure, got $t")
    case Invalid(e) => e.toNonEmptyList.toList.map(_.message).mkString("; ")
  }

  "ask" should {

    "answer a choice with its option" in {
      val (transport, questions) = fixture()
      transport.answer("category", "scheduling", Confidence.High)
      val answers = valid(await(questions.ask(TestAiFeature, Input, Seq(Category))))
      answers(Category) mustBe Answer("scheduling", Confidence.High, Band.Used)
      answers.actOn(Category) mustBe "scheduling"
    }

    "answer a score with the feature's own level, mapped back to its order" in {
      val (transport, questions) = fixture()
      transport.answer("impact", "critical", Confidence.High)
      val answers = valid(await(questions.ask(TestAiFeature, Input, Seq(Impact))))
      answers(Impact).value mustBe Severity.Critical
      Impact.rank(answers(Impact).value) mustBe 2
    }

    "answer a yes/no as a boolean" in {
      val (transport, questions) = fixture()
      transport.answer("is_receipt", "no", Confidence.Medium)
      val answers = valid(await(questions.ask(TestAiFeature, Input, Seq(Receipt))))
      answers(Receipt) mustBe Answer(false, Confidence.Medium, Band.Review)
      answers.actOn(Receipt) mustBe true
    }

    "answer every question of one call, each recorded under its own decision key" in {
      val (transport, questions) = fixture()
      transport.answer("category", "billing", Confidence.Low)
      val answers = valid(await(questions.ask(TestAiFeature, Input, Seq(Category, Impact, Receipt))))
      answers.recorded.map(_.decisionKey) mustBe Seq("email-42:category", "email-42:impact", "email-42:is_receipt")
      answers.recorded.head mustBe RecordedAnswer(
        "email-42:category",
        "category",
        "billing",
        Confidence.Low,
        Band.Unanswered
      )
      // Unanswered questions default to the alphabetically first answer at high confidence.
      answers(Impact) mustBe Answer(Severity.Critical, Confidence.High, Band.Used)
      answers(Receipt).value mustBe false
    }

    "fail as a malformed response when the answer is not one of the question's options" in {
      val (transport, questions) = fixture()
      transport.answer("category", "refunds", Confidence.High)
      val message = errors(await(questions.ask(TestAiFeature, Input, Seq(Category, Receipt))))
      message must include("Content is not valid")
      message must include("'refunds' is not one of question 'category''s answers (billing, scheduling, other)")
    }

    "refuse questions sharing a key, and an empty list, without a call" in {
      val (_, questions) = fixture()
      errors(await(questions.ask(TestAiFeature, Input, Seq(Category, Category)))) must include(
        "Questions share a key: category"
      )
      errors(await(questions.ask(TestAiFeature, Input, Nil))) must include("Cannot ask an empty list of questions")
    }

    "refuse reading a question the call did not ask" in {
      val (_, questions) = fixture()
      val answers = valid(await(questions.ask(TestAiFeature, Input, Seq(Category))))
      answers.get(Receipt) mustBe None
      an[IllegalArgumentException] must be thrownBy answers(Receipt)
    }
  }

  "reads" should {

    def read(js: JsObject, questions: Seq[Question[?]] = Seq(Category, Receipt)) =
      ClaudeQuestions.reads("email-42", questions).reads(js)

    "accept exactly the questions asked" in {
      val result = read(
        Json.obj(
          "category" -> Json.obj("answer" -> "other", "confidence" -> "medium"),
          "is_receipt" -> Json.obj("answer" -> "yes", "confidence" -> "high")
        )
      )
      result.get(Receipt) mustBe Answer(true, Confidence.High, Band.Used)
    }

    "refuse a question missing from the answer" in {
      read(Json.obj("category" -> Json.obj("answer" -> "other", "confidence" -> "medium"))).isError mustBe true
      read(Json.obj("category" -> Json.obj("answer" -> "other", "confidence" -> "medium"))).toString must include(
        "question 'is_receipt' is missing from the answer"
      )
    }

    "refuse a question that was never asked" in {
      read(
        Json.obj(
          "category" -> Json.obj("answer" -> "other", "confidence" -> "low"),
          "mood" -> Json.obj("answer" -> "happy", "confidence" -> "low")
        ),
        Seq(Category)
      ).toString must include("'mood' is not a question that was asked")
    }

    "refuse a confidence outside low, medium and high" in {
      read(
        Json.obj(
          "category" -> Json.obj("answer" -> "other", "confidence" -> "certain"),
          "is_receipt" -> Json.obj("answer" -> "yes", "confidence" -> "high")
        )
      ).toString must include("question 'category' has confidence certain, not one of low, medium, high")
    }
  }

  "bands" should {

    "follow the default thresholds: used at high, review at medium, unanswered at low" in {
      Thresholds.Default.band(Confidence.High) mustBe Band.Used
      Thresholds.Default.band(Confidence.Medium) mustBe Band.Review
      Thresholds.Default.band(Confidence.Low) mustBe Band.Unanswered
    }

    "follow a question's own thresholds" in {
      val lenient = Thresholds(use = Confidence.Medium, review = Confidence.Low)
      lenient.band(Confidence.High) mustBe Band.Used
      lenient.band(Confidence.Medium) mustBe Band.Used
      lenient.band(Confidence.Low) mustBe Band.Review

      val noReview = Thresholds(use = Confidence.High, review = Confidence.High)
      noReview.band(Confidence.High) mustBe Band.Used
      noReview.band(Confidence.Medium) mustBe Band.Unanswered
      noReview.band(Confidence.Low) mustBe Band.Unanswered

      Category.withThresholds(lenient).band(Confidence.Medium) mustBe Band.Used
    }

    "refuse a review threshold above the use threshold" in {
      an[IllegalArgumentException] must be thrownBy Thresholds(use = Confidence.Medium, review = Confidence.High)
    }

    "act on the answer only when it is used" in {
      Category.actOn(Answer("billing", Confidence.High, Band.Used)) mustBe "billing"
      Category.actOn(Answer("billing", Confidence.Medium, Band.Review)) mustBe "other"
      Category.actOn(Answer("billing", Confidence.Low, Band.Unanswered)) mustBe "other"
    }
  }

  "the option shuffle" should {

    "give the same order for the same input and question every time" in {
      val orders = (1 to 5).map(_ => ClaudeQuestions.optionOrder("email-42", Impact))
      orders.distinct.size mustBe 1
      orders.head.sorted mustBe Impact.names.sorted
    }

    "not move with the JVM or the Scala version" in {
      // Pinned: a change here re-orders every retry of every input already asked, so it is a deliberate act.
      ClaudeQuestions.shuffle(Seq("a", "b", "c", "d", "e"), "email-42:category") mustBe Seq("d", "c", "a", "e", "b")
    }

    "vary across inputs, including the order of score levels and of yes and no" in {
      val keys = (1 to 40).map(i => s"email-$i")
      keys.map(ClaudeQuestions.optionOrder(_, Category)).distinct.size must be > 1
      keys.map(ClaudeQuestions.optionOrder(_, Impact)).distinct.size must be > 1
      keys.map(ClaudeQuestions.optionOrder(_, Receipt)).distinct.size mustBe 2
    }

    "offer the shuffled order in both the schema and the prompt" in {
      val order = ClaudeQuestions.optionOrder("email-42", Impact)
      val schema = ClaudeQuestions.format("email-42", Seq(Impact)).schema
      (schema \ "properties" \ "impact" \ "properties" \ "answer" \ "enum").as[Seq[String]] mustBe order
      ClaudeQuestions.questionText("email-42", Seq(Impact)) must include(s"Answers: ${order.mkString(", ")}")
    }
  }

  "the schema" should {

    "hold, per question, an answer enum and a confidence enum and nothing else" in {
      val schema = ClaudeQuestions.format("email-42", Seq(Category, Impact, Receipt)).schema
      (schema \ "required").as[Seq[String]] mustBe Seq("category", "impact", "is_receipt")
      (schema \ "additionalProperties").as[Boolean] mustBe false
      Seq("category", "impact", "is_receipt").foreach { key =>
        val q = (schema \ "properties" \ key).as[JsObject]
        (q \ "properties").as[JsObject].keys mustBe Set("answer", "confidence")
        (q \ "required").as[Seq[String]] mustBe Seq("answer", "confidence")
        (q \ "additionalProperties").as[Boolean] mustBe false
        (q \ "properties" \ "confidence" \ "enum").as[Seq[String]] mustBe Seq("low", "medium", "high")
      }
    }

    "send the feature's instructions as a cached system prompt" in {
      val request = ClaudeQuestions.request(Input, Seq(Category), QuestionSettings())
      request.system mustBe Some("You triage email.")
      request.cacheSystem mustBe true
    }
  }

  "a question" should {

    "refuse a fallback that is not one of its options" in {
      an[IllegalArgumentException] must be thrownBy Question.choice("c", "?", Seq("a", "b"), fallback = "z")
    }

    "refuse an option named twice, fewer than two options, and a key that is not snake case" in {
      an[IllegalArgumentException] must be thrownBy Question.choice("c", "?", Seq("a", "a"), fallback = "a")
      an[IllegalArgumentException] must be thrownBy Question.choice("c", "?", Seq("a"), fallback = "a")
      an[IllegalArgumentException] must be thrownBy Question.choice("Not-Snake", "?", Seq("a", "b"), fallback = "a")
    }
  }

  "the batch form" should {

    "submit the same questions and read the answers back through the same check" in {
      val transport = new TestClaudeClient()
      transport.answer("category", "billing", Confidence.Medium)
      val batchClient = ClaudeBatchClient(transport, ClaudeConfig("test-api-key"))
      val items = Seq("email-1", "email-2").map { key =>
        valid(
          ClaudeQuestions.batchItem(key, Input.copy(key = key), Seq(Category, Receipt), ClaudeModel.ClaudeHaiku45)
        )
      }
      val batch = valid(await(batchClient.submit(items)))
      val outcomes = valid(await(batchClient.results(batch)))
      val byId = outcomes.map(o => o.customId -> valid(ClaudeQuestions.answers(o, o.customId, Seq(Category, Receipt))))
      byId.map(_._1).sorted mustBe Seq("email-1", "email-2")
      byId.foreach { case (id, answers) =>
        answers(Category) mustBe Answer("billing", Confidence.Medium, Band.Review)
        answers.recorded.head.decisionKey mustBe s"$id:category"
      }
    }

    "fail a batched answer that is not one of the options" in {
      val transport = new TestClaudeClient()
      transport.answer("is_receipt", "maybe")
      val batchClient = ClaudeBatchClient(transport, ClaudeConfig("test-api-key"))
      val item = valid(ClaudeQuestions.batchItem("email-1", Input, Seq(Receipt), ClaudeModel.ClaudeHaiku45))
      val outcome = valid(await(batchClient.results(valid(await(batchClient.submit(Seq(item))))))).head
      errors(ClaudeQuestions.answers(outcome, "email-1", Seq(Receipt))) must include(
        "'maybe' is not one of question 'is_receipt''s answers (yes, no)"
      )
    }
  }
}
