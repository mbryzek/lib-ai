package com.bryzek.ai.claude

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNec
import cats.implicits.*
import com.bryzek.claude.response.models.*
import com.bryzek.claude.response.models.json.*
import com.bryzek.claude.client.IClient
import com.bryzek.claude.models.*
import com.bryzek.claude.models.json.*
import org.joda.time.DateTime
import play.api.libs.json.{JsValue, Json}

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import javax.inject.Singleton
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

object TestClaudeClient {
  val OutputFormatNameHeader: String = "X-Output-Format"
}

/** Answers every request in-process. [[SimulatedClaudeClient]] is what makes its failures say so: this client raises
  * ordinary-looking errors (an unknown output format, a rejected request shape) that are indistinguishable from a
  * provider failure once they are read out of a CI log, which is what happened in ISS-2522.
  */
@Singleton
class TestClaudeClient extends IClient with ClaudeBatchResults with SimulatedClaudeClient {

  private def getHeader(requestHeaders: Seq[(String, String)], name: String): Option[String] = {
    requestHeaders.find(_._1.toLowerCase.trim == name.toLowerCase.trim).map(_._2).toList.distinct match {
      case Nil => None
      case one :: Nil => Some(one)
      case multiple =>
        sys.error(s"Found multiple request headers named '$name' with values: ${multiple.mkString(", ")}")
    }
  }

  protected def validateOutputFormatByName(name: String): ValidatedNec[String, TestResponseFormat] = {
    ClaudeOutputFormats.all
      .find(_.name == name)
      .toValidNec(s"Could not identify json schema from object")
      .andThen(toTestResponseType)
  }

  def postClaudeRequest(
    claudeRequest: ClaudeRequest,
    format: TestResponseFormat,
    @scala.annotation.nowarn("msg=unused") requestHeaders: Seq[(String, String)] = Nil
  ): JsValue = {
    format.generateResponse(claudeRequest)
  }

  /** The model wants a tool call when tools are offered, the caller hasn't forced `tool_choice: none`, and no tool
    * result has been returned yet. This lets [[ClaudeClient.runToolLoop]] be exercised without a live API: one
    * `tool_use` turn, then (once results come back) a structured / text final turn.
    */
  private def shouldRequestTool(body: ClaudeRequest): Boolean = {
    val hasTools = body.tools.exists(_.nonEmpty)
    val forcedNone = body.toolChoice.exists(_.`type` == ClaudeToolChoiceType.None)
    val alreadyRanTool = body.messages.exists(_.content.exists(_.`type` == ClaudeContentType.ToolResult))
    hasTools && !forcedNone && !alreadyRanTool
  }

  private def toolUseResponse(body: ClaudeRequest): ClaudeResponse = {
    val tool = body.tools
      .flatMap(_.headOption)
      .getOrElse(sys.error("shouldRequestTool implies at least one tool is present"))
    response(
      Seq(
        ClaudeContentBlock(ClaudeContentType.ToolUse).copy(
          id = Some("test-tool-use-1"),
          name = Some(tool.name),
          input = Some(Json.obj())
        )
      ),
      ClaudeStopReason.ToolUse
    )
  }

  private def response(
    content: Seq[ClaudeContentBlock],
    stopReason: ClaudeStopReason,
    serviceTier: ClaudeServiceTier = ClaudeServiceTier.Standard
  ): ClaudeResponse = {
    ClaudeResponse(
      id = "test-response-id",
      `type` = "message",
      role = ClaudeRole.Assistant,
      content = content,
      model = ClaudeModel.ClaudeSonnet5,
      stopReason = stopReason,
      usage = ClaudeUsage(inputTokens = 10, outputTokens = 20).copy(serviceTier = Some(serviceTier))
    )
  }

  override def createMessage(
    body: ClaudeRequest,
    requestHeaders: Seq[(String, String)] = Nil
  ): Future[ClaudeResponse] = Future {
    // The Claude API rejects a request whose final message is an assistant turn (an assistant-turn prefill),
    // and rejects it outright alongside structured outputs. Enforce the wire contract here so that class of bug
    // surfaces in tests instead of only against production.
    require(
      body.messages.lastOption.forall(_.role != ClaudeRole.Assistant),
      "Request must not end with an assistant message (assistant-turn prefill is rejected by the Claude API)"
    )
    if (shouldRequestTool(body)) {
      toolUseResponse(body)
    } else {
      val responseText = getHeader(requestHeaders, TestClaudeClient.OutputFormatNameHeader) match {
        case None =>
          "This is a test plain text response."
        case Some(name) =>
          val format = expectValid { validateOutputFormatByName(name) }
          Json.prettyPrint(postClaudeRequest(body, format, requestHeaders))
      }
      response(Seq(ClaudeClient.textBlock(responseText)), ClaudeStopReason.EndTurn)
    }
  }

  // --- Message Batches -------------------------------------------------------------------------------------------
  //
  // An in-memory stand-in so a caller's submit / poll / reconcile path can be exercised without a live API. Batches
  // complete the moment they are created: the point of the double is the RECONCILIATION -- results keyed by custom id,
  // arriving out of order, some of them failed -- not the waiting, which a caller's own poller tests should drive by
  // constructing the batch states they care about.

  private val batches = new ConcurrentHashMap[String, (ClaudeBatch, Seq[String])]()
  private val batchCounter = new AtomicInteger(0)

  override def createClaudeBatch(
    body: ClaudeBatchForm,
    requestHeaders: Seq[(String, String)] = Nil
  ): Future[ClaudeBatch] = Future {
    val id = s"msgbatch_test_${batchCounter.incrementAndGet()}"
    val details = body.requests.map { item =>
      item.customId -> (Try(batchResponse(item.params)) match {
        case Success(r) => ClaudeBatchResultDetail(ClaudeBatchResultType.Succeeded).copy(message = Some(r))
        case Failure(e) =>
          ClaudeBatchResultDetail(ClaudeBatchResultType.Errored)
            .copy(error = Some(ClaudeErrorResponse(error = ClaudeError(message = e.getMessage))))
      })
    }
    val succeeded = details.count(_._2.`type` == ClaudeBatchResultType.Succeeded)
    val now = DateTime.now()
    val batch = ClaudeBatch(
      id = id,
      processingStatus = ClaudeBatchProcessingStatus.Ended,
      requestCounts = ClaudeBatchRequestCounts(
        processing = 0,
        succeeded = succeeded.toLong,
        errored = (details.size - succeeded).toLong,
        canceled = 0,
        expired = 0
      ),
      createdAt = now,
      expiresAt = now.plusHours(24)
    ).copy(
      endedAt = Some(now),
      resultsUrl = Some(s"https://api.anthropic.com/v1/messages/batches/$id/results")
    )
    // Reversed on purpose. The real API returns results in arbitrary order (verified 2026-08-11: a batch submitted
    // a, b, c came back c, b, a), so a double that preserved submission order would let a caller key results by
    // position, pass every test, and mismatch every club in production.
    val lines = details.reverse.map { case (customId, detail) =>
      Json.stringify(Json.toJson(ClaudeBatchResult(customId = customId, result = detail)))
    }
    batches.put(id, (batch, lines))
    batch
  }

  override def getClaudeBatchById(
    id: String,
    requestHeaders: Seq[(String, String)] = Nil
  ): Future[ClaudeBatch] = lookup(id).map(_._1)

  override def cancelClaudeBatchById(
    id: String,
    requestHeaders: Seq[(String, String)] = Nil
  ): Future[ClaudeBatch] = lookup(id).map { case (batch, lines) =>
    val canceling = batch.copy(cancelInitiatedAt = Some(DateTime.now()))
    batches.put(id, (canceling, lines))
    canceling
  }

  override def fetchBatchResults(
    batch: ClaudeBatch,
    requestHeaders: Seq[(String, String)] = Nil
  ): Future[Seq[String]] = lookup(batch.id).map(_._2)

  private def lookup(id: String): Future[(ClaudeBatch, Seq[String])] =
    Option(batches.get(id)) match {
      case Some(found) => Future.successful(found)
      case None => Future.failed(new IllegalArgumentException(s"No such batch '$id'"))
    }

  /** The response a batched request produces. Its output format is discovered from `output_config.format` rather than
    * from the `X-Output-Format` header [[createMessage]] uses, because a batch carries no per-request headers -- which
    * is also closer to how the real API decides.
    */
  private def batchResponse(body: ClaudeRequest): ClaudeResponse = {
    require(body.messages.nonEmpty, "messages: at least one message is required")
    require(
      body.messages.lastOption.forall(_.role != ClaudeRole.Assistant),
      "Request must not end with an assistant message (assistant-turn prefill is rejected by the Claude API)"
    )
    val responseText = body.outputConfig.flatMap(_.format).map(_.schema) match {
      case None => "This is a test plain text response."
      case Some(schema) =>
        val format = ClaudeOutputFormats.all
          .find(_.toApi.schema == schema)
          .getOrElse(sys.error("Could not identify json schema from output_config.format"))
        Json.prettyPrint(postClaudeRequest(body, expectValid(toTestResponseType(format))))
    }
    response(Seq(ClaudeClient.textBlock(responseText)), ClaudeStopReason.EndTurn, ClaudeServiceTier.Batch)
  }

  private def toTestResponseType(f: ClaudeOutputFormat): ValidatedNec[String, TestResponseFormat] = {
    f match {
      case ClaudeOutputFormats.CommentsResponse => TestResponseFormat.Comments.validNec
      case ClaudeOutputFormats.RecommendationsResponse => TestResponseFormat.Recommendations.validNec
      case ClaudeOutputFormats.SingleInsight => TestResponseFormat.SingleInsight.validNec
      case ClaudeOutputFormats.InsightSectionsResponse => TestResponseFormat.InsightSections.validNec
      case other => s"Could not find test response format for class ${other.getClass.getName}".invalidNec
    }
  }

  private def expectValid[T](r: ValidatedNec[String, T]): T = {
    r match {
      case Invalid(e) => sys.error(e.toNonEmptyList.toList.mkString(", "))
      case Valid(result) => result
    }
  }
}

abstract class TestResponseFormat(@scala.annotation.nowarn("msg=unused") schema: ClaudeOutputFormat) {

  def generateResponse(claudeRequest: ClaudeRequest): JsValue

  protected def steps: Seq[ClaudeStep] = Seq(
    ClaudeStep(
      explanation = "Test explanation",
      output = "Test result"
    )
  )
}

object TestResponseFormat {
  val Comments: TestResponseFormat = new TestResponseFormat(ClaudeOutputFormats.CommentsResponse) {

    override def generateResponse(claudeRequest: ClaudeRequest): JsValue = buildJs(
      Seq("Test comment")
    )

    def buildJs(comments: Seq[String]): JsValue = Json.toJson(
      CommentsResponse(
        steps = steps,
        comments = comments
      )
    )
  }

  val Recommendations: TestResponseFormat = new TestResponseFormat(ClaudeOutputFormats.RecommendationsResponse) {
    override def generateResponse(claudeRequest: ClaudeRequest): JsValue = buildJs(
      Seq(
        Recommendation(category = "coffee", confidence = 75),
        Recommendation(category = "restaurants", confidence = 50)
      )
    )

    def buildJs(recommendations: Seq[Recommendation]): JsValue = Json.toJson(
      RecommendationResponse(
        steps = steps,
        recommendations = recommendations
      )
    )

  }

  val SingleInsight: TestResponseFormat = new TestResponseFormat(ClaudeOutputFormats.SingleInsight) {
    override def generateResponse(claudeRequest: ClaudeRequest): JsValue = buildJs("You are doing amazing")

    def buildJs(insight: String): JsValue = Json.toJson(
      SingleInsightResponse(
        steps = steps,
        insight = insight
      )
    )
  }

  val InsightSections: TestResponseFormat = new TestResponseFormat(ClaudeOutputFormats.InsightSectionsResponse) {
    override def generateResponse(claudeRequest: ClaudeRequest): JsValue = buildJs(
      Seq(
        InsightSection(
          title = "Test Section",
          icon = "chart",
          items = Some(Seq("Test insight item"))
        )
      )
    )

    def buildJs(sections: Seq[InsightSection]): JsValue = Json.toJson(
      InsightSectionsResponse(
        steps = steps,
        sections = sections
      )
    )
  }
}
