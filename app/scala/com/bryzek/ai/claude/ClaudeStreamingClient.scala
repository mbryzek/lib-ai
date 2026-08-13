package com.bryzek.ai.claude

import com.bryzek.claude.client.{Client, IClient}
import com.bryzek.claude.models.json.*
import com.bryzek.claude.models.{ClaudeBatch, ClaudeBatchForm, ClaudeRequest, ClaudeResponse}
import org.apache.pekko.stream.Materializer
import org.apache.pekko.stream.scaladsl.Framing
import org.apache.pekko.util.ByteString
import play.api.libs.json.{JsObject, JsTrue, Json}
import play.api.libs.ws.{WSClient, WSResponse, writeableOf_JsValue}

import java.io.IOException
import scala.concurrent.duration.{FiniteDuration, MINUTES}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object ClaudeStreamingClient {

  /** No bytes at all for this long means the connection is dead, not that the model is busy. This is the ONLY limit
    * that binds a healthy streaming request, and it replaces a wall-clock ceiling with the question that actually
    * distinguishes a working request from a broken one.
    *
    * Sized from the quietest thing a live stream does. Measured against api.anthropic.com on 2026-08-09, a request
    * still thinking and emitting no content sends a `ping` event every 29-30 seconds, and once generation starts deltas
    * arrive continuously (a 21,305-token answer streamed over 245 seconds with no gap approaching a second). Two
    * minutes is four consecutive missed pings -- far outside anything a working connection does, and, unlike a
    * total-duration cap, it does not get tighter as the request gets bigger.
    */
  val IdleTimeout: FiniteDuration = FiniteDuration(2, MINUTES)

  /** Backstop on total wall clock for one attempt, so a pathologically slow but never-quite-idle stream cannot hold a
    * connection forever. It should never be what ends a request and in practice cannot be: at the ~87 output
    * tokens/second measured above, even the largest budget any model here accepts (128k) finishes in ~25 minutes.
    * Coincides with the ceiling the official TypeScript SDK scales its own non-streaming timeout up to.
    */
  val RequestTimeout: FiniteDuration = FiniteDuration(60, MINUTES)

  /** Longest single SSE line accepted. Deltas are tiny; the outliers are a `content_block_start` carrying a whole
    * `redacted_thinking` payload or a `message_start` with a large usage block.
    */
  val MaxLineBytes: Int = 4 * 1024 * 1024

  /** Cap on how much of a non-2xx body is read before it is turned into an error message. */
  val MaxErrorBodyBytes: Int = 8 * 1024

  /** Wall clock for one batch operation. Generous compared with an ordinary API call because creating a batch UPLOADS
    * every request in it -- the API's own ceiling is 256 MB -- and none of the three batch operations generates
    * anything, so a slow one is a slow transfer rather than a model still thinking.
    */
  val BatchTimeout: FiniteDuration = FiniteDuration(10, MINUTES)

  /** Ceiling on a results body held in memory at once.
    *
    * The results of a batch are read whole: they are a JSONL body with no pagination and no partial read, so this
    * implementation materializes every line. That is exactly right at the size a fan-out across clubs produces (tens of
    * requests, a few MB) and would be wrong at the API's own limit of 100,000 requests -- which is why the cap fails
    * loudly and names itself, rather than being discovered as an OutOfMemoryError on the day somebody batches a
    * backfill. Fetching results is idempotent and the body stays retrievable for 29 days, so nothing is lost by failing
    * here; a caller that genuinely needs more has to stream, which is a different method than this one.
    */
  val MaxResultsBytes: Long = 64L * 1024 * 1024

  private val Newline = ByteString("\n")

  /** Renders an error body as `type: message` when it is Anthropic's documented error envelope, and verbatim
    * (truncated) when it is anything else -- an HTML page from a proxy, say.
    */
  private[claude] def describeError(body: String): String =
    // Json.parse THROWS on a non-JSON body, so the parse has to be inside the Try, not merely its result.
    Try {
      for {
        js <- Json.parse(body).asOpt[JsObject]
        error <- (js \ "error").asOpt[JsObject]
        message <- (error \ "message").asOpt[String]
      } yield (error \ "type").asOpt[String].fold(message)(t => s"$t: $message")
    }.toOption.flatten.getOrElse(body.trim.take(1000))
}

/** The Anthropic client, streaming.
  *
  * Every request goes out with `"stream": true` and the Server-Sent Events that come back are accumulated into the same
  * [[ClaudeResponse]] a non-streaming call would have produced, so nothing above this class knows the difference.
  *
  * ==Why this is not an option==
  *
  * A non-streaming turn sends zero bytes for the entire generation, so its wall clock has to fit inside one silent HTTP
  * request. That gives two independently binding limits on every call -- the client's request timeout and Play WS's
  * idle timeout -- both of which exist only because nothing is flowing, and both of which are a cliff: a request that
  * needs one second more than the ceiling fails completely, having paid for the whole generation. ISS-1229 is that
  * failure: a 64,000-token synthesis needing ~13.8 minutes against a fixed 10-minute timeout, retried three times, each
  * attempt deterministically doomed. Sizing the ceiling to `max_tokens` raises it; it does not remove it, and the next
  * caller to raise `max_tokens` walks into the same wall.
  *
  * Streaming removes it. Bytes arrive continuously, so the idle timeout stops being a factor and total wall clock stops
  * being a cliff -- what is left is [[ClaudeStreamingClient.IdleTimeout]], which asks the only question that actually
  * distinguishes a working request from a broken one: is anything still coming? This is also Anthropic's own guidance
  * (default to streaming for long output or high `max_tokens`), and what their SDKs do: the Python SDK refuses a
  * non-streaming request it estimates will exceed ~10 minutes, and the TypeScript SDK scales its timeout to 60 minutes
  * for the same case.
  *
  * ==Why unconditionally, rather than only for large requests==
  *
  * A size-triggered switch would leave the streaming path exercised only by the rarest and most expensive requests --
  * exactly the ones that cannot afford to be the first to find a bug in it -- and would put a behaviour change on a
  * `max_tokens` threshold, where a caller nudging a number silently moves between two transports. One path, always
  * taken, is both simpler and better tested by ordinary traffic.
  *
  * Errors are raised in the shapes [[ClaudeClient]] already understands, so retry, model fallback and the audit trail
  * are unchanged: [[ClaudeStreamException]] for anything the API reported a status for, and [[IOException]] for a
  * stream that broke off mid-answer, which is the transient-transport channel that is already retried.
  */
class ClaudeStreamingClient(
  ws: WSClient,
  baseUrl: String = "https://api.anthropic.com",
  requestTimeout: FiniteDuration = ClaudeStreamingClient.RequestTimeout,
  idleTimeout: FiniteDuration = ClaudeStreamingClient.IdleTimeout
)(implicit ec: ExecutionContext, mat: Materializer)
  extends IClient
  with ClaudeBatchResults {

  import ClaudeStreamingClient.*

  /** The three JSON batch operations are delegated to the generated client rather than hand-written here. Only
    * `createMessage` needs this class's streaming transport -- a batch operation returns a small JSON document the
    * moment it is asked, so there is nothing to stream and no idle-timeout question to answer. Delegating keeps the
    * hand-written surface down to the one thing apibuilder cannot express, which is [[fetchBatchResults]].
    */
  private val batchDelegate: IClient = new Client(ws, baseUrl, defaultTimeout = BatchTimeout)

  override def createClaudeBatch(
    body: ClaudeBatchForm,
    requestHeaders: Seq[(String, String)] = Nil
  ): Future[ClaudeBatch] = batchDelegate.createClaudeBatch(body, requestHeaders)

  override def getClaudeBatchById(id: String, requestHeaders: Seq[(String, String)] = Nil): Future[ClaudeBatch] =
    batchDelegate.getClaudeBatchById(id, requestHeaders)

  override def cancelClaudeBatchById(id: String, requestHeaders: Seq[(String, String)] = Nil): Future[ClaudeBatch] =
    batchDelegate.cancelClaudeBatchById(id, requestHeaders)

  /** `results_url` is preferred over the path this client could build itself, because it is what the API says to follow
    * and it is free to point somewhere else (a signed object store, say) without this being a breaking change. The
    * canonical path is the fallback for a batch that has ended but whose url we were not given.
    */
  override def fetchBatchResults(batch: ClaudeBatch, requestHeaders: Seq[(String, String)] = Nil): Future[Seq[String]] =
    ws.url(batch.resultsUrl.getOrElse(s"$baseUrl/v1/messages/batches/${batch.id}/results"))
      .withRequestTimeout(BatchTimeout)
      .addHttpHeaders(requestHeaders*)
      .withMethod("GET")
      .stream()
      .flatMap { response =>
        if (response.status / 100 == 2) collectLines(response) else raise(response)
      }

  private def collectLines(response: WSResponse): Future[Seq[String]] =
    response.bodyAsSource
      .idleTimeout(idleTimeout)
      .via(Framing.delimiter(Newline, MaxLineBytes, allowTruncation = true))
      .runFold((Vector.empty[String], 0L)) { case ((lines, bytes), line) =>
        val total = bytes + line.length
        // Not an IOException: that is the retryable-transport channel, and a body that is too big is too big on
        // every attempt. Failing here rather than retrying it three times is the whole point.
        if (total > MaxResultsBytes) {
          throw new IllegalStateException(
            s"Batch results exceed ${MaxResultsBytes} bytes; this client reads them whole (see MaxResultsBytes)"
          )
        }
        (lines :+ line.utf8String, total)
      }
      .map(_._1)

  override def createMessage(
    body: ClaudeRequest,
    requestHeaders: Seq[(String, String)] = Nil
  ): Future[ClaudeResponse] = {
    // `stream` is set here rather than on the apibuilder spec on purpose: it is a property of this transport, not of
    // the request every caller builds, and adding it to the shared spec would put it on the generated client in
    // every consuming repo without any of them being able to honor it.
    val payload = Json.toJson(body).as[JsObject] + ("stream" -> JsTrue)

    ws.url(s"$baseUrl/v1/messages")
      .withRequestTimeout(requestTimeout)
      .addHttpHeaders(requestHeaders*)
      .addHttpHeaders("Accept" -> "text/event-stream")
      .withMethod("POST")
      .withBody(payload)
      .stream()
      .flatMap { response =>
        if (response.status / 100 == 2) accumulate(response) else raise(response)
      }
  }

  private def accumulate(response: WSResponse): Future[ClaudeResponse] =
    response.bodyAsSource
      .idleTimeout(idleTimeout)
      .via(Framing.delimiter(Newline, MaxLineBytes, allowTruncation = true))
      .runFold(ClaudeStream.Empty)((acc, line) => acc.line(line.utf8String))
      .flatMap { acc =>
        acc.outcome match {
          case ClaudeStreamOutcome.Completed(r) => Future.successful(r)
          case ClaudeStreamOutcome.Failed(Some(status), message) =>
            Future.failed(
              ClaudeStreamException(
                status,
                message,
                response.header("Retry-After"),
                ClaudeErrorLabels.providerId(response.headers)
              )
            )
          // No status means the stream itself broke its contract -- truncated, or malformed. Raised as an
          // IOException so it lands in the same transient-transport bucket as a dropped connection and is retried.
          case ClaudeStreamOutcome.Failed(None, message) =>
            Future.failed(new IOException(s"Claude stream did not complete: $message"))
        }
      }

  private def raise[T](response: WSResponse): Future[T] =
    response.bodyAsSource
      .idleTimeout(idleTimeout)
      .runFold(ByteString.empty)((acc, b) => if (acc.length >= MaxErrorBodyBytes) acc else acc ++ b)
      .flatMap { body =>
        Future.failed(
          ClaudeStreamException(
            response.status,
            describeError(body.utf8String),
            response.header("Retry-After"),
            ClaudeErrorLabels.providerId(response.headers)
          )
        )
      }
}
