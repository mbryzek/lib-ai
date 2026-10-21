package com.bryzek.ai.claude

import generated.errors.{ApiException, ClaudeErrorResponseResponse}

import java.io.IOException
import java.net.UnknownHostException
import java.nio.channels.ClosedByInterruptException
import java.util.concurrent.{Executors, ThreadFactory, TimeUnit, TimeoutException}
import javax.net.ssl.SSLException
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Random, Try}

/** Retry policy shared by every HTTP call this library makes -- a message, and the three batch operations.
  *
  * Extracted from [[ClaudeClient]] rather than reimplemented for batches. The decisions here are about the TRANSPORT
  * (which statuses are worth another attempt, how long to wait, which IOExceptions are permanent), and none of them
  * differ between posting a message and posting a batch. A second copy would drift: the batch endpoints return the same
  * 429 with the same `Retry-After` header, and the first thing a divergent copy would lose is honoring it.
  */
private[claude] object ClaudeRetries {

  /** Total HTTP attempts before failing (initial call + retries). */
  val MaxHttpAttempts = 3

  private val scheduler = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
    override def newThread(r: Runnable): Thread = {
      val t = new Thread(r, "claude-retry-scheduler")
      t.setDaemon(true)
      t
    }
  })

  /** Non-blocking delay (no Thread.sleep) used to space out retries. */
  private def delay(d: FiniteDuration): Future[Unit] = {
    val p = Promise[Unit]()
    scheduler.schedule(
      new Runnable { override def run(): Unit = p.success(()) },
      d.toMillis,
      TimeUnit.MILLISECONDS
    )
    p.future
  }

  /** Retry the given HTTP attempt, honoring `Retry-After` on 429 and using jittered backoff on 5xx and transient
    * transport failures (read/request timeouts, connection resets). Non-retryable failures (or exhausted attempts)
    * propagate the original exception.
    *
    * Every request gets the same [[MaxHttpAttempts]], regardless of its size. ISS-1229 briefly traded attempts away on
    * large requests, because a non-streaming attempt had to be given a bigger and bigger slice of a fixed wall-clock
    * envelope to have any chance of finishing. Streaming makes that trade backwards: a failing streaming attempt fails
    * on [[ClaudeStreamingClient.IdleTimeout]] in minutes rather than burning its whole ceiling, so a large request can
    * afford its retries as easily as a small one -- and, being large, has more to lose by not having them.
    *
    * `beforeRetry` is asked immediately before every attempt after the first, once its delay has passed; a `Some`
    * fails the call with that throwable instead of making the attempt. [[ClaudeClient]] passes its budget check, so a
    * retry waiting out a 429 is not sent for a feature another call exhausted during the wait.
    */
  def withRetries[T](backoff: ClaudeBackoff, beforeRetry: () => Option[Throwable] = () => None)(
    attempt: => Future[T]
  )(implicit ec: ExecutionContext): Future[T] = {
    def retry(n: Int): Future[T] = beforeRetry() match {
      case Some(stop) => Future.failed(stop)
      case None => loop(n)
    }
    def loop(n: Int): Future[T] =
      attempt.recoverWith {
        case NonFatal(e) if n < MaxHttpAttempts =>
          retryDelay(e, backoff) match {
            case Some(d) if d.length <= 0 => retry(n + 1)
            case Some(d) => delay(d).flatMap(_ => retry(n + 1))
            case None => Future.failed(e)
          }
      }
    loop(1)
  }

  private def retryDelay(e: Throwable, backoff: ClaudeBackoff): Option[FiniteDuration] = e match {
    case r: ClaudeErrorResponseResponse =>
      r.response.status match {
        case 429 => Some(retryAfter(r.response.header("Retry-After"), backoff))
        case s if s >= 500 => Some(backoff.next())
        case _ => None
      }
    // The streaming transport reports statuses through its own exception rather than a WSResponse (a streamed
    // response has no materialized body to hand back), so it gets the same status-driven decision.
    case s: ClaudeStreamException =>
      s.status match {
        case 429 => Some(retryAfter(s.retryAfter, backoff))
        case c if c >= 500 => Some(backoff.next())
        case _ => None
      }
    case a: ApiException if a.response.status >= 500 => Some(backoff.next())
    // AsyncHttpClient surfaces read/request timeouts as j.u.c.TimeoutException and dropped connections as
    // IOException; both are transient transport failures, not API rejections. Known-permanent IOException
    // subtypes (TLS/config, DNS, thread interrupt) fail fast instead of burning retries -- an exclusion
    // list, because the transient drops we do want to retry surface as bare IOException (e.g. AHC's
    // "Remotely closed") with no dedicated subtype to allow-list.
    case _: SSLException | _: UnknownHostException | _: ClosedByInterruptException => None
    case _: TimeoutException => Some(backoff.next())
    case _: IOException => Some(backoff.next())
    case _ => None
  }

  private def retryAfter(header: Option[String], backoff: ClaudeBackoff): FiniteDuration =
    header
      .flatMap(h => Try(h.trim.toLong).toOption)
      .map(s => FiniteDuration(s, TimeUnit.SECONDS))
      .getOrElse(backoff.next())
}

/** How long to wait before retrying a failure the API did not put a `Retry-After` on: `base` plus a uniformly random
  * share of `spread`, so a burst of callers that failed together does not retry together.
  *
  * A `Retry-After` the API sends is honored as sent whatever this says -- it is the server's instruction, not a guess.
  */
case class ClaudeBackoff(base: FiniteDuration, spread: FiniteDuration) {
  def next(): FiniteDuration =
    if (spread.toMillis <= 0) base
    else base + FiniteDuration(Random.nextLong(spread.toMillis), MILLISECONDS)
}

object ClaudeBackoff {

  /** 500ms to 1s. */
  val Default: ClaudeBackoff = ClaudeBackoff(FiniteDuration(500, MILLISECONDS), FiniteDuration(500, MILLISECONDS))

  /** Retry at once. For tests, which exercise the retry DECISIONS and gain nothing by waiting between them. */
  val Immediate: ClaudeBackoff = ClaudeBackoff(FiniteDuration(0, MILLISECONDS), FiniteDuration(0, MILLISECONDS))
}
