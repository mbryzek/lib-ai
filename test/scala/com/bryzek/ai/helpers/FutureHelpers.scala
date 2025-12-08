package helpers

import org.apache.pekko.util.Timeout
import org.scalatest.concurrent.Eventually._
import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.concurrent.{Await, Future}
import org.scalatest.time.{Seconds, Span}

trait FutureHelpers {

  def eventuallyInNSeconds[T](n: Long = 3)(f: => T): T = {
    eventually(timeout(Span(n, Seconds))) {
      f
    }
  }

  private implicit val defaultTimeout: Timeout = FiniteDuration(5, SECONDS)

  def awaitNSeconds[T](n: Long = 5)(future: Future[T]): T = {
    await(future)(using Timeout(FiniteDuration(n, SECONDS)))
  }

  def await[T](future: Future[T])(implicit timeout: Timeout = defaultTimeout): T = {
    Await.result(future, timeout.duration)
  }
}
