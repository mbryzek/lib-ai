package com.mbryzek.ai.claude

import com.bryzek.claude.v0.models.{ClaudeError, ClaudeModel, ClaudeRequest, ClaudeResponse, ClaudeRole}
import helpers.FutureHelpers
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.play.guice.GuiceOneAppPerSuite

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext.Implicits.global

case class NoopClaudeStore() extends ClaudeStore {

  override def storeRequest(request: ClaudeRequestMetadata): Unit = {
    ()
  }

  override def storeResponseError(request: ClaudeRequestMetadata, errors: Seq[ClaudeError]): Unit = {
    ()
  }

  override def storeResponseSuccess[T](response: ClaudeResponseMetadata[T]): Unit = {
    ()
  }

}

class ClaudeClientSpec extends AnyWordSpec with Matchers with GuiceOneAppPerSuite with FutureHelpers {

  private val testClient: ClaudeClient = {
    val factory = app.injector.instanceOf[ClaudeClientFactory]
    factory.instance("test-api-key")(NoopClaudeStore())
  }

  "ClaudeClient" should {

    "parse mock response correctly" in {
      val response = await(
        testClient.chatComments(
          ClaudeEnvironment.Sandbox,
          ClaudeRequest(
            model = ClaudeModel.ClaudeSonnet420250514,
            messages = Seq(
              ClaudeClient.makeClaudeMessage(ClaudeRole.User, "Sending a test message")
            )
          )
        )
      )
      println(s"response: $response")
    }

  }

}
