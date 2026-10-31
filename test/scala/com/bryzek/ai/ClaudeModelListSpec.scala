package com.bryzek.ai.claude

import com.bryzek.claude.models.{ClaudeModel, ClaudeModelInfo, ClaudeModelPage}
import helpers.FutureHelpers
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/** Reading `GET /v1/models` and finding a carried model's `retires_at` in it. */
class ClaudeModelListSpec extends AnyWordSpec with Matchers with FutureHelpers {

  private val Created = new DateTime(2025, 10, 15, 0, 0, DateTimeZone.UTC)
  private val Retires = new DateTime(2026, 11, 30, 17, 0, DateTimeZone.UTC)

  private def info(id: String, retiresAt: Option[DateTime] = None) =
    ClaudeModelInfo(id, id, Created, Some("haiku"), Some("active"), None, retiresAt)

  private def config = ClaudeConfig("test").copy(backoff = ClaudeBackoff.Immediate)

  "models" must {
    "list every model this library carries from the test client" in {
      val listed = await(ClaudeModelListClient(new TestClaudeClient(), config).models()).toOption.get
      listed.map(_.id) mustBe ClaudeModel.all.map(_.toString)
      listed.flatMap(_.line).distinct.sorted mustBe Seq("fable", "haiku", "opus", "sonnet")
    }

    "follow every page, asking for each after the last one's last id" in {
      val asked = mutable.Buffer.empty[Option[String]]
      val transport = new ClaudeModelList {
        override def listModels(afterId: Option[String], requestHeaders: Seq[(String, String)]) = {
          asked += afterId
          Future.successful(afterId match {
            case None => ClaudeModelPage(Seq(info("a"), info("b")), hasMore = true, Some("a"), Some("b"))
            case Some(_) => ClaudeModelPage(Seq(info("c")), hasMore = false, Some("c"), Some("c"))
          })
        }
      }
      await(ClaudeModelListClient(transport, config).models()).toOption.get.map(_.id) mustBe Seq("a", "b", "c")
      asked.toSeq mustBe Seq(None, Some("b"))
    }

    "fail the whole read when a page fails, rather than answer with part of the list" in {
      val transport = new ClaudeModelList {
        override def listModels(afterId: Option[String], requestHeaders: Seq[(String, String)]) =
          afterId match {
            case None => Future.successful(ClaudeModelPage(Seq(info("a")), hasMore = true, Some("a"), Some("a")))
            case Some(_) => Future.failed(new IllegalStateException("page two is gone"))
          }
      }
      val result = await(ClaudeModelListClient(transport, config).models())
      result.isInvalid mustBe true
      result.swap.toOption.get.head.message must include("page two is gone")
    }
  }

  "entryFor and retiresAt" must {
    val listed = Seq(
      info("claude-haiku-5-5"),
      info("claude-haiku-4-5-20251001", retiresAt = Some(Retires)),
      info("claude-sonnet-5-5")
    )

    "find a model under its alias" in {
      ClaudeModelListClient.entryFor(listed, ClaudeModel.ClaudeHaiku55).map(_.id) mustBe Some("claude-haiku-5-5")
    }

    "find a model the list names only by a dated id" in {
      ClaudeModelListClient.entryFor(listed, ClaudeModel.ClaudeHaiku45).map(_.id) mustBe
        Some("claude-haiku-4-5-20251001")
      ClaudeModelListClient.retiresAt(listed, ClaudeModel.ClaudeHaiku45) mustBe Some(Retires)
    }

    "not take one version for another that shares a prefix" in {
      ClaudeModelListClient.entryFor(Seq(info("claude-sonnet-5-5")), ClaudeModel.ClaudeSonnet5) mustBe None
      ClaudeModelListClient.retiresAt(listed, ClaudeModel.ClaudeHaiku55) mustBe None
    }
  }

  "the wire shape" must {
    "parse a page as the live API answers it" in {
      // Captured from GET /v1/models on 2026-10-31, trimmed to one model and its capabilities dropped: the fields this
      // library does not model are ignored, not refused.
      val body =
        """{"data":[{"type":"model","id":"claude-sonnet-4-5-20250929","display_name":"Claude Sonnet 4.5",
          |"created_at":"2025-09-29T00:00:00Z","line":"sonnet","max_input_tokens":1000000,"max_tokens":64000,
          |"capabilities":{"batch":{"supported":true}},"lifecycle":"deprecated","deprecated_at":null,
          |"retires_at":"2026-11-30T17:00:00Z"}],"has_more":false,"first_id":"claude-sonnet-4-5-20250929",
          |"last_id":"claude-sonnet-4-5-20250929"}""".stripMargin
      import com.bryzek.claude.models.json.*
      val page = Json.parse(body).as[ClaudeModelPage]
      page.hasMore mustBe false
      page.data.map(m => (m.id, m.line, m.lifecycle)) mustBe
        Seq(("claude-sonnet-4-5-20250929", Some("sonnet"), Some("deprecated")))
      page.data.head.retiresAt.map(_.getMillis) mustBe Some(Retires.getMillis)
    }
  }
}
