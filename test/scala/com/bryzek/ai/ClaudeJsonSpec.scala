package com.bryzek.ai.claude

import play.api.libs.json.{JsObject, Json}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClaudeJsonSpec extends AnyWordSpec with Matchers {

  "ClaudeJson.extractJson" should {
    "parse a bare JSON object" in {
      ClaudeJson.extractJson("""{"a":1}""") mustBe Right(Json.obj("a" -> 1))
    }

    "strip a ```json fenced object (the weekly-insight failure case)" in {
      val fenced =
        """```json
          |{
          |  "corrections": [],
          |  "content": {"headline": "hi"}
          |}
          |```""".stripMargin
      ClaudeJson.extractJson(fenced) mustBe Right(
        Json.obj("corrections" -> Json.arr(), "content" -> Json.obj("headline" -> "hi"))
      )
    }

    "strip a bare ``` fence with no language tag" in {
      ClaudeJson.extractJson("```\n{\"a\":1}\n```") mustBe Right(Json.obj("a" -> 1))
    }

    "tolerate surrounding whitespace and newlines" in {
      ClaudeJson.extractJson("\n\n  {\"a\":1}  \n") mustBe Right(Json.obj("a" -> 1))
    }

    "recover an object surrounded by prose" in {
      ClaudeJson.extractJson("Here is the result: {\"a\":1} — hope that helps!") mustBe
        Right(Json.obj("a" -> 1))
    }

    "parse a JSON array" in {
      ClaudeJson.extractJson("[1,2,3]").map(_.toString) mustBe Right("[1,2,3]")
    }

    "return Left for text with no JSON" in {
      ClaudeJson.extractJson("no json here").isLeft mustBe true
    }

    "return Left for an empty string" in {
      ClaudeJson.extractJson("").isLeft mustBe true
    }
  }

  "ClaudeJson.extractJsonObject" should {
    "return the object for fenced JSON" in {
      ClaudeJson.extractJsonObject("```json\n{\"a\":1}\n```") mustBe Some(Json.obj("a" -> 1).as[JsObject])
    }

    "return None when the JSON is not an object" in {
      ClaudeJson.extractJsonObject("[1,2,3]") mustBe None
    }

    "return None for text with no JSON" in {
      ClaudeJson.extractJsonObject("nope") mustBe None
    }
  }
}
