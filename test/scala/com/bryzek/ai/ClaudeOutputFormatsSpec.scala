package com.bryzek.ai.claude

import com.bryzek.claude.models.{ClaudeModel, ClaudeRequest, ClaudeResponse, ClaudeRole}
import com.bryzek.ai.UnlimitedAiBudget
import helpers.{ApiSpecs, FutureHelpers, TestAiFeature}
import org.apache.pekko.util.Timeout
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsNull, JsObject, Json}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, SECONDS}

/** Every structured output schema [[ClaudeOutputFormats]] sends is the JSON schema of the apibuilder model of the same
  * name in api's `spec/ai/claude-response.json`. The schema is written by hand, and structured outputs constrain the model to
  * it, so a field the model gains and the schema lacks is a field Claude can never return: ISS-45418, where
  * `recommendation_response.type_label` was parsed and never sent.
  */
class ClaudeOutputFormatsSpec extends AnyWordSpec with Matchers with FutureHelpers {

  private implicit val timeout: Timeout = FiniteDuration(30, SECONDS)

  private val models: Map[String, JsObject] =
    (Json.parse(ApiSpecs.read("spec/ai/claude-response.json")) \ "models").as[Map[String, JsObject]]

  private case class Field(name: String, `type`: String, required: Boolean)

  private def fields(model: String): Seq[Field] =
    (models(model) \ "fields").as[Seq[JsObject]].map { f =>
      Field((f \ "name").as[String], (f \ "type").as[String], (f \ "required").asOpt[Boolean].getOrElse(true))
    }

  /** Every way `schema` differs from the JSON schema the apibuilder type `apiType` describes. */
  private def differences(apiType: String, schema: JsObject, path: String): Seq[String] = {
    def expectType(t: String): Seq[String] =
      if ((schema \ "type").asOpt[String].contains(t)) Nil
      else Seq(s"$path: type is ${(schema \ "type").asOpt[String].getOrElse("absent")}, the model says $t ($apiType)")

    apiType match {
      case "string" => expectType("string")
      case "long" | "integer" => expectType("integer")
      case "double" | "decimal" => expectType("number")
      case "boolean" => expectType("boolean")
      case list if list.startsWith("[") && list.endsWith("]") =>
        expectType("array") ++ ((schema \ "items").asOpt[JsObject] match {
          case Some(items) => differences(list.drop(1).dropRight(1), items, s"$path[]")
          case None => Seq(s"$path: an array with no items schema")
        })
      case model if models.contains(model) => expectType("object") ++ objectDifferences(model, schema, path)
      case other => Seq(s"$path: no JSON schema mapping for apibuilder type '$other'")
    }
  }

  private def objectDifferences(model: String, schema: JsObject, path: String): Seq[String] = {
    val properties = (schema \ "properties").asOpt[JsObject].getOrElse(Json.obj())
    val required = (schema \ "required").asOpt[Seq[String]].getOrElse(Nil).toSet
    val modelFields = fields(model)
    val missing = modelFields.filterNot(f => properties.keys.contains(f.name)).map { f =>
      s"$path.${f.name}: a field of $model, absent from the schema's properties"
    }
    val extra = properties.keys.toSeq.sorted.filterNot(modelFields.map(_.name).contains).map { k =>
      s"$path.$k: in the schema, not a field of $model"
    }
    val requiredness = modelFields.filter(f => properties.keys.contains(f.name) && f.required != required(f.name)).map {
      f => s"$path.${f.name}: required is ${required(f.name)} in the schema, ${f.required} in $model"
    }
    val closed =
      if ((schema \ "additionalProperties").asOpt[Boolean].contains(false)) Nil
      else Seq(s"$path: additionalProperties is not false")
    val nested = modelFields.flatMap { f =>
      (properties \ f.name).asOpt[JsObject].toSeq.flatMap(differences(f.`type`, _, s"$path.${f.name}"))
    }
    missing ++ extra ++ requiredness ++ closed ++ nested
  }

  "ClaudeOutputFormats" should {

    ClaudeOutputFormats.all.foreach { format =>
      s"send ${format.name} as exactly the schema of the ${format.name} model" in {
        models.keySet must contain(format.name)
        objectDifferences(format.name, format.schema, "$") mustBe Nil
      }
    }

    "flag a field the model has and the schema lacks" in {
      val schema = ClaudeOutputFormats.RecommendationsResponse.schema
      val withoutTypeLabel = schema ++ Json.obj("properties" -> ((schema \ "properties").as[JsObject] - "type_label"))
      objectDifferences("recommendation_response", withoutTypeLabel, "$") mustBe Seq(
        "$.type_label: a field of recommendation_response, absent from the schema's properties"
      )
    }

    "send type_label as an optional property of the recommendation output format" in {
      val recorded = scala.collection.mutable.Buffer.empty[ClaudeRequest]
      val recording = new TestClaudeClient {
        override def createMessage(
          body: ClaudeRequest,
          requestHeaders: Seq[(String, String)]
        ): Future[ClaudeResponse] = {
          recorded.synchronized { recorded += body }
          super.createMessage(body, requestHeaders)
        }
      }
      val client = ClaudeClient(recording, ClaudeConfig("test-api-key"), NoopClaudeStore, new UnlimitedAiBudget())
      val request = AiRequest(messages = Seq(ClaudeClient.makeClaudeMessage(ClaudeRole.User, "label this merchant")))

      val result = await(client.chatRecommendations(request, Seq(ClaudeModel.ClaudeSonnet5), TestAiFeature))
      result.toOption.flatMap(_.typeLabel) mustBe Some("coffee shop")

      val sent = recorded.synchronized(recorded.toList).flatMap(_.outputConfig.flatMap(_.format)).map(_.schema)
      sent.size mustBe 1
      (sent.head \ "properties" \ "type_label" \ "type").asOpt[String] mustBe Some("string")
      (sent.head \ "required").as[Seq[String]] must not contain "type_label"
    }
  }

  "ClaudeOutputFormats.create" should {

    def refusal(f: => ClaudeOutputFormat): String = intercept[IllegalArgumentException](f).getMessage

    "refuse a JSON null, naming its path" in {
      refusal(
        ClaudeOutputFormats.create("x", Json.obj("verdict" -> Json.obj("type" -> "string", "enum" -> JsNull)), Nil)
      ) must include("$.properties.verdict.enum")
    }

    "refuse a String val read before its object initialised it" in {
      // The judge's defect (ISS-116739): the schema val is declared above the val that supplies its enum value.
      class InitOrder {
        val format: ClaudeOutputFormat = ClaudeOutputFormats.create(
          "judgement",
          Json.obj("verdict" -> Json.obj("type" -> "string", "enum" -> Json.arr("pass", Verdict))),
          Seq("verdict")
        )
        val Verdict: String = "fail"
      }
      val message = refusal(new InitOrder().format)
      message must include("judgement")
      message must include("$.properties.verdict.enum[1]")
    }

    "refuse a null required name" in {
      refusal(
        ClaudeOutputFormats.create("x", Json.obj("a" -> Json.obj("type" -> "string")), Seq("a", null))
      ) must include("$.required[1]")
    }

    "accept every format lib-ai sends" in {
      ClaudeOutputFormats.all.foreach { f =>
        ClaudeOutputFormat.nullPaths(f.schema, "$") mustBe Nil
        ClaudeOutputFormat.unionTypedEnumPaths(f.schema, "$") mustBe Nil
      }
    }
  }

  /** The checks hold for a format built directly, not only through [[ClaudeOutputFormats.create]]: playbook's
    * ConsentScanModel built one as `ClaudeOutputFormat(name, schema)` and the API refused every call with a 400.
    * ISS-162487
    */
  "ClaudeOutputFormat" should {

    def refusal(f: => ClaudeOutputFormat): String = intercept[IllegalArgumentException](f).getMessage

    def objectSchema(properties: JsObject): JsObject = Json.obj(
      "type" -> "object",
      "properties" -> properties,
      "required" -> Json.arr(),
      "additionalProperties" -> false
    )

    "refuse a JsNull enum value, naming its path" in {
      val message = refusal(
        ClaudeOutputFormat(
          "x",
          objectSchema(Json.obj("verdict" -> Json.obj("type" -> "string", "enum" -> Json.arr("pass", JsNull))))
        )
      )
      message must include("ClaudeOutputFormat(x)")
      message must include("$.properties.verdict.enum[1]")
    }

    "refuse an enum under a union type, naming its path and the anyOf form" in {
      val message = refusal(
        ClaudeOutputFormat(
          "x",
          objectSchema(
            Json.obj(
              "outer" -> Json.obj(
                "type" -> "object",
                "properties" -> Json.obj(
                  "kind" -> Json.obj("type" -> Json.arr("string", "null"), "enum" -> Json.arr("a", "b"))
                )
              )
            )
          )
        )
      )
      message must include("$.properties.outer.properties.kind")
      message must include("anyOf")
    }

    "refuse a union-typed enum inside an array's items" in {
      refusal(
        ClaudeOutputFormat(
          "x",
          objectSchema(
            Json.obj(
              "kinds" -> Json.obj(
                "type" -> "array",
                "items" -> Json.obj("type" -> Json.arr("string", "null"), "enum" -> Json.arr("a"))
              )
            )
          )
        )
      ) must include("$.properties.kinds.items")
    }

    "offer no copy and no new that would skip the check" in {
      assertDoesNotCompile("""ClaudeOutputFormats.SingleInsight.copy(name = "y")""")
      assertDoesNotCompile("""new ClaudeOutputFormat("y", play.api.libs.json.Json.obj())""")
    }

    "accept a nullable enum written as anyOf" in {
      val format = ClaudeOutputFormat(
        "x",
        objectSchema(
          Json.obj(
            "kind" -> Json.obj(
              "anyOf" -> Json.arr(
                Json.obj("type" -> "string", "enum" -> Json.arr("a", "b")),
                Json.obj("type" -> "null")
              )
            ),
            "note" -> Json.obj("type" -> Json.arr("string", "null"))
          )
        )
      )
      format.name mustBe "x"
    }

    "build the probe's deliberately broken control through uncheckedForProbes alone" in {
      val schema =
        objectSchema(Json.obj("kind" -> Json.obj("type" -> Json.arr("string", "null"), "enum" -> Json.arr("a"))))
      refusal(ClaudeOutputFormat("probe", schema)) must include("$.properties.kind")
      ClaudeOutputFormat.uncheckedForProbes("probe", schema).schema mustBe schema
    }
  }

  "TestClaudeClient.schemaViolations" should {

    val schema = ClaudeOutputFormats.RecommendationsResponse.schema

    "accept a response the schema allows, with or without the optional field" in {
      val base =
        Json.obj("steps" -> Json.arr(), "recommendations" -> Json.arr(Json.obj("category" -> "a", "confidence" -> 5)))
      TestClaudeClient.schemaViolations(schema, base) mustBe Nil
      TestClaudeClient.schemaViolations(schema, base ++ Json.obj("type_label" -> "gym")) mustBe Nil
    }

    "name a property the schema does not declare, a missing required one and a wrong type" in {
      val bad = Json.obj(
        "steps" -> Json.arr(),
        "recommendations" -> Json.arr(Json.obj("category" -> 1, "confidence" -> 5)),
        "extra" -> true
      )
      TestClaudeClient.schemaViolations(
        Json.obj(
          "type" -> "object",
          "properties" -> Json.obj(),
          "required" -> Json.arr("insight"),
          "additionalProperties" -> false
        ),
        Json.obj()
      ) mustBe Seq(
        "$.insight: required but absent"
      )
      TestClaudeClient.schemaViolations(schema, bad) mustBe Seq(
        "$.extra: not a property of the schema",
        "$.recommendations[0].category: expected a string"
      )
    }
  }
}
