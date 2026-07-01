package com.bryzek.ai.claude

import play.api.libs.json.{JsObject, JsValue, Json}

import scala.util.Try

/** Tolerant extraction of JSON from an LLM text response.
  *
  * Large language models frequently wrap their JSON output in markdown code fences (```json ... ```) or surround it
  * with prose, even when the prompt explicitly asks them not to. Handing that raw text to `Json.parse` then fails on
  * the leading backtick. These helpers strip a surrounding markdown fence and, as a last resort, isolate the object
  * between the first `{` and the last `}` before parsing.
  */
object ClaudeJson {

  /** Parse a JSON value from `text`, tolerating a surrounding markdown code fence and leading/trailing prose. Returns
    * Left with a human-readable message when no valid JSON can be recovered.
    */
  def extractJson(text: String): Either[String, JsValue] = {
    val stripped = stripFences(text)
    Try(Json.parse(stripped)).toEither match {
      case Right(js) => Right(js)
      case Left(ex) =>
        objectSubstring(stripped) match {
          case Some(sub) => Try(Json.parse(sub)).toEither.left.map(e => s"Invalid JSON: ${e.getMessage}")
          case None => Left(s"Invalid JSON: ${ex.getMessage}")
        }
    }
  }

  /** Like [[extractJson]] but yields the value as a JsObject, or None when no JSON object can be recovered. Convenience
    * for callers that require an object.
    */
  def extractJsonObject(text: String): Option[JsObject] =
    extractJson(text).toOption
      .flatMap(_.asOpt[JsObject])
      .orElse(objectSubstring(stripFences(text)).flatMap(s => Try(Json.parse(s)).toOption).flatMap(_.asOpt[JsObject]))

  private def stripFences(text: String): String =
    text.trim
      .replaceAll("(?s)^```(?:json)?\\s*", "")
      .replaceAll("(?s)```\\s*$", "")
      .trim

  private def objectSubstring(stripped: String): Option[String] = {
    val start = stripped.indexOf('{')
    val end = stripped.lastIndexOf('}')
    if (start >= 0 && end > start) Some(stripped.substring(start, end + 1)) else None
  }
}
