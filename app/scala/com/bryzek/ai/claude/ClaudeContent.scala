package com.bryzek.ai.claude

import cats.data.ValidatedNec
import cats.implicits.*
import com.bryzek.claude.models.{ClaudeContentType, ClaudeError, ClaudeResponse}
import play.api.libs.json.{JsError, JsSuccess, Json, Reads}

import scala.util.{Failure, Success, Try}

/** Turning a [[ClaudeResponse]] into the answer a caller asked for.
  *
  * Shared by the synchronous path ([[ClaudeClient]]) and the batch path ([[ClaudeBatchClient]]) so that a request
  * answers identically whichever one carried it. That matters more than it sounds: the pipeline's recovery logic keys
  * off the exact WORDING of these messages ([[ClaudeClient.isBudgetExhaustionError]] matches "No text content" and
  * "Unexpected end-of-input"), so a batch path with its own hand-rolled parse would silently lose budget-exhaustion
  * recovery for every request it carried, with nothing failing to compile to say so.
  *
  * `err` is how an error is labelled with its caller's identity -- [[ClaudeRequestMetadata.error]] on the synchronous
  * path, which appends a request id; the custom id on the batch path, which is the only handle a batched request has.
  */
private[claude] object ClaudeContent {

  type ErrorFactory = (String, Option[String]) => ClaudeError

  /** Concatenates the text of all `text` content blocks, skipping thinking / tool_use / tool_result blocks. */
  def text(response: ClaudeResponse): String =
    response.content.flatMap(b => if (b.`type` == ClaudeContentType.Text) b.text else None).mkString("\n")

  /** A response with no `text` content block -- almost always `stop_reason=max_tokens` where extended thinking consumed
    * the whole output budget before any answer was emitted, but also a refusal or a pure-thinking turn. Names
    * `stop_reason` + `output_tokens` in the message so the failure is self-diagnosing on the run page and in the
    * persisted audit error, instead of an opaque "no content".
    */
  def noTextError(err: ErrorFactory, response: ClaudeResponse): ClaudeError =
    err(
      s"No text content in response (stop_reason=${response.stopReason}, output_tokens=${response.usage.outputTokens})",
      None
    )

  /** The response's text, or a [[noTextError]] when it has none. */
  def requireText(err: ErrorFactory, response: ClaudeResponse): ValidatedNec[ClaudeError, String] =
    text(response) match {
      case t if t.nonEmpty => t.validNec
      case _ => noTextError(err, response).invalidNec
    }

  /** Parses the response's text as `T`. With structured outputs Claude returns clean JSON without markdown delimiters,
    * so this is a strict parse; the tolerant fence-stripping recovery lives in [[ClaudeJson]] for callers reading
    * prose.
    */
  def parse[T](err: ErrorFactory, response: ClaudeResponse)(implicit
    reads: Reads[T]
  ): ValidatedNec[ClaudeError, T] =
    requireText(err, response).andThen { content =>
      def parseError(msg: String) = err(msg, Some(text(response))).invalidNec

      Try(Json.parse(content.trim)) match {
        case Failure(ex) => parseError(s"Content is not valid JSON: ${ex.getMessage}")
        case Success(js) =>
          js.validate[T] match {
            case JsSuccess(value, _) => value.validNec
            case JsError(errors) =>
              val messages = errors.flatMap(e => e._2.map(m => s"${e._1}: ${m.message}"))
              parseError(s"Content is not valid: ${messages.mkString(", ")}")
          }
      }
    }
}
