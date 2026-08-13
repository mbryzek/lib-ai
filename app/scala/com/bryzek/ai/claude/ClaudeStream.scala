package com.bryzek.ai.claude

import com.bryzek.claude.models.ClaudeResponse
import com.bryzek.claude.models.json.*
import play.api.libs.json.*

import scala.util.{Failure, Success, Try}

/** Thrown when the Claude API answers a streaming request with a non-2xx status, or emits an SSE `error` event
  * mid-stream. Carries the status so [[ClaudeClient]] can make the same retry and model-fallback decisions it makes for
  * the non-streaming transport: the message embeds `status <n>` in the shape
  * [[ClaudeClient.isOverloadedError]]/[[ClaudeClient.isModelNotFoundError]] already match, and `retryAfter` carries the
  * 429 header verbatim.
  *
  * `providerRequestId` is Anthropic's own `request-id` header. This is where it matters most: every production message
  * goes through the streaming transport, so an error that omitted it here would omit it from essentially every real API
  * failure the system sees.
  */
final case class ClaudeStreamException(
  status: Int,
  body: String,
  retryAfter: Option[String] = None,
  providerRequestId: Option[String] = None
) extends RuntimeException(
    s"POST /v1/messages failed with status $status: $body" + ClaudeErrorLabels.providerSuffix(providerRequestId)
  )

/** How a stream ended. `Failed(status)` is an error the API reported and whose HTTP status is known; `Failed(None)` is
  * a stream that broke its own contract (truncated, unparseable), which the transport raises as an IOException because
  * that is the transient-transport channel [[ClaudeClient]] already retries.
  */
private[claude] sealed trait ClaudeStreamOutcome
private[claude] object ClaudeStreamOutcome {
  case class Completed(response: ClaudeResponse) extends ClaudeStreamOutcome
  case class Failed(status: Option[Int], message: String) extends ClaudeStreamOutcome
}

/** One content block being rebuilt from the stream. `content_block_start` supplies the block's shape and its
  * already-final fields; the deltas that follow append to exactly one of the text/thinking/signature/input channels.
  * Chunks are held as a Vector and joined once at the end rather than concatenated per delta -- a 64k-token answer
  * arrives as thousands of deltas, and repeated string concatenation there is quadratic.
  */
private[claude] case class ClaudeStreamBlock(
  start: JsObject,
  text: Vector[String] = Vector.empty,
  thinking: Vector[String] = Vector.empty,
  signature: Vector[String] = Vector.empty,
  partialJson: Vector[String] = Vector.empty
) {

  def delta(d: JsObject): ClaudeStreamBlock = {
    def str(field: String): String = (d \ field).asOpt[String].getOrElse("")
    (d \ "type").asOpt[String] match {
      case Some("text_delta") => copy(text = text :+ str("text"))
      case Some("thinking_delta") => copy(thinking = thinking :+ str("thinking"))
      case Some("signature_delta") => copy(signature = signature :+ str("signature"))
      case Some("input_json_delta") => copy(partialJson = partialJson :+ str("partial_json"))
      // A delta type this client does not model leaves the block as the API already described it in
      // content_block_start rather than failing the whole response over a field nothing here reads.
      case _ => this
    }
  }

  /** The block as the non-streaming endpoint would have returned it. */
  def toJson: Either[String, JsObject] = {
    val appended = Seq(
      "text" -> text,
      "thinking" -> thinking,
      "signature" -> signature
    ).foldLeft(start) { case (obj, (field, chunks)) =>
      if (chunks.isEmpty) obj
      else obj + (field -> JsString((obj \ field).asOpt[String].getOrElse("") + chunks.mkString))
    }
    partialJson.mkString.trim match {
      case "" => Right(appended)
      case json =>
        Try(Json.parse(json)) match {
          case Success(o: JsObject) => Right(appended + ("input" -> o))
          case Success(other) => Left(s"tool_use input is not a JSON object: ${Json.stringify(other)}")
          // Reached only when the turn was cut off mid-tool-call (stop_reason=max_tokens). Failing here is
          // deliberate: the alternative is handing the caller's tool an empty or half-parsed argument object
          // and letting it act on it.
          case Failure(e) => Left(s"tool_use input is not valid JSON (truncated tool call?): ${e.getMessage}")
        }
    }
  }
}

/** Rebuilds the JSON body a non-streaming `POST /v1/messages` would have returned from the SSE event sequence a
  * streaming one emits, then validates it with the generated [[ClaudeResponse]] reader.
  *
  * Reassembling the wire JSON -- rather than constructing the case class field by field -- is what keeps this honest:
  * there is exactly one definition of what a response looks like (the apibuilder spec), and a field added to it starts
  * flowing through here with no change. It also means a block type this client has never heard of survives the round
  * trip intact instead of being dropped.
  *
  * Instances are immutable and folded over the stream, so the accumulator can be unit-tested against a captured event
  * sequence with no HTTP involved.
  */
private[claude] case class ClaudeStreamAccumulator(
  message: Option[JsObject] = None,
  blocks: Map[Int, ClaudeStreamBlock] = Map.empty,
  stopReason: Option[JsValue] = None,
  stopSequence: Option[JsValue] = None,
  usage: JsObject = Json.obj(),
  failure: Option[ClaudeStreamOutcome.Failed] = None
) {

  /** Feeds one raw line of the SSE body. Only `data:` lines carry anything: every Anthropic event repeats its own name
    * inside the payload as `type`, which makes the `event:` line redundant, and blank lines and `:` comments are
    * framing. A line that arrives after a failure is ignored so the FIRST failure is the one reported.
    */
  def line(raw: String): ClaudeStreamAccumulator = {
    val trimmed = raw.stripSuffix("\r").trim
    if (failure.isDefined || !trimmed.startsWith("data:")) this
    else
      Try(Json.parse(trimmed.drop("data:".length).trim)) match {
        case Success(js: JsObject) => event(js)
        case Success(other) => failed(s"SSE data line is not a JSON object: ${Json.stringify(other)}")
        case Failure(e) => failed(s"SSE data line is not valid JSON: ${e.getMessage}")
      }
  }

  private def failed(message: String): ClaudeStreamAccumulator =
    copy(failure = Some(ClaudeStreamOutcome.Failed(None, message)))

  private def event(js: JsObject): ClaudeStreamAccumulator = {
    def index: Option[Int] = (js \ "index").asOpt[Int]
    (js \ "type").asOpt[String] match {
      case Some("message_start") =>
        (js \ "message").asOpt[JsObject] match {
          case Some(m) => copy(message = Some(m), usage = (m \ "usage").asOpt[JsObject].getOrElse(Json.obj()))
          case None => failed("message_start carried no message")
        }

      case Some("content_block_start") =>
        (index, (js \ "content_block").asOpt[JsObject]) match {
          case (Some(i), Some(block)) => copy(blocks = blocks + (i -> ClaudeStreamBlock(block)))
          case _ => failed("content_block_start is missing index or content_block")
        }

      case Some("content_block_delta") =>
        (index, (js \ "delta").asOpt[JsObject]) match {
          case (Some(i), Some(d)) =>
            blocks.get(i) match {
              case Some(b) => copy(blocks = blocks + (i -> b.delta(d)))
              case None => failed(s"content_block_delta for index $i with no preceding content_block_start")
            }
          case _ => failed("content_block_delta is missing index or delta")
        }

      case Some("message_delta") =>
        val delta = (js \ "delta").asOpt[JsObject].getOrElse(Json.obj())
        copy(
          // `++` is a shallow right-biased merge: message_delta reports the FINAL usage for the turn, so its
          // fields replace the running counts message_start opened with.
          usage = usage ++ (js \ "usage").asOpt[JsObject].getOrElse(Json.obj()),
          stopReason = (delta \ "stop_reason").asOpt[JsValue].orElse(stopReason),
          stopSequence = (delta \ "stop_sequence").asOpt[JsValue].orElse(stopSequence)
        )

      case Some("error") =>
        val err = (js \ "error").asOpt[JsObject].getOrElse(Json.obj())
        val kind = (err \ "type").asOpt[String].getOrElse("error")
        val detail = (err \ "message").asOpt[String].getOrElse(Json.stringify(err))
        copy(failure = Some(ClaudeStreamOutcome.Failed(Some(ClaudeStream.statusFor(kind)), s"$kind: $detail")))

      // content_block_stop / message_stop / ping carry nothing this accumulator needs, and an event type added to
      // the protocol later is ignored rather than fatal.
      case _ => this
    }
  }

  /** The finished response, or why there isn't one. */
  def outcome: ClaudeStreamOutcome = {
    def fail(message: String) = ClaudeStreamOutcome.Failed(None, message)
    failure.getOrElse {
      (message, stopReason.filterNot(_ == JsNull)) match {
        case (None, _) => fail("stream ended without a message_start event")
        // Every terminated turn reports its stop_reason on message_delta, so its absence means the connection
        // dropped mid-generation. Surfaced as a truncation rather than parsed into a half-response, because a
        // half-response would look to the caller exactly like a short answer.
        case (_, None) => fail("stream ended without a message_delta carrying stop_reason (truncated?)")
        case (Some(m), Some(reason)) =>
          blocks.toSeq
            .sortBy(_._1)
            .map(_._2.toJson)
            .foldLeft[Either[String, Vector[JsObject]]](Right(Vector.empty)) { (acc, b) =>
              acc.flatMap(all => b.map(all :+ _))
            }
            .flatMap { content =>
              val js = m ++ Json.obj("content" -> JsArray(content), "stop_reason" -> reason, "usage" -> usage) ++
                stopSequence.filterNot(_ == JsNull).fold(Json.obj())(s => Json.obj("stop_sequence" -> s))
              js.validate[ClaudeResponse] match {
                case JsSuccess(r, _) => Right(r)
                case JsError(errors) =>
                  Left(
                    "assembled stream is not a valid response: " +
                      errors.map { case (path, msgs) => s"$path: ${msgs.map(_.message).mkString(", ")}" }.mkString(", ")
                  )
              }
            }
            .fold(fail, ClaudeStreamOutcome.Completed.apply)
      }
    }
  }
}

private[claude] object ClaudeStream {

  val Empty: ClaudeStreamAccumulator = ClaudeStreamAccumulator()

  /** Maps an SSE `error` event's type onto the HTTP status the same condition carries when it happens before the stream
    * opens, so one condition produces one error shape however far into the request it surfaces -- an `overloaded_error`
    * at token 5,000 falls back to the next model exactly like a 529 at connect time.
    */
  def statusFor(kind: String): Int = kind match {
    case "overloaded_error" => 529
    case "rate_limit_error" => 429
    case "api_error" | "timeout_error" => 500
    case "authentication_error" => 401
    case "permission_error" => 403
    case "not_found_error" => 404
    case _ => 400
  }
}
