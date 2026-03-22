package generated.errors

import _root_.play.api.libs.ws.WSResponse
import _root_.play.api.libs.json.{Json, JsValue, JsArray, Reads, JsSuccess, JsError}

case class ApiException(
  response: _root_.play.api.libs.ws.WSResponse,
  message: String
) extends RuntimeException(s"HTTP ${response.status}: $message")

object Util {
  private def validate[T](response: WSResponse, name: String, js: JsValue)(implicit reader: Reads[T]): T = {
    js.validate[T] match {
      case JsSuccess(value, _) => value
      case JsError(errors) => throw ApiException(
        response,
        s"Unable to parse response body as a $name: " + errors.map { case (path, messages) => s"$path: ${messages.mkString(", ")}" }.mkString(", ")
      )
    }
  }

  def mustParse[T](response: WSResponse, name: String)(implicit reader: Reads[T]): T = {
    validate(response, name, Json.parse(response.body))
  }

  def mustParseSeq[T](response: WSResponse, name: String)(implicit reader: Reads[T]): Seq[T] = {
    Json.parse(response.body) match {
      case a: JsArray => a.value.toSeq.map { v =>
        validate(response, name, v)
      }
      case other => throw ApiException(
        response,
        s"Unable to parse response body. Expected a Seq but found a ${other.getClass.getName}"
      )
    }
  }
}