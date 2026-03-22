package generated.mock

import org.apache.pekko.stream.scaladsl.Source
import org.apache.pekko.util.ByteString
import play.api.libs.json.{JsValue, Json, JsNull, Writes}
import play.api.libs.ws.{WSBodyReadables, WSCookie, WSResponse}
import java.net.URI
import scala.xml.Elem

case class MockWSResponse(
  override val uri: URI,
  override val status: Int = 200,
  override val body: String = "",
  override val headers: Map[String, Seq[String]] = Map.empty,
  cookieSeq: Seq[WSCookie] = Seq.empty,
  underlyingResponse: Any = null
) extends WSResponse with WSBodyReadables {

  override val statusText: String = MockWSResponse.statusTextFor(status)

  override def underlying[T]: T = underlyingResponse.asInstanceOf[T]

  override def cookies: Seq[WSCookie] = cookieSeq

  override def cookie(name: String): Option[WSCookie] =
    cookieSeq.find(_.name == name)

  override def bodyAsBytes: ByteString =
    ByteString(body.getBytes("UTF-8"))

  override def bodyAsSource: Source[ByteString, ?] =
    Source.single(bodyAsBytes)

  @deprecated("Use response.headers", "2.6.0")
  override def allHeaders: Map[String, Seq[String]] = headers

  override def xml: Elem = scala.xml.XML.loadString(body)

  override def json: JsValue = Json.parse(body)
}

object MockWSResponse {

  val DefaultURI: URI = new URI("http://mock.localhost/")

  def empty(status: Int = 200, uri: URI = DefaultURI): MockWSResponse =
    MockWSResponse(
      uri = uri,
      status = status,
      body = Json.stringify(JsNull),
      headers = Map("Content-Type" -> Seq("application/json"))
    )

  def json[T](status: Int = 200, body: T, uri: URI = DefaultURI)(implicit writes: Writes[T]): MockWSResponse =
    MockWSResponse(
      uri = uri,
      status = status,
      body = Json.stringify(Json.toJson(body)),
      headers = Map("Content-Type" -> Seq("application/json"))
    )

  def error(status: Int, message: String = "", uri: URI = DefaultURI): MockWSResponse =
    MockWSResponse(
      uri = uri,
      status = status,
      body = message
    )

  private def statusTextFor(status: Int): String = status match {
    case 200 => "OK"
    case 201 => "Created"
    case 204 => "No Content"
    case 400 => "Bad Request"
    case 401 => "Unauthorized"
    case 403 => "Forbidden"
    case 404 => "Not Found"
    case 422 => "Unprocessable Content"
    case 500 => "Internal Server Error"
    case 502 => "Bad Gateway"
    case 503 => "Service Unavailable"
    case _ => "Unknown"
  }
}