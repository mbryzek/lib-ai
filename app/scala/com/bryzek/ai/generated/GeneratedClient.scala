package generated.client

sealed trait Auth
object Auth {
  case class Basic(username: String, password: Option[String] = None) extends Auth
}

object PathEncoder {

  /**
   * Percent encodes a single URL path segment. Without this a parameter value containing
   * '/', '?' or '#' silently retargets the request at a different endpoint while still
   * carrying this client's auth headers.
   *
   * URLEncoder implements application/x-www-form-urlencoded, which differs from RFC 3986
   * path encoding in exactly one way that matters here: it emits '+' for a space, which a
   * path segment reads as a literal '+'. Rewriting it to %20 makes the output a valid path
   * segment.
   *
   * Note this does NOT neutralize a segment that is exactly "." or ".." — those characters
   * are legal in a path segment and are left alone. Callers that accept arbitrary user
   * input as a path parameter must reject dot segments themselves.
   */
  def encode(value: String): String = {
    _root_.java.net.URLEncoder.encode(value, _root_.java.nio.charset.StandardCharsets.UTF_8).replace("+", "%20")
  }
}