package generated.client

sealed trait Auth
object Auth {
  case class Basic(username: String, password: Option[String] = None) extends Auth
}