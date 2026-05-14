package generated.binders

trait Bindable[T] {
  def fromString(value: String): T
  def toString(value: T): String
  def example: T
  def validValues: Seq[T] = Nil

  final def errorMessage(key: String, value: String): String = {
    val base = s"Invalid value '$value' for parameter '$key'. "
    validValues.toList match {
      case Nil => base + "Example: " + toString(example)
      case values => base + ". Valid values are: " + values.map(toString).mkString("'", "', '", "'")
    }
  }

  final def bind(key: String, value: String): Either[String, T] = {
    try {
      Right(fromString(value))
    } catch {
      case _: java.lang.Exception => Left(errorMessage(key, value))
    }
  }
}
        case class BaseQueryStringBindable[T](bindable: Bindable[T]) extends _root_.play.api.mvc.QueryStringBindable[T] {
  final override def bind(key: String, params: Map[String, Seq[String]]): _root_.scala.Option[_root_.scala.Either[String, T]] = {
    params.get(key).flatMap(_.headOption).map { v => bindable.bind(key, v) }
  }
  final override def unbind(key: String, value: T): String = s"$key=${bindable.toString(value)}"
}

case class BasePathBindable[T](bindable: Bindable[T]) extends _root_.play.api.mvc.PathBindable[T] {
  final override def bind(key: String, value: String): Either[String, T] = {
    bindable.bind(key, value)
  }
  final override def unbind(key: String, value: T): String = bindable.toString(value)
}

private val localDateBinder = new Bindable[_root_.org.joda.time.LocalDate] {
  override def fromString(value: String): _root_.org.joda.time.LocalDate = _root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseLocalDate(value)
  override def toString(value: _root_.org.joda.time.LocalDate): String = _root_.org.joda.time.format.ISODateTimeFormat.date.print(value)
  override def example: _root_.org.joda.time.LocalDate = _root_.org.joda.time.LocalDate.now
}
implicit def pathBinderDateIso8601(implicit stringBinder: play.api.mvc.QueryStringBindable[String]): play.api.mvc.PathBindable[_root_.org.joda.time.LocalDate] = BasePathBindable(localDateBinder)
implicit def queryStringBinderDateIso8601(implicit stringBinder: play.api.mvc.QueryStringBindable[String]): play.api.mvc.QueryStringBindable[_root_.org.joda.time.LocalDate] = BaseQueryStringBindable(localDateBinder)
private val dateTimeBinder = new Bindable[_root_.org.joda.time.DateTime] {
  override def fromString(value: String): _root_.org.joda.time.DateTime = _root_.org.joda.time.format.ISODateTimeFormat.dateTimeParser.parseDateTime(value)
  override def toString(value: _root_.org.joda.time.DateTime): String = _root_.org.joda.time.format.ISODateTimeFormat.dateTime.print(value)
  override def example: _root_.org.joda.time.DateTime = _root_.org.joda.time.DateTime.now
}
implicit def pathBinderDateTimeIso8601(implicit stringBinder: play.api.mvc.QueryStringBindable[String]): play.api.mvc.PathBindable[_root_.org.joda.time.DateTime] = BasePathBindable(dateTimeBinder)
implicit def queryStringBinderDateTimeIso8601(implicit stringBinder: play.api.mvc.QueryStringBindable[String]): play.api.mvc.QueryStringBindable[_root_.org.joda.time.DateTime] = BaseQueryStringBindable(dateTimeBinder)
private val localTimeBinder = new Bindable[_root_.org.joda.time.LocalTime] {
  override def fromString(value: String): _root_.org.joda.time.LocalTime = _root_.org.joda.time.format.ISODateTimeFormat.timeParser.parseLocalTime(value)
  override def toString(value: _root_.org.joda.time.LocalTime): String = _root_.org.joda.time.format.ISODateTimeFormat.timeNoMillis.print(value)
  override def example: _root_.org.joda.time.LocalTime = _root_.org.joda.time.LocalTime.now
}
implicit def pathBinderTimeIso8601(implicit stringBinder: play.api.mvc.QueryStringBindable[String]): play.api.mvc.PathBindable[_root_.org.joda.time.LocalTime] = BasePathBindable(localTimeBinder)
implicit def queryStringBinderTimeIso8601(implicit stringBinder: play.api.mvc.QueryStringBindable[String]): play.api.mvc.QueryStringBindable[_root_.org.joda.time.LocalTime] = BaseQueryStringBindable(localTimeBinder)