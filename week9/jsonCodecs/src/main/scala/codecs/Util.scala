package codecs

import org.typelevel.jawn.{ Parser, SimpleFacade }

// Utility methods that decode values from `String` JSON blobs, and
// render values to `String` JSON blobs
object Util {

  /**
   * Parse a JSON document in a `String` value into a `Json` value, returns
   * `None` in case the supplied `value` is not a valid JSON document.
   * */
  def parseJson(s: String): Option[Json] = Parser.parseFromString[Json](s).toOption

  /**
   * Parse the JSON value from the supplied
   * */
  def parseAndDecode[A](s: String)(implicit decoder: Decoder[A]): Option[A] =
    for {
      json <- parseJson(s)
      a <- decoder.decode(json)
    } yield a


  /**
   * Render the supplied `value` into JSON using the given `encoder`
   * */
  def renderJson[A](value: A)(implicit encoder: Encoder[A]): String =
    render(encoder.encode(value))

  private def render(json: Json): String = json match {
    case Json.Null => "Null"
    case Json.Bool(b) => b.toString
    case Json.Num(n) => n.toString
    case Json.Str(s) => renderString(s)
    case Json.Arr(vs) => vs.map(render).mkString("[", ",", "]")
    case Json.Obj(vs) => vs.map { case (k, v) => s"${renderString(k)}:${render(v)}" }.mkString("{", ",", "}")
  }

  private def renderString(s: String): String = {
    val sb = new StringBuilder
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      s.charAt(i) match {
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c =>
          if (c < ' ') sb.append("\\u%04x" format c.toInt)
          else sb.append(c)
      }
      i += 1
    }
    sb.append('"').toString
  }

  implicit val facade: SimpleFacade[Json] = new SimpleFacade[Json] {
    override def jarray(vs: List[Json]): Json = Json.Arr(vs)

    override def jobject(vs: Map[String, Json]): Json = Json.Obj(vs)

    override def jnull(): Json = Json.Null

    override def jfalse(): Json = Json.Bool(false)

    override def jtrue(): Json = Json.Bool(true)

    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int): Json = Json.Num(BigDecimal(s.toString))

    override def jstring(s: CharSequence): Json = Json.Str(s.toString)
  }
}