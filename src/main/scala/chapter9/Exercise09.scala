package parser

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}

object Exercise09 {
  import JSON._

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    val space: Parser[String] = "\\s".r
    val spaces: Parser[List[String]] = many(space)
    // val spaces: Parser[String] = "\\s*".r
    val comma: Parser[String] = ","
    val colon: Parser[String] = ":"

    implicit class JSONParserOps[A](pa: Parser[A]) {
      def *>[B](pb: Parser[B]): Parser[B] = pa ** pb map (_._2)
      def <*[B](pb: Parser[B]): Parser[A] = pa ** pb map (_._1)
      def surround[B, C](pb: Parser[B], pc: Parser[C]): Parser[A] =
        pb *> pa <* pc
      def sep[B](pb: Parser[B]): Parser[List[A]] =
        map2(pa, many(pb *> pa))(_ :: _) or succeed(List())
    }

    def jnull: Parser[JSON] = string("null") map (_ => JNull)
    def num: Parser[JNumber] = "[0-9]+".r map (n => JNumber(n.toInt))
    def bool: Parser[JBool] =
      (string("true") map (_ => JBool(true))) or
        (string("false") map (_ => JBool(false)))

    // allows for escaped quotes
    def jstring: Parser[JString] = "([\\\"'])(?:\\\\\\1|.)*?\\1".r map (
        s => JString(s drop 1 dropRight 1)
    )

    // succeed(JArray(IndexedSeq(JNumber(2))))
    def array: Parser[JArray] =
      ((jval) surround (spaces, spaces)) sep (comma) surround ("[", "]") map (
          vs => JArray(vs.toIndexedSeq)
      )

    def keyVal: Parser[(String, JSON)] =
      jstring.map(_.get) ** ((colon surround (spaces, spaces)) *> jval)
    def obj: Parser[JObject] =
      (keyVal surround (spaces, spaces)) sep (comma) surround ("{", "}") map (
          kvs => JObject(kvs.toMap)
      )

    def primitives = jnull or jstring or num or bool

    def jval: Parser[JSON] = primitives or array or obj

    jval surround (spaces, spaces)
  }
}
