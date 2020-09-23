package parser

object Exercise15 extends App {
  val jsonParser = Exercise09.jsonParser(MyParsers)

  println(MyParsers.run(jsonParser)("9"))
  println(MyParsers.run(jsonParser)("'foo'"))
  println(MyParsers.run(jsonParser)("null"))
  println(MyParsers.run(jsonParser)("[]"))
  println(MyParsers.run(jsonParser)("{}"))

  println(MyParsers.run(jsonParser)("null  "))
  println(MyParsers.run(jsonParser)("  null"))

  println(MyParsers.run(jsonParser)("[  ]"))
  println(MyParsers.run(jsonParser)("[10]"))
  println(MyParsers.run(jsonParser)("['foo']"))
  println(MyParsers.run(jsonParser)("[9, 2]"))
  println(MyParsers.run(jsonParser)("{ \"foo\": \"bar\" }"))
  println(MyParsers.run(jsonParser)("{ \"foo\": \"bar\", \"baz\": [4, 15] }"))
}
