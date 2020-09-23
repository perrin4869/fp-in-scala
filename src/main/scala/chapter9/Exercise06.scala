package parser

object Exercise06 extends App {

  def parser[Parser[+_]](P: Parsers[Parser]): Parser[List[Char]] = {
    import P._

    "[0-9]+".r map (_.toInt) flatMap (char('a') listOfN (_))
  }

  // or from answers key
  def parserFromAnswersKey[Parser[+_]](
      P: Parsers[Parser]
  ): Parser[Int] = {
    import P._

    for {
      digit <- "[0-9]+".r
      val n = digit.toInt // we really should catch exceptions thrown by toInt and convert to parse failure
      _ <- listOfN(n, char('a'))
    } yield n
  }
}
