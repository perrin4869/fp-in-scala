package parser

import chapter8.{Gen, SGen, Prop}
import scala.util.matching.Regex
import parser.ParserInstance.Success
import parser.ParserInstance.Failure

trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(
      implicit f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(f andThen succeed)

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  // the definition below only works if map
  // is not implemented in terms of flatMap and succeed
  def succeed[A](a: A): Parser[A] // = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  // Exercise 07
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p flatMap (a => p2 map (b => (a, b)))

  // Exercise 07
  def productViaFor[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p
      b <- p2
    } yield (a, b)

  // Exercise 01
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    product(p, p2).map((t) => f(t._1, t._2))
  // Beatiful
  // product(p, p2).map(f.tupled)

  // Exercise 07
  def map2ViaFlatMap[A, B, C](p: Parser[A], p2: => Parser[B])(
      f: (A, B) => C
  ): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a, b)

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  // Exercise 03
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  // Exercise 04
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n >= 0) map2(p, listOfN(n - 1, p))(_ :: _) else succeed(List())

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // Exercise 08
  def mapViaFlatMap[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p flatMap (a => succeed(f(a)))

  implicit def regex(r: Regex): Parser[String]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many[B >: A]: Parser[List[B]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice[B]: Parser[String] = self.slice(p)
    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    import chapter8.TestingExtra.SGenExtra
    import chapter8.TestingExtra

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(succeed(a))(s) == Right(a))

    // Exercise 02
    def productLaw[A, B](a: A, b: B)(in: Gen[String]): Prop =
      Prop.forAll(in)(s => run(succeed(a) ** succeed(b))(s) == (a, b))
    // from the answer key: a ** (b ** c) map (unbiasL) == (a ** b) ** c map (unbiasR)
    // or: a ** (b ** c) ~= (a ** b) ** c
    def productAssociativeLaw[A, B, C](
        pa: Parser[A],
        pb: Parser[B],
        pc: Parser[C]
    )(in: Gen[String]): Prop =
      equal((pa ** pb) ** pc map (unbiasL), pa ** (pb ** pc) map (unbiasR))(in)

    def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
    def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) =
      (p._1, p._2._1, p._2._2)

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      Prop.forAll(inputs ** TestingExtra.string) {
        case (input, msg) =>
          run(label(msg)(p))(input) match {
            case Left(e) => e.latest.get._2 == msg
            case _       => true
          }
      }
  }

  def errorLocation(e: ParseError): Location = e.latest.get._1
  def errorMessage(e: ParseError): String = e.latest.get._2
}

case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest.map(_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1        => offset + 1
    case lineStart => offset - lineStart
  }
  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))
  def advanceBy(n: Int): Location =
    copy(offset = offset + n)
}

object ParserInstance {
  type Parser[+A] = Location => Result[A]

  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, isCommitted) => Failure(f(e), isCommitted)
      case _                       => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _                => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _             => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _             => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean)
      extends Result[Nothing]

  /** Returns -1 if s1.startsWith(s2), otherwise returns the
    * first index where the two strings differed. If s2 is
    * longer than s1, returns s1.length. */
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i + offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length - offset >= s2.length) -1
    else s1.length - offset
  }
}

object MyParsers extends Parsers[ParserInstance.Parser] {
  import ParserInstance._

  def string(s: String): Parser[String] = {
    val msg = "'" + s + "'"
    l => {
      val i = firstNonmatchingIndex(l.input, s, l.offset)
      if (i == -1) // they matched
        Success(s, s.length)
      else
        Failure(l.advanceBy(i).toError(msg), i != 0)
    }
  }

  def succeed[A](a: A): Parser[A] =
    l => Success(a, 0)

  def attempt[A](p: Parser[A]): Parser[A] = s => p(s).uncommit

  def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B] =
    l =>
      pa(l) match {
        case Success(a, n) =>
          f(a)(l.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
        case e @ Failure(_, _) => e
      }

  def scope[A](str: String)(p: Parser[A]): Parser[A] =
    l => p(l).mapError(_.push(l, str))

  def label[A](msg: String)(pa: Parser[A]): Parser[A] =
    l => pa(l).mapError(_.label(msg))

  def regex(r: Regex): Parser[String] = {
    val msg = "regex " + r
    l =>
      r.findPrefixOf(l.input.substring(l.offset)) match {
        case None    => Failure(l.toError(msg), false)
        case Some(m) => Success(m, m.length)
      }
  }

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =
    l =>
      s1(l) match {
        case Failure(e, false) => s2(l)
        case r                 => r
      }

  def slice[A](p: Parser[A]): Parser[String] =
    l =>
      p(l) match {
        case Success(_, consumed) =>
          Success(
            l.input.substring(l.offset, consumed),
            consumed
          )
        case f @ Failure(_, _) => f
      }

  def run[A](p: Parser[A])(str: String): Either[ParseError, A] =
    p(Location(str, 0)) match {
      case Success(get, _) => Right(get)
      case Failure(get, _) => Left(get)
    }
}
