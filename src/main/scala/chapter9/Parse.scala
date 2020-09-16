package parser

import chapter8.{Gen, Prop}
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(
      implicit f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))

  def map[A, B](p: Parser[A])(f: A => B): Parser[B]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def succeed[A](a: A) = string("") map (_ => a)

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

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def many[B >: A]: Parser[List[B]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice[B]: Parser[String] = self.slice(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
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
  }
}
