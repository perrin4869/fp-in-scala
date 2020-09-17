package chapter10

import chapter8.{Gen, Prop}

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero = Nil
  }

  // Exercise01
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  // Exercise01
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  // Exercise01
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  // Exercise01
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  // Exercise02
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val zero = None
  }

  // Exercise03
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f andThen g
    val zero = (a: A) => a
  }

  // Exercise04
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(gen)(a => m.op(a, m.zero) == a) &&
      Prop.forAll(gen ** gen ** gen)({
        case ((a, b), c) =>
          m.op(m.op(a, b), c) == m.op(a, m.op(b, c))
      })

  def concatnate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Exercise 05
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    // from answerkey: (doesn't require map)
    // as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
    concatnate(as.map(f), m)

  // Exercise06 (with help of the answers key)
  def foldRight[A, B](as: List[A])(i: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(i)
}
