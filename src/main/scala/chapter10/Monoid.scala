package chapter10

import chapter8.{Gen, Prop}
import scala.math.floor
import chapter7.nonblocking._
import chapter10.Monoid.Stub
import chapter10.Monoid.Part
import chapter3.{Tree, Leaf, Branch}

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

  // Exercise07
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    v.length match {
      case 0 => m.zero
      case 1 => m.op(f(v(0)), m.zero)
      case n => {
        val (v1, v2): (IndexedSeq[A], IndexedSeq[A]) =
          v.splitAt(floor(n / 2).toInt)
        m.op(foldMapV(v1, m)(f), foldMapV(v2, m)(f))
      }
    }

  // Exercise08
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op(_, _))

    def zero: Par[A] = Par.unit(m.zero)
  }
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(
      f: A => B
  ): Par[B] = foldMapV(v, par(m))(f andThen Par.unit)
  // The answere key improves the above by parallelizing the mapping too
  def parFoldMapFromAnswerKey[A, B](v: IndexedSeq[A], m: Monoid[B])(
      f: A => B
  ): Par[B] =
    Par.flatMap(Par.parMap(v)(f))(bs => {
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    })

  // Exercise09, taken from answer key
  def orderedSeq(ints: IndexedSeq[Int]): Boolean = {
    val m = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(
          a1: Option[(Int, Int, Boolean)],
          a2: Option[(Int, Int, Boolean)]
      ): Option[(Int, Int, Boolean)] = (a1, a2) match {
        case (Some((x1, y1, p1)), Some((x2, y2, p2))) =>
          Some(x1 min x2, y1 max y2, p1 && p2 && x2 < y1)
        case (x, None) => x
        case (None, x) => x
      }

      def zero: Option[(Int, Int, Boolean)] = None

    }
    foldMapV(ints, m)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise10
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => {
        val s = if (r1 != "" || l2 != "") 1 else 0 // little bit more elegant in answer key

        Part(l1, w1 + w2 + s, r2)
      }
      case (Stub(s), Part(l, w, r)) => Part(s + l, w, r)
      case (Part(l, w, r), Stub(s)) => Part(l, w, r + s)
      case (Stub(s1), Stub(s2))     => Stub(s1 + s2)
    }

    def zero: WC = Stub("")

  }

  // Exercise11
  def wc(str: String): Int =
    foldMapV(str.toIndexedSeq, wcMonoid)({
      case s if s.isWhitespace => Part("", 0, "")
      case s                   => Stub(s.toString)
    }) match {
      case Stub(chars) => if (chars.isEmpty()) 0 else 1
      case Part(lStub, words, rStub) =>
        words + (if (lStub.isEmpty()) 0 else 1) + (if (rStub.isEmpty()) 0
                                                   else 1)
    }

  // Exercise12
  val foldableList = new Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
      as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))

  }

  // Exercise12
  val foldableIndexedSeq = new Foldable[IndexedSeq] {
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      foldMapV(as, mb)(f)

  }

  // Exercise12
  val foldableStream = new Foldable[Stream] {
    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
      as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))

  }

  // Exercise13
  object TreeFoldable extends Foldable[Tree] {
    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(value) => f(value, z)
        case Branch(left, right) =>
          foldRight(left)(foldRight(right)(z)(f))(f)
      }

    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(value) => f(z, value)
        case Branch(left, right) =>
          foldLeft(right)(foldLeft(left)(z)(f))(f)
      }

    def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case Leaf(value) => f(value)
        case Branch(left, right) =>
          mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      }

  }

  // Exercise14
  val foldableOption = new Foldable[Option] {
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)

    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))

  }

  // Exercise 16
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(a1: (A, B), a2: (A, B)): (A, B) =
        (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

      def zero: (A, B) = (A.zero, B.zero)

    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def op(a1: Map[K, V], a2: Map[K, V]) =
        (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
        }

      def zero = Map[K, V]()

    }

  // Exercise17
  def functionMonoid[A, B](B: Monoid[B]) = new Monoid[A => B] {
    def op(a1: A => B, a2: A => B) = (a) => B.op(a1(a), a2(a))

    def zero = (_) => B.zero

  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV[A, Map[A, Int]](as, mapMergeMonoid(intAddition))((a => Map((a, 1)))
    )
}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // Exercise15
  def toList[A](fa: F[A]): List[A] =
    foldMap(fa)(List(_))(Monoid.listMonoid)
  // From Answers Key:
  // foldRight(fa)(Nil: List[A])(_ :: _)
}
