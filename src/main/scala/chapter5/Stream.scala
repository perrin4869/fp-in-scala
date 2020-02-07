package chapter5

sealed trait Stream[+A] {
  import Stream._

  def toList: List[A] =
    this match {
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def take(n: Int): Stream[A] =
    this match {
      case _ if n == 0 => empty
      case Empty       => empty
      case Cons(h, t)  => cons(h(), t() take (n - 1))
    }

  // Stole the annotation from the solutions
  @annotation.tailrec()
  final def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n != 0 => t() drop (n - 1)
      case _                    => this
    }

  def headOption: Option[A] =
    this.foldRight[Option[A]](None)((a, z) => Some(a))

  def headOption_1: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  def takeWhile(f: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](empty)((a, z) => if (f(a)) cons(a, z) else empty)

  def takeWhile_1(f: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if (f(h())) => cons(h(), t() takeWhile (f))
      case _                      => empty
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  def exists(p: A => Boolean): Boolean =
    this.foldRight(false)((a, z) => p(a) || z)

  def forAll(f: A => Boolean): Boolean =
    this.foldRight(true)((a, z) => f(a) && z)

  def map[B](f: A => B): Stream[B] =
    this.foldRight[Stream[B]](empty)((a, s) => cons(f(a), s))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](empty)((a, s) => if (f(a)) cons(a, s) else s)

  // def append[B >: A](b: => Stream[B]): Stream[B] =
  //   this.foldRight[Stream[B]](empty)((a, s) => cons(a, if (s == Empty) b else s)
  //   )

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)((a, s) => cons(a, s))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, z) => f(a) append z)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _                            => None
    }

  def zipAllWith[B, C](
      s: Stream[B]
  )(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(Some(h1()), Some(h2())), (t1(), t2())))
      case (Empty, Cons(h2, t2)) => Some((f(None, Some(h2())), (empty, t2())))
      case (Cons(h1, t1), Empty) => Some((f(Some(h1()), None), (t1(), empty)))
      case _                     => None
    }

  def zip[B](s: Stream[B]): Stream[(A, B)] =
    zipWith(s)((_, _))

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    zipAllWith(s)((_, _))

  def startsWith[B >: A](s: Stream[B]): Boolean =
    zipAll(s) takeWhile (!_._2.isEmpty) forAll ({
      case (h, h2) => h == h2
    })

  // original solution without drop
  // def tails: Stream[Stream[A]] =
  //   unfold(this)({
  //     case Empty      => None
  //     case Cons(h, t) => Some((Cons(h, t), t()))
  //   }) append Stream(empty)

  def tails: Stream[Stream[A]] =
    unfold(this)({
      case Empty => None
      case s     => Some((s, s drop 1))
    }) append Stream(empty)

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_.startsWith(s))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map({ case (a, s) => cons(a, unfold(s)(f)) }).getOrElse(empty)
  }
}
