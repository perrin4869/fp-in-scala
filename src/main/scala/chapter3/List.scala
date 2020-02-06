package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1
    case Cons(0.0, _) => 0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil              => Nil
      case Cons(head, tail) => tail
    }

  def setHead[A](a: A, as: List[A]): List[A] =
    as match {
      case Nil              => Nil
      case Cons(head, tail) => Cons(a, tail)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0)
      l
    else
      drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil              => Nil
      case Cons(head, tail) => if (f(head)) dropWhile(tail, f) else l
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] = {
    def loop(res: List[A], tail: List[A]): List[A] =
      tail match {
        case Nil              => Nil
        case Cons(head, Nil)  => res
        case Cons(head, tail) => loop(append(res, List(head)), tail)
      }

    loop(Nil, l)
  }
}
