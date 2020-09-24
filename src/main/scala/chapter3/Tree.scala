package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(value)         => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value)         => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = {
    def go[A](t: Tree[A], d: Int): Int = {
      t match {
        case Leaf(value)         => d + 1
        case Branch(left, right) => go(left, d + 1) max go(right, d + 1)
      }
    }
    go(t, 0)
  }

  def depthFromAnswersKey[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) =>
      1 + (depthFromAnswersKey(left) max depthFromAnswersKey(right))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(value)         => f(value)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)((_) => 1)(_ + _ + 1)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)((_) => 0)((l, r) => 1 + (l max r))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(v => Leaf(f(v)))((l, r) => Branch(l, r))
}
