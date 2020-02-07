package chapter5

object Exercise13 extends App {
  def map[A, B](s: Stream[A], f: A => B): Stream[B] =
    Stream.unfold(s)({
      case Empty      => None
      case Cons(h, t) => Some((f(h()), t()))
    })

  println(map(Stream(1, 2, 3), (x: Int) => x + 1).toList)

  def take[A](s: Stream[A], n: Int): Stream[A] =
    Stream.unfold((s, n))({
      case (Cons(h, t), 1) => Some((h(), (Empty, 0)))
      case (Cons(h, t), n) => Some((h(), (t(), n - 1)))
      case _               => None
    })

  println(take(Stream(1, 2, 3, 4, 5, 6), 4).toList)
  println(take(Stream(1, 2, 3, 4, 5, 6), 20).toList)

  def takeWhile[A](s: Stream[A], f: A => Boolean): Stream[A] =
    Stream.unfold(s)({
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _                    => None
    })

  println(takeWhile(Stream(1, 2, 3, 4, 5, 6), (x: Int) => x < 5).toList)
  println(takeWhile(Stream(1, 2, 3, 4, 5, 6), (x: Int) => x < 20).toList)

  println(Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ + _).toList)
  println(Stream(1, 2, 3).zip(Stream(4, 5, 6)).toList)
  println(Stream(1, 2, 3).zipAll(Stream(4, 5, 6, 7)).toList)
}
