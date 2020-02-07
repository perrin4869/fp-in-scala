package chapter5

object Exercise09 extends App {
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  println(from(5).take(10).toList)
}
