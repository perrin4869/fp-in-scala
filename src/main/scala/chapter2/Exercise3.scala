package chapter2

object Exercise3 extends App {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a) => (b) => f(a, b)
  val add = (x: Int, y: Int) => x + y

  println(curry(add)(4)(5))
}
