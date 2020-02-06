package chapter2

object Exercise4 extends App {
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
  def add(x: Int)(y: Int) = x + y

  println(uncurry(add)(1, 2))
}
