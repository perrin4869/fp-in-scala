package chapter2

object Exercise5 extends App {
  def compose[A, B, C](f: A => B, g: B => C): A => C = (a) => g(f(a))
  def add10(x: Int) = x + 10
  def multiplyBy2(x: Int) = x * 2

  println(compose(add10, multiplyBy2)(5))

  val composed = add10 _ andThen multiplyBy2 _
  println(composed(5))
}
