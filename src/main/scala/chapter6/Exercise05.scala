package chapter6

object Exercise05 extends App {
  def double: Rand[Double] =
    map(nonNegativeInt)(_ / Int.MaxValue.toDouble + 1)

  println(double(SimpleRNG(42)))
}
