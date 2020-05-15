package chapter8

import chapter6.SimpleRNG
import chapter6.{State, double}

object Exercise08 extends App {
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(double)).flatMap(d => if (d < threshold) g1._1 else g2._1)
  }

  val gen1 = Exercise04.choose(5, 12)
  val gen2 = Exercise04.choose(45, 62)

  val rng = SimpleRNG(10)
  println(weighted((gen1, 1000000), (gen2, 1)).sample.run(rng))

  val rng2 = SimpleRNG(12)
  println(weighted((gen1, 1), (gen2, 10000000)).sample.run(rng2))
}
