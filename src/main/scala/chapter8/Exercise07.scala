package chapter8

import chapter6.SimpleRNG

object Exercise07 extends App {
  def union[A](gen1: Gen[A], gen2: Gen[A]): Gen[A] =
    Exercise05.boolean flatMap (
      if (_) gen1 else gen2
    )

  val gen1 = Exercise04.choose(5, 12)
  val gen2 = Exercise04.choose(45, 62)

  val rng = SimpleRNG(10)
  println(union(gen1, gen2).sample.run(rng))

  val rng2 = SimpleRNG(12)
  println(union(gen1, gen2).sample.run(rng2))
}
