package chapter8

import chapter6.{State, RNG, nonNegativeInt}
import chapter6.SimpleRNG

object Exercise04 extends App {
  def chooseMine(start: Int, stopExclusive: Int): Gen[Int] = {
    // rng.nextInt -> rng.nonNegativeInt
    val sample: State[RNG, Int] = State((rng: RNG) => rng.nextInt).map(n =>
      (n % stopExclusive - start) + start
    )
    Gen[Int](sample)
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  val gen = choose(5, 10)
  println(gen.sample.run(SimpleRNG(47)))
}
