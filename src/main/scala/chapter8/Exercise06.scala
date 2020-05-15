package chapter8

import chapter6.{SimpleRNG}

object Exercise06 extends App {
  val rng = SimpleRNG(12)

  val genInt = Exercise04.choose(5, 12)
  println(genInt.sample.run(rng))

  println(genInt.listOfN(genInt).sample.run(SimpleRNG(23)))
  println(genInt.listOfN(genInt).sample.run(SimpleRNG(52)))
}
