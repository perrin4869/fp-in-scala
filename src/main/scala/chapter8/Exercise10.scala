package chapter8

import chapter6.SimpleRNG
import Exercise04.choose

object Exercise10 extends App {
  val gen1 = choose(5, 12)
  println(gen1.unsized.forSize(10).sample.run(SimpleRNG(10)))
}
