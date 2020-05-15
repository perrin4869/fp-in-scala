package chapter8

import chapter6.SimpleRNG
import Exercise04.choose

object Exercise09 extends App {
  val gen1 = choose(5, 12)
  val prop1 = Prop.forAll(gen1)(n => n >= 5)
  val prop2 = Prop.forAll(gen1)(n => n <= 10)

  val prop = prop1 && prop2
  val rng = SimpleRNG(42)
  println(prop.run(1, 10, rng))

  val propOr = prop1 || prop2
  println(propOr.run(1, 10, rng))
}
