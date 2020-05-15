package chapter8

import chapter6.SimpleRNG
import Exercise04.choose
import Exercise05.listOfN

object Exercise12 extends App {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n, g))

  println(listOf(choose(0, 1000)).forSize(5).sample.run(SimpleRNG(12)))
  println(listOf(choose(0, 1000)).forSize(10).sample.run(SimpleRNG(25)))
}
