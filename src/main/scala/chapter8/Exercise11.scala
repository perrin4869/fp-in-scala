package chapter8

import chapter6.SimpleRNG
import Exercise04.choose
import Exercise05.{listOfN, unit}

object Exercise11 extends App {
  val sgen = SGen(n => choose(0, n))
  println(sgen.forSize(100).sample.run(SimpleRNG(25)))
  println(sgen.forSize(1000).sample.run(SimpleRNG(25)))

  val sgen2 = sgen.flatMap(n => SGen(size => listOfN(size, unit(n))))
  println(sgen2.forSize(10).sample.run(SimpleRNG(12)))
  println(sgen2.forSize(100).sample.run(SimpleRNG(25))) // bigger numbers and longer lists
}
