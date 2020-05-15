package chapter8

import Exercise04.choose
import Exercise05.listOfN
import Exercise12.listOf
import Prop._

object Exercise13 extends App {
  val smallInt = choose(-10, 10);
  val maxProp = forAll(listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  run(maxProp) // fails on the empty list

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n max 1, g))
  val maxProp1 = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  run(maxProp1) // fails on the empty list
}
