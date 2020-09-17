package chapter10

import chapter8.{Gen, Prop}

object Exercise04 extends App {
  import Monoid._

  val ints: Gen[Int] = chapter8.Exercise04.choose(Int.MinValue, Int.MaxValue);

  Prop.run(
    monoidLaws(
      intAddition,
      ints
    )
  )

  Prop.run(
    monoidLaws(
      intMultiplication,
      ints
    )
  )
}
