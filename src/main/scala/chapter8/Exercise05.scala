package chapter8

import chapter6.{State, RNG, SimpleRNG, int}

object Exercise05 extends App {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean(): Gen[Boolean] =
    Gen(State(int).map(_ % 2 == 0))
  // (rng: RNG) => rng.nextInt
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  println(unit(6).sample.run(SimpleRNG(10)))
  println(boolean().sample.run(SimpleRNG(10)))
  println(boolean().sample.run(SimpleRNG(12)))
  println(listOfN(5, boolean).sample.run(SimpleRNG(12)))
  println(listOfN(5, boolean).sample.run(SimpleRNG(43)))
}
