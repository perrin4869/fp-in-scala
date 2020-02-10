package chapter6

object Exercise10 extends App {
  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)
  println(int.run(SimpleRNG(42)))
}
