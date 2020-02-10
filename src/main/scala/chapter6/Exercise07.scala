package chapter6

object Exercise07 extends App {
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      fs.foldRight[(List[A], RNG)]((Nil, rng))({
        case (rand, (l, r)) => {
          val (a, r1) = rand(r)
          (a +: l, r1)
        }
      })

  def ints = sequence(List.fill(10)(int))
  println(ints(SimpleRNG(42)))
}
