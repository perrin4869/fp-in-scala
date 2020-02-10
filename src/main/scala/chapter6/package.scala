package object chapter6 {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match {
      case (Int.MinValue, nextRng) => nextRng.nextInt
      case (n, nextRng)            => (math.abs(n), nextRng)
    }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), nextRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, newRng) = rng.nextInt
    val (d, finalRng) = double(newRng)

    ((n, d), finalRng)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), r) = intDouble(rng)

    ((d, n), r)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)

    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(l: List[Int], n: Int, r: RNG): (List[Int], RNG) =
      if (n == 0)
        (l, r)
      else {
        val (i, r1) = r.nextInt
        loop(i +: l, n - 1, r1)
      }

    loop(Nil, count, rng)
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)

      (f(a, b), rng3)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }
}
