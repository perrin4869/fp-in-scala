package chapter6

object Exercise08 extends App {
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    })
}
