package chapter5

import Stream._

object Exercise10 extends App {
  def fibs: Stream[Int] = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  println(fibs.take(10).toList)
}
