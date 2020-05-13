package chapter7

import java.util.concurrent.Executors
import parallelism._

object Exercise08 extends App {
  import Par._

  val a = lazyUnit(42 + 1)
  val S = Executors.newFixedThreadPool(1)
  println(equal(S)(a, fork(a)))
}
