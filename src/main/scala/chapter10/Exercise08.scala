package chapter10

import java.util.concurrent.Executors

object Exercise08 extends App {
  import Monoid._
  import chapter7.nonblocking.Par

  val es = Executors.newFixedThreadPool(2)
  val p = parFoldMap(IndexedSeq(5, 2, 3, 9), intAddition)(identity)
  println(Par.run(Executors.newFixedThreadPool(2))(p))

  val p2 =
    parFoldMapFromAnswerKey(IndexedSeq(5, 2, 3, 9), intAddition)(identity)
  println(Par.run(Executors.newFixedThreadPool(2))(p2))
}
