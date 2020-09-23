package chapter10

object Exercise07 extends App {
  import Monoid._

  println(foldMapV(IndexedSeq(5, 2, 3, 9), intAddition)(identity))
}
