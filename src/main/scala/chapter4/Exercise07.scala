package chapter4

import Either._

object Exercise07 extends App {
  println(traverse(List("35", "89", "22"))(x => Try(x.toInt)))
  println(traverse(List("35", "TT", "22"))(x => Try(x.toInt)))
  println(sequence(List(Right(1), Right(2), Right(3))))
  println(sequence(List(Right(1), Left(2), Right(3))))
}
