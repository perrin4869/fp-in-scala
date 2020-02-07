package chapter5

object Exercise15 extends App {
  println(Stream(1, 2, 3).tails.map(_.toList).toList)
}
