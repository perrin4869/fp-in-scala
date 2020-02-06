package chapter4

import Option._

object Exercise05 extends App {
  println(traverse(List("32", "53", "97"))(i => Try(i.toInt)))
  println(traverse(List("32", "TH", "97"))(i => Try(i.toInt)))
}
