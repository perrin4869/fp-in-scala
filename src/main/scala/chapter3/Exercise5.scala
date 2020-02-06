package chapter3

import List.dropWhile

object Exercise5 extends App {
  println(dropWhile(List(1, 2, 3, 4), (num: Int) => num != 4))
}
