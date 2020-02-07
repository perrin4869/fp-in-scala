package chapter5

object Exercise03 extends App {
  println(Stream(1, 2, 3, 4, 5).takeWhile_1(_ < 4))
  println(Stream(1, 2, 3, 4, 5).takeWhile_1(_ < 4).toList)
}
