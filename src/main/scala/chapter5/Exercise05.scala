package chapter5

object Exercise05 extends App {
  println(Stream(1, 2, 3, 4, 5).takeWhile(_ < 4))
  println(Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList)
}
