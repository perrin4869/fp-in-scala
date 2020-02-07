package chapter5

object Exercise02 extends App {
  println(Stream(1, 2, 3, 4, 5).take(3))
  println(Stream(1, 2, 3, 4, 5).take(3).toList)

  println(Stream(1, 2, 3, 4, 5).drop(3))
  println(Stream(1, 2, 3, 4, 5).drop(3).toList)
}
