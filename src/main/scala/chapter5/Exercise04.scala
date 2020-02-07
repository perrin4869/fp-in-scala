package chapter5

object Exercise04 extends App {
  println(Stream(1, 2, 3, 4).forAll(_ < 10))
  println(Stream(1, 2, 3, 4).forAll(_ < 3))
  println(Stream(1, 2, 3, 4).forAll(_ > 0))
  println(Stream(1, 2, 3, 4).forAll(_ > 1))
  println(Stream(1, 2, 3, 4).forAll(_ > 2))
  println(Stream(1, 2, 3, 4).forAll(_ > 3))
  println(Stream(1, 2, 3, 4).forAll(_ > 4))
  println(Stream(1, 2, 3, 4).forAll(_ > 5))
}
