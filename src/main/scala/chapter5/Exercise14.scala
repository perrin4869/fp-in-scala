package chapter5

object Exercise14 extends App {
  import Exercise12.{constant, from}
  println(Stream(1, 2, 3).startsWith(Stream(1, 2)))
  println(constant(1).startsWith(constant(1).take(10)))
  println(constant(1).startsWith(constant(2).take(10)))
  println(from(5).startsWith(from(5).take(10)))
  println(from(5).startsWith(from(6).take(10)))
  println(from(5).startsWith(from(4).take(10)))
}
