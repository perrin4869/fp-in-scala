package chapter5

object Exercise11 extends App {
  println(Stream.unfold(0)(i => Some((i + 1, i + 1))).take(10).toList)
}
