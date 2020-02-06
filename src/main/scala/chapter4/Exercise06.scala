package chapter4

object Exercise06 extends App {
  println(Right(4).map(_ + 1))
  println((Left("ee"): Either[String, Int]).map(_ + 1))
  // println(Right(4).flatMap(Right((x: Int) => x + 1)))
  println(Right(4).flatMap(x => Right(x + 1)))
  println(Right(4).flatMap(Left(_)))
  println((Left("ee"): Either[String, Int]).flatMap(x => Right(x + 1)))
  println(Right(4).map2(Right(5))(_ + _))
  println(Right(4).map2((Left("ee"): Either[String, Int]))(_ + _))
}
