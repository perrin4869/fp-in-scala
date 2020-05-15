package chapter8

import chapter6.State

case class SGen[+A](forSize: Int => Gen[A]) {
  // Exercise 11
  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n) flatMap { f(_).forSize(n) })

  // Exercise 11
  // probably not meant to be converted
  // the answer key doesn't have listOfN either
  def listOfN(size: SGen[Int]): SGen[List[A]] =
    SGen(n => forSize(n).listOfN(size.forSize(n)))

  def apply(n: Int): Gen[A] =
    forSize(n)
}
