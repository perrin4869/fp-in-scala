package chapter8

import Exercise05.unit
import chapter6.{State, RNG}

// the book text does not mark +A
// which is weird because it won't compile otherwise
case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(s => Exercise05.listOfN(s, this))

  def unsized: SGen[A] = SGen { (n) =>
    this
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(this.sample.map2(g.sample)(f))

  def map[B](f: A => B): Gen[B] =
    this flatMap (a => unit(f(a)))

  def **[B](g: Gen[B]): Gen[(A, B)] =
    this.map2(g)((_, _))
}
