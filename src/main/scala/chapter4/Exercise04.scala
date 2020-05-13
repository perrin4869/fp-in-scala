package chapter4

import Option._

object Exercise04 extends App {
  // def sequence[A](a: List[Option[A]]): Option[List[A]] =
  //   a.foldRight(Some(Nil): Option[List[A]])((x, z) =>
  //     x flatMap (xx => z.map(zz => xx :: zz))
  //   )

  // def sequence[A](a: List[Option[A]]): Option[List[A]] =
  //   a.foldRight(Some(Nil): Option[List[A]])((x, z) =>
  //     for {
  //       xx <- x
  //       zz <- z
  //     } yield xx :: zz
  //   )

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, z) => map2(x, z)(_ :: _))

  // foldLeft requires appending to the singly-linked-list, which is more expensive
  // def sequence[A](a: List[Option[A]]): Option[List[A]] =
  //   a.foldLeft[Option[List[A]]](Some(Nil))((z, x) => map2(z, x)(_ :+ _))

  println(sequence(List(Some(1), Some(2), Some(3))))
  println(sequence(List(Some(1), None, Some(3))))
}
