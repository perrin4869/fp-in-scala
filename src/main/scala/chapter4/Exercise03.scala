package chapter4

object Exercise03 extends App {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  println(map2(Some(1), Some(2))(_ + _))
  println(map2(None: Option[Int], Some(2))(_ + _))
}
