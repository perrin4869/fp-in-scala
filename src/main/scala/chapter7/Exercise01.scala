package chapter7
import parallelism_deadlock._

object Exercise01 extends App {
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
}
