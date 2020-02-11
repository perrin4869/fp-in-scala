package chapter7
import parallelism_deadlock._

object Exercise02 extends App {
  case class Par[A](f: () => A)

  object Par {
    def cons[A](a: => A): Par[A] = {
      lazy val aa = a
      Par(() => aa)
    }
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
  }
}
