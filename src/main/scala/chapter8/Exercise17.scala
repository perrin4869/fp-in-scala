package chapter8

import chapter7.parallelism.Par
import chapter7.nonblocking.{Par => NonBlockingPar}
import Prop._
import java.util.concurrent.Executors
import Exercise04.choose
import Exercise15.equal

object Exercise16 extends App {
  val g = choose(-100, 100) map { Par.unit(_) }
  val p = forAllPar(g)(n => equal(n, Par.fork(n)))
  run(p)

  // lets try the non-blocking API
  def forAllParNonBlocking[A](
      g: Gen[A]
  )(f: A => NonBlockingPar[Boolean]): Prop =
    forAll(S ** g) { case s ** a => NonBlockingPar.run(s)(f(a)) } // S is the weighted executor

  def equalNonBlocking[A](
      p: NonBlockingPar[A],
      p2: NonBlockingPar[A]
  ): NonBlockingPar[Boolean] =
    NonBlockingPar.map2(p, p2)(_ == _)

  val g2 = choose(-100, 100) map { NonBlockingPar.unit(_) }
  val p2 =
    forAllParNonBlocking(g2)(n => equalNonBlocking(n, NonBlockingPar.fork(n)))
  run(p2)
}
