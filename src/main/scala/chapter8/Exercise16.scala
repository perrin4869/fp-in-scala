package chapter8

import chapter7.parallelism.Par
import Prop._
import java.util.concurrent.Executors
import Exercise04.choose

object Exercise15 extends App {
  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  run(checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  })

  run(check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(Executors.newFixedThreadPool(1)).get
  })

  val pint = choose(0, 10) map (Par.unit(_))
  run(forAllPar(pint)(n => equal(Par.map(n)(y => y), n)))

  val psqrt = choose(1, 100) map (
      n => Par.parMap(List.range(1, n))(math.sqrt(_))
  )
  run(
    forAllPar(psqrt)(n =>
      equal(Par.unit(List.range(1, 100).map(math.sqrt(_))), n)
    )
  )

  // taken from answer key
  val pint2: Gen[Par[Int]] = choose(-100, 100)
    .listOfN(choose(0, 20))
    .map(l =>
      l.foldLeft(Par.unit(0))((p, i) =>
        Par.fork { Par.map2(p, Par.unit(i))(_ + _) }
      )
    )
  // generates a list of length 0-20 with integers -100 to 100 and sums them in parallel
}
