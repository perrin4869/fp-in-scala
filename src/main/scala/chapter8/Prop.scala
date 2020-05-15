package chapter8

import chapter6.{RNG, SimpleRNG}
import chapter5.Stream

// Access the types defined in Prop (MaxSize, TestCases, etc)
// outside of the body of the Prop object
import Prop._
import java.util.concurrent.Executors
import chapter7.parallelism.Par

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      // case Passed => p.run(max, n, rng)
      case Passed | Proved => p.run(max, n, rng)
      case f               => f
    }
  }

  def ||(p: Prop): Prop = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(_, _) => p.run(max, n, rng)
      case x               => x
      // case Passed => Passed
      // case _      => p.run(max, n, rng)
    }
  }
  // def &&(p: Prop): Prop = Prop { (n, rng) =>
  //   (this.run(n, rng), p.run(n, rng)) match {
  //     case (Passed, Passed) => Passed
  //     case (f, Passed)      => f
  //     case (Passed, f)      => f
  //     case (Falsified(f1, s1), Falsified(f2, s2)) =>
  //       Falsified(f1 + f2, s1 + s2)
  //   }
  // }
  //
  // def ||(p: Prop): Prop = Prop { (n, rng) =>
  //   (this.run(n, rng), p.run(n, rng)) match {
  //     case (Passed, Passed) => Passed
  //     case (f, Passed)      => Passed
  //     case (Passed, f)      => Passed
  //     case (Falsified(f1, s1), Falsified(f2, s2)) =>
  //       Falsified(f1 + f2, s1 + s2)
  //   }
  // }
}

object Prop {
  type MaxSize = Int
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount)
      extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng)
      .zip(chapter5.Exercise12.from(0))
      .take(n)
      .map {
        case (a, i) =>
          try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def apply[A](f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) =>
      f(n, rng)
    }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f) // uses the apply method of SGen

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        chapter5.Exercise12
          .from(0)
          .take((n min max) + 1)
          .map(i => forAll(g(i))(f))
      val prop: Prop =
        props
          .map(p =>
            Prop { (max, _, rng) =>
              p.run(max, casesPerSize, rng)
            }
          )
          .toList
          .reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(
      p: Prop,
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis)
  ): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  val S = Exercise08.weighted(
    Exercise04.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    Exercise05.unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  def checkPar[A](p: Par[Boolean]): Prop =
    forAllPar(Exercise05.unit(()))(_ => p)
}
