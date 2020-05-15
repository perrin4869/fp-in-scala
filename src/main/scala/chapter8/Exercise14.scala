package chapter8

import Exercise04.choose
import Exercise05.listOfN
import Exercise12.listOf
import Prop._

object Exercise14 extends App {
  val smallInt = choose(-10, 10);
  val sortedProp = forAll(listOf(smallInt)) { ns =>
    val nss = ns.sorted

    // taken from answer key, couldn't figure out nss.zip(nss.tail) (smart!!!)
    (ns.isEmpty || ns.tail.isEmpty || !nss
      .zip(nss.tail)
      .exists({
        case (a, b) => a > b
      })) && !ns.exists(!nss.contains(_)) && !nss.exists(!ns.contains(_))
  }

  run(sortedProp)
}
