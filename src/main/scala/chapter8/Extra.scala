package chapter8

import Exercise04.choose
import Exercise05.listOfN

// These are methods that are used later in the book (chapter 9+) but not mentioned in this chapter
object TestingExtra {
  implicit class SGenExtra[A](sgen: SGen[A]) {
    // Mentioned and used first in chapter 9
    def **[B](s2: SGen[B]): SGen[(A, B)] =
      SGen(n => sgen.apply(n) ** s2(n))
  }

  // Mentioned and used first in chapter 9
  /* Not the most efficient implementation, but it's simple.
   * This generates ASCII strings.
   */
  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  // Mentioned and used first in chapter 9
  val string: SGen[String] = SGen(stringN)
}
