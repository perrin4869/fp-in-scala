package chapter4

object Exercise02 extends App {
  def mean(xs: Seq[Double]): Option[Double] = xs match {
    case xs if xs.isEmpty => None
    case xs               => Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  println(variance(Seq(1, 2, 5, 7, 4)))
}
