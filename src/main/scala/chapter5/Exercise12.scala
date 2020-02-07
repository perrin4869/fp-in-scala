package chapter5

object Exercise12 extends App {
  def fibs: Stream[Int] =
    Stream.unfold((0, 1))({
      case (f0, f1) => Some((f0, (f1, f0 + f1)))
    })

  println(fibs.take(15).toList)

  def from(n: Int) =
    Stream.unfold(n)((s) => Some((s, s + 1)))
  println(from(5).take(15).toList)

  def constant(n: Int) =
    Stream.unfold(n)((s) => Some((s, s)))
  println(constant(5).take(15).toList)

  def ones =
    Stream.unfold(1)((s) => Some((s, s)))
  println(ones.take(15).toList)
}
