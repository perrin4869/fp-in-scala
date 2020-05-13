package chapter7
import java.util.concurrent.Executors
import parallelism.Par._

object Exercise14 extends App {
  val nested = unit(unit("foo"))

  val es = Executors.newFixedThreadPool(1)
  println(run(es)(join(nested)).get)

  def chooser(n: Int) = n match {
    case 0 => unit("foo")
    case _ => unit("bar")
  }

  println(run(es)(flatMap(unit(0))(chooser)).get)
  println(run(es)(flatMap(unit(20))(chooser)).get)
}
