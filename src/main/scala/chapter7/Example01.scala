package chapter7
import nonblocking.Par._
import java.util.concurrent.Executors

object Example01 extends App {
  val p = parMap(List.range(1, 100000))(math.sqrt(_))
  val es = Executors.newFixedThreadPool(2)
  println(run(Executors.newFixedThreadPool(2))(p))

  // Equivalent to:
  // p(es)(println _);
}
