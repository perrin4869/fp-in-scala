package chapter7
import java.util.concurrent.Executors
import parallelism.Par._

object Exercise13 extends App {
  val par = choiceViaChooser(unit(true))(unit("foo"), unit("bar"))
  val parN = choiceNViaChooser(unit(0))(List(lazyUnit("foo")))

  val es = Executors.newFixedThreadPool(1)
  println(run(es)(par).get)
  println(run(es)(parN).get)
}
