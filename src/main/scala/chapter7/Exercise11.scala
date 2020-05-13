package chapter7
import java.util.concurrent.Executors
import parallelism.Par._

object Exercise11 extends App {
  val par = choiceViaChoiceN(unit(true))(unit("foo"), unit("bar"))

  val es = Executors.newFixedThreadPool(1)
  println(run(es)(par).get)
}
