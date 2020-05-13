package chapter7
import java.util.concurrent.Executors
import parallelism.Par._

object Exercise12 extends App {
  val par = choiceMap(unit("mykey"))(Map("mykey" -> unit("myval")))

  val es = Executors.newFixedThreadPool(1)
  println(run(es)(par).get)
}
