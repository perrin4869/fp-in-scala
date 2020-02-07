package chapter5

object Exercise08 extends App {
  val ones: Stream[Int] = Stream.cons(1, ones)
  println(ones.take(5).toList)

  def constant[A](a: A): Stream[A] = {
    // https://www.scala-lang.org/files/archive/spec/2.11/04-basic-declarations-and-definitions.html
    // The scope of a name introduced by a declaration or definition
    // is the whole statement sequence containing the binding. However,
    // there is a restriction on forward references in blocks: In a
    // statement sequence s1…sn making up a block, if a simple name in si
    // refers to an entity defined by sj where j≥i, then for all sk
    // between and including si and sj,
    //
    // 1. sk cannot be a variable definition.
    // 2. If sk is a value definition, it must be lazy.
    //
    // val as: Stream[A] = Stream.cons(a, as)

    lazy val as: Stream[A] = Stream.cons(a, as)
    as
  }

  println(constant(5).take(4).toList)
}
