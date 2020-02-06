package chapter4

object Exercise01 extends App {
  println(Some(1).map(_ + 1))
  println(Some(1).flatMap(x => Some(x + 1)))
  println(Some(1).flatMap((x) => None))
  println(Some(1).orElse(Some(2)))
  println(None.orElse(Some(2)))
  println(
    None
      .orElse(None)
      .getOrElse(Some(2))
      .orElse(Some(5))
  )
}
