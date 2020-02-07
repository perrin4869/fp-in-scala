package chapter5

object Exercise07 extends App {
  println(Stream(1, 2, 3).map(_ + 1).toList)
  println(Stream(1, 2, 3).filter(_ != 2).toList)
  println(Stream(1, 2, 3).append(Stream(4, 5)).toList)
  println(Stream(1, 2, 3).flatMap(Stream(_, 5)).toList)

  Stream(1, 2, 3, 4)
    .map({ x =>
      println(s"map($x)")
      x + 10
    })
    .filter({ x =>
      println(s"filter($x)")
      x % 2 == 0
    })

  println(Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList)
}
