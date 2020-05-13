package chapter6

object Exercise11 extends App {
  import State._

  val insertCoin = (s: Machine) =>
    s match {
      case Machine(true, candies, coins) if candies > 0 =>
        Machine(false, candies, coins + 1)
      case _ => s
    }
  val insertCoinAction = State[Machine, Unit](s => ((), insertCoin(s)))

  val turnKnob = (s: Machine) =>
    s match {
      case Machine(false, candies, coins) if candies > 0 =>
        Machine(true, candies - 1, coins)
      case _ => s
    }
  val turnKnobAction = State[Machine, Unit](s => ((), turnKnob(s)))

  val actions = for {
    _ <- insertCoinAction
    _ <- turnKnobAction
    _ <- insertCoinAction
    _ <- turnKnobAction
    _ <- insertCoinAction
    _ <- turnKnobAction
    _ <- insertCoinAction
    _ <- insertCoinAction
    _ <- turnKnobAction
    _ <- turnKnobAction
    res <- get
  } yield (res.coins, res.candies)

  println(actions.run(Machine(true, 10, 4)))

  def update =
    (i: Input) =>
      (s: Machine) =>
        i match {
          // (i, s) match {
          // case (_, Machine(_, 0, _)) => s

          // Does the two above and handles zero candies case above
          case Coin => insertCoin(s)
          // case (Coin, Machine(false, _, _)) => s
          // case (Coin, Machine(true, candy, coin)) =>
          //   Machine(false, candy, coin + 1)

          // Does the two above and handles zero candies case on the top
          case Turn => turnKnob(s)
          // case (Turn, Machine(true, _, _)) => s
          // case (Turn, Machine(false, candy, coin)) =>
          //   Machine(true, candy - 1, coin)
        }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)

  println(simulateMachine(List(Coin, Turn)).run(Machine(true, 10, 4)))
  println(simulateMachine(List(Coin, Turn)).run(Machine(true, 0, 4)))

  val purchaseCandy = for {
    _ <- modify[Machine](update(Coin))
    _ <- modify[Machine](update(Turn))
    s <- get
  } yield s

  // val purchaseCandy = modify[Machine](update(Coin)).flatMap(_ =>
  //   modify[Machine](update(Turn)).flatMap(_ => get.map(s => s))
  // )

  // val purchaseCandy = modify[Machine](update(Coin)).flatMap(_ =>
  //   modify[Machine](update(Turn)).flatMap(_ => get)
  // )

  println(purchaseCandy.run(Machine(true, 10, 4)))
}
