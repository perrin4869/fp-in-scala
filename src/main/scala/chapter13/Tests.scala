package fpinscala.iomonad

object RunConsoleFunction0 extends App {
  import IO3._

  // Stack overflows
  runConsoleFunction0(freeMonad.forever(Console.printLn("Hello")))()
}

// Continues forever without overflow
object RunConsolePar extends App {
  import java.util.concurrent.Executors

  import chapter7.nonblocking.Par
  import IO3._

  val es = Executors.newFixedThreadPool(2)

  Par.run(es)(
    runFree[Console, Par, Unit](
      freeMonad.forever(Console.printLn("Hello"))
    )(consoleToPar)
  )
}

object RunConsoleFunction0Safe extends App {
  import IO3._

  // Stack overflows
  runConsole(freeMonad.forever(Console.printLn("Hello")))
}
