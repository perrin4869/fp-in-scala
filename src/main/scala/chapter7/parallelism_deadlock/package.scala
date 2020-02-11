package chapter7

package object parallelism_deadlock {
  // Using custom Future
  import java.util.concurrent.{ExecutorService, Future}

  type Par[A] = ExecutorService => Future[A]
}
