package chapter7

package object parallelism {
  import java.util.concurrent.{ExecutorService}

  type Par[A] = ExecutorService => Future[A]
}
