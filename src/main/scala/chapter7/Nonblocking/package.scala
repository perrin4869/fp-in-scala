package chapter7

package object nonblocking {
  import java.util.concurrent.{ExecutorService}

  type Par[A] = ExecutorService => Future[A]
}
