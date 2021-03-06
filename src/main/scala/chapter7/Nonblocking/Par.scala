package chapter7.nonblocking

import java.util.concurrent.{
  Callable,
  ExecutorService,
  CountDownLatch,
  TimeUnit
}
import java.util.concurrent.atomic.AtomicReference

sealed trait Future[A] {
  private[chapter7] def apply(k: A => Unit): Unit
}

object Par {
  // run here is different from the blocking implementation
  // the blocking implementation returns a cancellable Future[A]
  // which alsogives the caller the flexebility of deciding how long
  // to wait before blocking the call
  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a =>
      ref.set(a); latch.countDown
    }
    latch.await
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
    es =>
      new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None

          val combiner = Actor[Either[A, B]](es) {
            case Left(a) =>
              br match {
                case None    => ar = Some(a)
                case Some(b) => eval(es)(cb(f(a, b)))
              }

            case Right(b) =>
              ar match {
                case None    => br = Some(b)
                case Some(a) => eval(es)(cb(f(a, b)))
              }
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  /**
    * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
    * This will come in handy in Chapter 13.
    */
  def async[A](f: (A => Unit) => Unit): Par[A] =
    es =>
      new Future[A] {
        def apply(k: A => Unit) = f(k)
      }

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    // Without the fork below, the main thread gets stack overflown due to
    // excessive calls into nested map2 Future.apply, for large list inputs
    // This way, map2's Future.apply is always executed in a new thread after
    // the first call.
    ps.foldRight(unit(Nil: List[A]))((pa, pas) => map2(pa, fork(pas))(_ :: _))
  // map(ps.foldLeft(unit(Nil: List[A]))(map2(_, _)((l, x) => x :: l)))(
  //   _.reverse
  // )
  // ps.foldRight(unit(Nil: List[A]))(map2(_, _)(_ :: _))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    sequence(as.map(asyncF(f)))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      as map (asyncF(a => if (f(a)) List(a) else Nil))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    run(e)(p) == run(e)(p2) // written by me

  /** A non-strict version of `unit` */
  def delay[A](a: => A): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

  // Extra functionality used in later chapters but not mentioned/developed
  // in the main text
  // Taken from the answers key
  def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    es =>
      new Future[B] {
        def apply(cb: B => Unit): Unit =
          p(es)(a => f(a)(es)(cb))
      }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    Par.fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

  def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
    sequenceBalanced(as.map(asyncF(f)))
}
