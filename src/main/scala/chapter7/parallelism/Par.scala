package chapter7.parallelism

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

  def lazyUnit[A](a: A): Par[A] = fork(unit(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    // ps.foldLeft(unit(Nil: List[A]))(map2(_, _)((l, x) => x :: l))
    // ps.foldRight(unit(Nil: List[A]))(map2(_, _)(_ :: _))
    // Without the fork below, large lists cause a stack overflow when
    // evaluating the futures
    ps.foldRight(unit(Nil: List[A]))((pa, pas) => map2(pa, fork(pas))(_ :: _))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    sequence(as.map(asyncF(f)))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      as map (asyncF(a => if (f(a)) List(a) else Nil))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    run(e)(p) == run(e)(p2) // written by me

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)
}
