package chapter7.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // the implementation for java.unil.concurrent.Future
  def unit[A](a: A): Par[A] = (es) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => Par.lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    // ps.foldRight[Par[List[A]]](unit(Nil))((pa, pas) => map2(pa, pas)(_ :: _))
    ps.foldRight(unit(Nil: List[A]))(map2(_, _)(_ :: _))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    sequence(as.map(asyncF(f)))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      as map (asyncF(a => if (f(a)) List(a) else Nil))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(if (_) 0 else 1))(List(t, f))

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es =>
      choices.lift(run(es)(n).get).map(run(es) _).getOrElse(run(es)(choices(0)))

  def choiceMap[K, V](k: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => choices(run(es)(k).get)(es)

  // Alias to flatMap
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    flatMap(pa)(choices)

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(if (_) t else f)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(i => choices(i))

  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(run(es)(pa).get)(es)

  def join[A](a: Par[Par[A]]): Par[A] =
    es => {
      val p = run(es)(a).get
      run(es)(p)
    }

  def flatMapViaJoin[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    join(map(pa)(a => choices(a)))
  // join(map(pa)(f))
}
