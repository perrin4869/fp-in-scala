package chapter12

import chapter10.Foldable
import chapter11.Functor
import chapter12.Applicative.Failure
import chapter12.Applicative.Success

trait Applicative[F[_]] extends Functor[F] { self =>
  // primitive combinators
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    // Exercise02 - so proud I got this one :D
    apply(apply(unit(f.curried))(fa))(fb)
  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    // Exercise02
    map2(fab, fa)((f, a) => f(a))

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  // Exercise01
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit[List[A]](Nil))((za, l) => map2(za, l)(_ :: _))

  // Exercise01
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    n match {
      case 0 => unit(Nil)
      case _ => map2(ma, replicateM(n - 1, ma))(_ :: _)
    }

  // Exercise01
  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  // Exercise02
  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  // Exercise 03
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E
  ): F[E] = apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  // Exercise08
  def product[G[_]](
      G: Applicative[G]
  ): Applicative[({ type f[x] = (F[x], G[x]) })#f] =
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def apply[A, B](fab: (F[A => B], G[A => B]))(
          fa: (F[A], G[A])
      ): (F[B], G[B]) = (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }

  // Exercise09
  def compose[G[_]](
      G: Applicative[G]
  ): Applicative[({ type f[x] = F[G[x]] })#f] =
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(
          f: (A, B) => C
      ): F[G[C]] =
        self.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)(f))
      // TODO: implement with apply
      // override def apply[A, B](fgab: F[G[A => B]])(fga: F[G[A]]): F[G[B]] =
      //   self.apply(fab)(fa)
    }

  // Exercise12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map[K, V]()))((kv, mkv) =>
      map2(kv._2, mkv)((v, m) => m.updated(kv._1, v))
    )
  // more readable from answers key:
  // def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
  // (ofa foldLeft unit(Map.empty[K,V])) { case (acc, (k, fv)) =>
  //   map2(acc, fv)((m, v) => m + (k -> v))
  // }
}

object Applicative {
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.continually(a)

    override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(
        f: (A, B) => C
    ): Stream[C] = fa zip fb map f.tupled

    // Exercise 04 - sequence in this context means that a List[Stream[A]]
    // is zipped into a single stream Stream[List[A]] where each List[A]
    // has an element from the list of streams
  }

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector())
      extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  // Exercise06
  def validationApplicative[E] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)
      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
          f: (A, B) => C
      ): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b))        => Success(f(a, b))
          case (f @ Failure(_, _), Success(_)) => f
          case (Success(_), f @ Failure(_, _)) => f
          case (Failure(h1, t1), Failure(h2, t2)) =>
            Failure(h1, h2 +: (t1 ++ t2))
        }
    }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(f))

  override def map[A, B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))

  override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
}

object Monad {
  import chapter6.State

  // Exercise05
  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x] })#f] =
    new Monad[({ type f[x] = Either[E, x] })#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](ma: Either[E, A])(
          f: A => Either[E, B]
      ): Either[E, B] = ma flatMap f
    }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](
        st: State[S, A]
    )(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  import chapter6.State
  import chapter10.Monoid

  def traverse[G[_]: Applicative, A, B](
      fa: F[A]
  )(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  // Exercise 14
  type Id[A] = A
  implicit val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma)
  }
  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)

  type Const[M, B] = M

  implicit def monoidApplicative[M: Monoid] =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = implicitly[Monoid[M]].zero
      override def map2[A, B, C](fa: M, fb: M)(f: (A, B) => C): M =
        implicitly[Monoid[M]].op(fa, fb)
    }

  override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({ type f[x] = Const[M, x] })#f, A, Nothing](as)(f)(
      monoidApplicative(mb)
    )

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)((a: A) =>
      (for {
        i <- State.get[Int]
        _ <- State.set(i + 1)
      } yield (a, i))
    ).run(0)._1

  override def toList[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) =>
      (for {
        as <- State.get[List[A]]
        _ <- State.set(a :: as)
      } yield ())
    ).run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) =>
      (for {
        s1 <- State.get[S]
        (b, s2) = f(a, s1)
        _ <- State.set(s2)
      } yield b)
    ).run(s)

  def toListViaMapAccum[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndexViaMapAccum[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)({ case (_, head :: tail) => (head, tail) })._1

  // Exercise07
  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(as, z)((a, b) => ((), f(b, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil)     => sys.error("zip: Incompatible shapes")
      case (a, b :: bs) => ((a, b), bs)
    })._1

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    (mapAccum(fa, toList(fb)) {
      case (a, Nil)     => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    })._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    (mapAccum(fb, toList(fa)) {
      case (b, Nil)     => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    })._1

  // Exercise 18
  def fuseViaRetardation[G[_], H[_], A, B](
      fa: F[A]
  )(f: A => G[B], g: A => H[B])(
      implicit
      G: Applicative[G],
      H: Applicative[H]
  ): (G[F[B]], H[F[B]]) =
    // Simplest duh implementation:
    (traverse(fa)(f)(G), traverse(fa)(g)(H))

  // Exercise18 - from answers key - not retarded like my solution above
  def fuse[G[_]: Applicative, H[_]: Applicative, A, B](
      fa: F[A]
  )(f: A => G[B], g: A => H[B]) =
    traverse[({ type f[x] = (G[x], H[x]) })#f, A, B](fa)(a => (f(a), g(a)))(
      implicitly[Applicative[G]] product implicitly[Applicative[H]]
    )

  // Exercise19
  def compose[G[_]: Traverse]: Traverse[({ type f[x] = F[G[x]] })#f] = {
    val self = this
    new Traverse[({ type f[x] = F[G[x]] })#f] {
      override def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(
          f: A => H[B]
      ): H[F[G[B]]] =
        self.traverse(fga)(ga => implicitly[Traverse[G]].traverse(ga)(f))
    }
  }

  def composeM[F[_], G[_]](
      implicit
      F: Monad[F],
      G: Monad[G],
      T: Traverse[G]
  ): Monad[({ type f[x] = F[G[x]] })#f] =
    new Monad[({ type f[x] = F[G[x]] })#f] {
      def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
      override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        F.flatMap(ma)(ga =>
          // My initial line:
          // F.map(T.sequence(G.map(ga)(f)))((gga: G[G[B]]) => G.join(gga))
          F.map(T.traverse(ga)(f))((G.join))
        )
    }
}

object Traverse {
  import chapter3.Tree

  // Exercise13
  val listTraverse = new Traverse[List] {
    override def sequence[G[_]: Applicative, A](
        fga: List[G[A]]
    ): G[List[A]] =
      fga.foldRight(implicitly[Applicative[G]].unit(Nil: List[A]))((ga, acc) =>
        implicitly[Applicative[G]].map2(ga, acc)(_ :: _)
      )
  }

  // from answers key (they implemented traverse instead of sequence)
  val listTraverseFromAnswersKey = new Traverse[List] {
    override def traverse[G[_], A, B](
        as: List[A]
    )(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
  }

  // Exercise13
  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](
        oa: Option[A]
    )(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa.foldRight(G.unit(None: Option[B]))((a, none) => G.map(f(a))(Some(_)))
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])

  // Exercise13
  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_]: Applicative, A, B](ta: Tree[A])(
        f: A => G[B]
    ): G[Tree[B]] =
      implicitly[Applicative[G]]
        .map2(f(ta.head), listTraverse.traverse(ta.tail)(traverse(_)(f)))(
          Tree(_, _)
        )
  }
}
