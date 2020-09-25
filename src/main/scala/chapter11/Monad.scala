package chapter11
import scala.collection.immutable.Nil

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistrtibute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa)  => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B) = as map f
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // Exercise03
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit[List[A]](Nil))((za, l) => map2(za, l)(_ :: _))
  // Exercise03
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit[List[B]](Nil))((a, fla) => map2(f(a), fla)(_ :: _))

  // Exercise04
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    n match {
      case 0 => unit(Nil)
      case _ => map2(ma, replicateM(n - 1, ma))(_ :: _)
    }

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  // Exercise 06
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms.foldRight(unit[List[A]](Nil))((a, mas) =>
      flatMap(f(a))(b => if (b) map2(unit(a), mas)(_ :: _) else mas)
    )

  // Exercise 07
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // Exercise 08
  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose[Int, A, B]((_) => ma, f)(0)
  // More elegantly from answers key
  // compose((_: Unit) => ma, f)(())

  // Exercise 12
  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

  // Exercise 13
  def flatMapViaJoin[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  // Exercise 13
  def composeViaJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))
}

object Monad {
  import chapter8.Gen
  import chapter7.nonblocking.Par
  import parser.ParserInstance.Parser
  import parser.{Parsers, MyParsers}
  import chapter6.State

  val genMonad = new Monad[Gen] {
    import chapter8.Exercise05

    def unit[A](a: => A): Gen[A] = Exercise05.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  // Exercise01
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.lazyUnit(a)
    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  // Exercise01
  val parserMonad = new Monad[Parser] {
    import MyParsers._ // Add ParserOps

    def unit[A](a: => A): Parser[A] = MyParsers.succeed(a)
    def flatMap[A, B](ma: Parser[A])(f: A => Parser[B]): Parser[B] =
      ma.flatMap(f)
  }

  // Exercise01
  def parserMonadFromAnswersKey[P[+_]](p: Parsers[P]) = new Monad[P] {
    import p.operators

    def unit[A](a: => A): P[A] = p.succeed(a)
    def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] =
      ma.flatMap(f)
  }

  // Exercise01
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  // Exercise01
  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  // Exercise01
  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  // Exercise02 - definitely taken from answers key XD
  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State.unit(a)
    def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
      ma flatMap (f)
  }
}

// Exercise 17
case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val identityMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }
}

// Exercise 20
case class Reader[R, A](run: R => A)

object Reader { // Read-only state
  def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(ma.run(r)).run(r))
  }
}
