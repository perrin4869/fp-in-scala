package chapter4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e)  => Left(e)
      case Right(a) => Right(f(a))
    }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e)  => Left(e)
      case Right(a) => f(a)
    }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(bb) => Right(bb)
      case Left(e)   => b
    }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(aa => b.map(bb => f(aa, bb)))
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, z) => f(a).map2(z)(_ :: _))

  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse(as)(x => x)
}
