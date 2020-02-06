package chapter4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None      => None
  }
  def flatMap_1[B](f: A => Option[B]): Option[B] = this match {
    case Some(get) => f(get)
    case None      => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None      => default
  }
  def orElse_1[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _    => this
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(get) if f(get) => this
    case None                => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def Try[A](x: => A) =
    try {
      Some(x)
    } catch {
      case e: Exception => None
    }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C) =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, z) => map2(f(x), z)(_ :: _))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}
