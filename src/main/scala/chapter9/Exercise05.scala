package parser

// From the answers key:

// def wrap[A](p: => Parser[A]): Parser[A]
// similar to `fork` in parallelism

// def many[A](p: Parser[A]): Parser[List[A]] =
//   map2(p, wrap(many(p)))(_ :: _) or succeed(List())
