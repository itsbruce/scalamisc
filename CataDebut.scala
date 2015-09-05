// Tony Morris's "Debut with a Catamorphism" challenge
// http://tonymorris.github.io/blog/posts/debut-with-a-catamorphism/

trait MyOption[+A] {
  import MyOption._
  // single abstract method
  def cata[X](some: A => X, none: => X): X

  def map[B](f: A => B): MyOption[B] = cata(f andThen some _, none)
  // or
  // def map[B](f: A => B): MyOption[B] = flatMap(f andThen some _)

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = cata(f, none)

  def getOrElse[AA >: A](e: => AA): AA = cata(identity, e)

  def filter(p: A => Boolean): MyOption[A] = flatMap { a =>
    if (p(a)) some(a) else none
  }

  def foreach(f: A => Unit): Unit = cata(f, ())

  def isDefined: Boolean = cata(_ => true, false)

  def isEmpty: Boolean = cata(_ => false, true)

  // WARNING: not defined for None
  def get: A = cata(identity, error("Get on none"))

  def orElse[AA >: A](o: MyOption[AA]): MyOption[AA] = cata(some, o)

  def toLeft[X](right: => X): Either[A, X] = cata({Left(_)}, Right(right))

  def toRight[X](left: => X): Either[X, A] = cata({Right(_)}, Left(left))

  def toList: List[A] = cata(List(_), Nil)

  def iterator: Iterator[A] = cata({Iterator(_)}, Iterator())
}

object MyOption {
  def none[A] = new MyOption[A] {
    def cata[X](s: A => X, n: => X) = n
  }

  def some[A](a: A) = new MyOption[A] {
    def cata[X](s: A => X, n: => X) = s(a)
  }
}
