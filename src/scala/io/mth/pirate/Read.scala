package io.mth.pirate

import scalaz._, Scalaz._

/**
 * Minimal parser combinator library.
 */
case class Read[A](read: List[String] => ReadError \/ (List[String], A)) {

  def map[B](f: A => B): Read[B] =
    flatMap(f andThen (Read.value(_)))

  def flatMap[B](f: A => Read[B]): Read[B] =
    Read(in => read(in) match {
      case -\/(e) => -\/(e)
      case \/-((rest, a)) => f(a).read(rest)
    })

  /**
   * Choice combinator, this or that (if this fails).
   */
  def |||(that: => Read[A]): Read[A] =
    Read(in => read(in) match  {
      case -\/(_) => that.read(in)
      case r @ \/-(_) => r
    })

  /**
   * List combinator, zero or more times.
   */
  def list: Read[List[A]] =
    list1 ||| Read.value(List[A]())

  /**
   * List combinator, one or more times.
   */
  def list1: Read[List[A]]  =
    lift2(list)(_::_)

  /**
   * Symbolic representation of list combinator, zero or more times.
   *
   * See: list.
   */
  def * = list

  /**
   * Symbolic representation of list combinator, one or more times.
   *
   * See list1.
   */
  def + = list1

  /**
   * Lazy implementation of lift 2.
   */
  def lift2[B, C](p: => Read[B])(f: (A, B) => C): Read[C] =
    flatMap((a: A) => p.map(f(a, _)))
}

object Read extends shapeless.ProductTypeClassCompanion[Read] {
  def of[A: Read]: Read[A] =
    implicitly[Read[A]]

  def read[A: Read](in: List[String]): ReadError \/ (List[String], A) =
    of[A].read(in)

  def parse[A: Read](in: List[String]): ReadError \/ A =
    read[A](in) flatMap ({
      case (Nil, a) => a.right
      case (_ :: _, _) => ReadErrorTooMuchInput.left
    })

  /**
   * The parser that always succeeds.
   */
  def value[A](v: => A): Read[A] =
    Read(in => (in, v).right)

  /**
   * The parser that always fails with message.
   */
  def failure[A](message: String): Read[A] =
    error(ReadErrorMessage(message))

  /**
   * The parser that always fails.
   */
  def error[A](err: ReadError): Read[A] =
    Read(_ => err.left)

  /**
   * String parser, consumes a single string off the input, fails
   * on empty input.
   */
  def string: Read[String] =
    Read(in => in match {
      case Nil => ReadErrorNotEnoughInput.left
      case h :: t => (t, h).right
    })

  /**
   * Consumes a string off the input, iff pred is true for that
   * string.
   */
  def satisfy(pred: String => Boolean): Read[String] =
    string >>=
      (s => if (pred(s)) value(s) else error(ReadErrorUnexpected(s)))

  /**
   * Consumes a string off the input, iff it equals string s.
   */
  def is(s: String): Read[String] =
    satisfy(_ == s)

  /**
   * Multi-way choice, first success proceeds.
   */
  def choiceN[A](ps: List[Read[A]]): Read[A] =
    ps.foldLeft(error[A](ReadErrorEmpty))(_ ||| _)

  implicit def ReadString: Read[String] =
    string

  implicit def ReadInt: Read[Int] =
    string flatMap (s => try { value(s.toInt) } catch { case e: NumberFormatException => error(ReadErrorInvalidType(s, "Int")) })

  implicit def ReadBoolean: Read[Boolean] =
    string flatMap (s => try { value(s.toBoolean) } catch { case e: NumberFormatException => error(ReadErrorInvalidType(s, "Boolean")) })

  implicit def ReadMonad: Monad[Read] with MonadPlus[Read] = new Monad[Read] with MonadPlus[Read]{
    def point[A](a: => A) = value(a)
    def bind[A, B](p: Read[A])(f: A => Read[B]) = p flatMap f
    def empty[A] = error[A](ReadErrorEmpty)
    def plus[A](p1: Read[A], p2: => Read[A]) = p1 ||| p2
  }

  implicit def ReadMonoid[A]: Monoid[Read[A]] = new Monoid[Read[A]] {
    def zero = error(ReadErrorEmpty)
    def append(p1: Read[A], p2: => Read[A]): Read[A] = p1 ||| p2
  }


  implicit def ReadTypeClass: shapeless.ProductTypeClass[Read] = new shapeless.ProductTypeClass[Read] {
    import shapeless._

    def emptyProduct =
      Read.value(HNil)

    def product[A, T <: HList](A: Read[A], T: Read[T]) =
      for { a <- A; t <- T } yield a :: t

    def project[F, G](instance: => Read[G], to : F => G, from : G => F) =
      instance.map(from)
  }
}

sealed trait ReadError
case object ReadErrorEmpty extends ReadError
case object ReadErrorNotEnoughInput extends ReadError
case object ReadErrorTooMuchInput extends ReadError
case class ReadErrorInvalidType(token: String, expected: String) extends ReadError
case class ReadErrorUnexpected(token: String) extends ReadError
case class ReadErrorMessage(message: String) extends ReadError
