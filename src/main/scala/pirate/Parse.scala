package pirate

import scalaz._, Scalaz._

sealed trait Parse[A] {
  def ~(name: String): Command[A] =
    Command(name, None, this)

  def map[B](f: A => B): Parse[B] = this match {
    case ValueParse(o) =>
      ValueParse(f(o))
    case ParserParse(p) =>
      ParserParse(p.map(f))
    case ApParse(k, a) =>
      ApParse(k.map(ff => ff.map(f)), a)
    case AltParse(a, b) =>
      AltParse(a.map(f), b.map(f))
    case BindParse(k, a) =>
      BindParse(k.map(_.map(f)), a)
  }

  def flatMap[B](f: A => Parse[B]): Parse[B] =
    BindParse(f, this)

  def |||(other: Parse[A]): Parse[A] =
    AltParse(this, other)

  def option: Parse[Option[A]] =
    map(_.some) ||| ValueParse(None)

  def default(fallback: => A): Parse[A] =
    option.map(_.getOrElse(fallback))

  def not(implicit ev: A =:= Boolean): Parse[Boolean] =
    map(!_)

  def some: Parse[List[A]] = for {
    a <- this
    b <- many
  } yield (a :: b)

  def many: Parse[List[A]] =
    some ||| nil.pure[Parse]
}

case class ValueParse[A](m: A) extends Parse[A]
case class ParserParse[A](p: Parser[A]) extends Parse[A]
case class ApParse[A, B](f: Parse[A => B], a: Parse[A]) extends Parse[B]
case class AltParse[A](a: Parse[A], b: Parse[A]) extends Parse[A]
case class BindParse[A, B](f: A => Parse[B], a: Parse[A]) extends Parse[B]

object Parse {
  implicit def ParseMonad: Monad[Parse] with Plus[Parse] = new Monad[Parse] with Plus[Parse] {
    def point[A](a: => A) = ValueParse(a)
    override def map[A, B](a: Parse[A])(f: A => B) = a map f
    def bind[A, B](a: Parse[A])(f: A => Parse[B]) = a flatMap f
    override def ap[A, B](a: => Parse[A])(f: => Parse[A => B]) = ApParse(f, a)
    def plus[A](a: Parse[A], b: => Parse[A]) = AltParse(a, b)
  }
}
