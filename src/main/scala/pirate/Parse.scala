package pirate

import scalaz._, Scalaz._

sealed trait Parse[A] {
  def ~(name: String): Command[A] =
    Command(name, None, this)

  def map[B](f: A => B): Parse[B] = this match {
    case ValueParse(o) =>
      ValueParse(o.map(f))
    case ParserParse(p, m) =>
      ParserParse(p.map(f), m)
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
    map(_.some) ||| ValueParse(Some(None))

  def default(fallback: => A): Parse[A] =
    option.map(_.getOrElse(fallback))

  def not(implicit ev: A =:= Boolean): Parse[Boolean] =
    map(!_)

  def hidden: Parse[A] = this match {
    case ValueParse(o) => this
    case ParserParse(p, m) => ParserParse(p, m.copy(visible = false))
    case ApParse(k, a) => ApParse(k, a hidden)
    case AltParse(a, b) => AltParse(a hidden, b hidden)
    case BindParse(k, a) => BindParse(k, a hidden)
  }

  def <> (d: String): Parse[A] = this match {
    case ValueParse(o) => this
    case ParserParse(p, m) => ParserParse(p, m.copy(description = d.some))
    case ApParse(k, a) => ApParse(k, a <> d)
    case AltParse(a, b) => AltParse(a <> d, b <> d)
    case BindParse(k, a) => BindParse(k, a <> d)
  }
}

case class ValueParse[A](m: Option[A]) extends Parse[A]
case class ParserParse[A](p: Parser[A], m: Metadata) extends Parse[A]
case class ApParse[A, B](f: Parse[A => B], a: Parse[A]) extends Parse[B]
case class AltParse[A](a: Parse[A], b: Parse[A]) extends Parse[A]
case class BindParse[A, B](f: A => Parse[B], a: Parse[A]) extends Parse[B]

object Parse {
  implicit def ParseMonad: Monad[Parse] with Plus[Parse] = new Monad[Parse] with Plus[Parse] {
    def point[A](a: => A) = ValueParse(Some(a))
    override def map[A, B](a: Parse[A])(f: A => B) = a map f
    def bind[A, B](a: Parse[A])(f: A => Parse[B]) = a flatMap f
    override def ap[A, B](a: => Parse[A])(f: => Parse[A => B]) = ApParse(f, a)
    def plus[A](a: Parse[A], b: => Parse[A]) = AltParse(a, b)
  }
}
