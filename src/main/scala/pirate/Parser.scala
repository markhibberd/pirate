package pirate

import scalaz._, Scalaz._

sealed trait Parser[A] {
  def map[B](f: A => B): Parser[B] = this match {
    case SwitchParser(flag, a) =>
      SwitchParser(flag, f(a))
    case FlagParser(flag, metas, p) =>
      FlagParser(flag, metas, p.map(f))
    case ArgumentParser(p) =>
      ArgumentParser(p.map(f))
    case CommandParser(name, p) =>
      CommandParser(name, p.map(f))
  }
}

case class SwitchParser[A](flag: Name, a: A) extends Parser[A]
case class FlagParser[A](flag: Name, metas: List[String], p: Read[A]) extends Parser[A]
case class ArgumentParser[A](p: Read[A]) extends Parser[A]
case class CommandParser[A](name: String, p: Parse[A]) extends Parser[A]

object Parser {
  implicit def ParserFunctor: Functor[Parser] = new Functor[Parser] {
    def map[A, B](a: Parser[A])(f: A => B): Parser[B] = a map f
  }

}
