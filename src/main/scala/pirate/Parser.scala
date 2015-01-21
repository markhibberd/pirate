package pirate

import scalaz._

sealed trait Parser[A] {
  def map[B](f: A => B): Parser[B] = this match {
    case SwitchParser(meta, a) =>
      SwitchParser(meta, f(a))
    case FlagParser(meta, p) =>
      FlagParser(meta, p.map(f))
    case ArgumentParser(meta, p) =>
      ArgumentParser(meta, p.map(f))
    case CommandParser(name, p) =>
      CommandParser(name, p.map(f))
  }

  def isArg: Boolean = this match {
    case ArgumentParser(_, _) => true
    case _                    => false
  }
}

case class SwitchParser[A](meta: Metadata, a: A) extends Parser[A]
case class FlagParser[A](meta: Metadata, p: Read[A]) extends Parser[A]
case class ArgumentParser[A](meta: Metadata, p: Read[A]) extends Parser[A]
case class CommandParser[A](name: String, p: Parse[A]) extends Parser[A]

object Parser {
  implicit def ParserFunctor: Functor[Parser] = new Functor[Parser] {
    def map[A, B](a: Parser[A])(f: A => B): Parser[B] = a map f
  }

}
