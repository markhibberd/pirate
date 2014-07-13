package io.mth.pirate

import scalaz._, Scalaz._

sealed trait Parser[A] {
  def map[B](f: A => B): Parser[B] = this match {
    case FlagParser(flag, a) =>
      FlagParser(flag, f(a))
    case OptionParser(flag, metas, p) =>
      OptionParser(flag, metas, p.map(f))
    case ArgumentParser(p) =>
      ArgumentParser(p.map(f))
    case SubCommandParser(name, p) =>
      SubCommandParser(name, p.map(f))
  }
}

case class FlagParser[A](flag: Name, a: A) extends Parser[A]
case class OptionParser[A](flag: Name, metas: List[String], p: Read[A]) extends Parser[A]
case class ArgumentParser[A](p: Read[A]) extends Parser[A]
case class SubCommandParser[A](name: String, p: Parse[A]) extends Parser[A]
