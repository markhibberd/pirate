package io.mth.pirate

import scalaz._, Scalaz._, \&/._

object Flags extends Flags

trait Flags {
  private def parse[A](p: Parser[A]): Parse[A] =
    ParserParse(p, Metadata(None, true))

  def terminator[A](n: Name, a: A): Parse[A] =
    parse(SwitchParser(n, a))

  def terminatorx[A: Read, B](n: Name, f: Option[A] => B): Parse[B] =
    parse(FlagParser(n, List(), Read.of[A].option).map(f))

  def switch(n: Name): Parse[Boolean] =
    parse(SwitchParser(n, true)) ||| false.pure[Parse]

  def flag[A: Read](n: Name, meta: String): Parse[A] =
    parse(FlagParser(n, List(meta), Read.of[A])) ||| ValueParse(None)

  object arguments {
    def one[A: Read](meta: String): Parse[A] =
      parse(ArgumentParser(Read.of[A])) ||| ValueParse(None)
  }

  object command {
    def of[A](name: String, p: Parse[A]): Parse[A] =
      parse(CommandParser(name, p)) ||| ValueParse(None)
  }
}
