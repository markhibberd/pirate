package io.mth.pirate

import scalaz._, Scalaz._, \&/._

object Flags extends Flags

trait Flags {
  private def parse[A](p: Parser[A]): Parse[A] =
    PiratedParse(p, Metadata(None, true))

  def terminator[A](n: Name, a: A): Parse[A] =
    parse(FlagParser(n, a))

  def terminatorx[A: Read, B](n: Name, f: Option[A] => B): Parse[B] =
    parse(OptionParser(n, List(), Read.of[A].option).map(f))

  def switch(n: Name): Parse[Boolean] =
    parse(FlagParser(n, true)) ||| false.pure[Parse]

  def option[A: Read](n: Name, meta: String): Parse[A] =
    parse(OptionParser(n, List(meta), Read.of[A])) ||| ValueParse(None)

  object positional {
    def one[A: Read](meta: String): Parse[A] =
      parse(ArgumentParser(Read.of[A])) ||| ValueParse(None)

  }

  object command {
    def of[A](name: String, p: Parse[A]): Parse[A] =
      parse(SubCommandParser(name, p)) ||| ValueParse(None)

  }
}
