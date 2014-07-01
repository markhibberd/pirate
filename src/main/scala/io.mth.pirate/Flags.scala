package io.mth.pirate

import scalaz._, Scalaz._, \&/._

object Flags extends Flags

trait Flags {
  def terminator[A](n: Name, a: A): Parse[A] =
    PiratedParse(
      FlagParser(n, a),
      PirateMeta(None, true))

  def terminatorx[A: Read, B](n: Name, f: Option[A] => B): Parse[B] =
    PiratedParse(
      OptionParser(n, List(), Read.of[A].option).map(f),
      PirateMeta(None, true))

  def switch(n: Name): Parse[Boolean] =
    PiratedParse(
      FlagParser(n, true),
      PirateMeta(None, true)) ||| false.pure[Parse]

  def option[A: Read](n: Name, meta: String): Parse[A] =
    PiratedParse(
      OptionParser(n, List(meta), Read.of[A]),
      PirateMeta(None, true)) ||| ValueParse(None)

  object positional {
    def one[A: Read](meta: String): Parse[A] =
      PiratedParse(
        ArgumentParser(Read.of[A]),
        PirateMeta(None, true)) ||| ValueParse(None)

  }

  object command {
    def of[A](name: String, p: Parse[A]): Parse[A] =
      PiratedParse(SubCommandParser(name, p), PirateMeta(None, true)) ||| ValueParse(None)

  }
}
