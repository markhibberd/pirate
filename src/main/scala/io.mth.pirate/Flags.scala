package io.mth.pirate

import scalaz._, Scalaz._, \&/._

object Flags extends Flags

trait Flags {
  object switch {
    def short(c: Char): Parse[Boolean] =
      PiratedParse(
        FlagParser(This(c), true),
        PirateMeta(None, true)) ||| false.pure[Parse]
  }

  object option {
    def short[A: Read](c: Char, meta: String): Parse[A] =
      PiratedParse(
        OptionParser(This(c), List(meta), Read.of[A]),
        PirateMeta(None, true)) ||| ValueParse(None)
  }

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
