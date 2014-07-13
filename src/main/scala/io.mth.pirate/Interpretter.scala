package io.mth.pirate

import scalaz._, Scalaz._, \&/._

object Interpretter extends Interpretter

trait Interpretter {
  def run[A](p: Parse[A], args: List[String]): PError \/ A =
    ParseTools.runParserFully(SkipOpts, p, args).run(NullPPrefs)
}
