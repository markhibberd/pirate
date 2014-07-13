package io.mth.pirate

import scalaz._, Scalaz._, \&/._
import io.mth.pirate.internal._

object Interpretter {
  def run[A](p: Parse[A], args: List[String]): ParseError \/ A =
    ParseTraversal.runParserFully(SkipOpts, p, args).run(NullPrefs)
}
