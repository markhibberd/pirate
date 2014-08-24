package pirate.internal

import scalaz._
import pirate._

object Interpretter {
  def run[A](p: Parse[A], args: List[String]): ParseError \/ A =
    ParseTraversal.runParserFully(SkipOpts, p, args).run(NullPrefs)
}
