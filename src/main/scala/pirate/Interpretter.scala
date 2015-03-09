package pirate

import scalaz._
import pirate.internal._

object Interpretter {
  def run[A](p: Parse[A], args: List[String]): (List[String], ParseError \/ A) =
    ParseTraversal.runParserFully(SkipOpts, p, args).run.run(NullPrefs)
}
