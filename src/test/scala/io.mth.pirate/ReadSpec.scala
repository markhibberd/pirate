package io.mth.pirate

import shapeless._
import scalaz._, Scalaz._
import io.mth.pirate.test.Laws._
import io.mth.pirate.test.Arbitraries._

class ReadSpec extends test.Spec { def is = s2"""

  Read Properties
  ===============

  Witness (compilation is sufficient) ${ok}

"""

  Read.of[Int]
  Read.of[String]
  Read.of[Boolean]
  Read.of[Char]

  import Read.auto._

  Read.of[(Int, Int)]
  Read.of[(String, Int)]
  Read.of[(Int, String)]
  Read.of[(Int, Int, Int, Int, Int)]
}
