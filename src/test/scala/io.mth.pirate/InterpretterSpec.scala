package io.mth.pirate

import Pirate._
import scalaz._, Scalaz._

class InterpretterSpec extends test.Spec { def is = s2"""

  Interpretter Properties
  =======================

  Required found                                  $requiredFound
  Required missing                                $requiredMissing
"""

  import Interpretter._

  def requiredFound =
    run(option[String]('a', ""), List("-a", "b")) ==== "b".right

  def requiredMissing =
    run(option[String]('a', ""), List()).toEither must beLeft
}
