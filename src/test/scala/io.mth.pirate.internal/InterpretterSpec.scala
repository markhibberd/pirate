package io.mth.pirate.internal

import io.mth.pirate._, Pirate._
import scalaz._, Scalaz._

class InterpretterSpec extends test.Spec { def is = s2"""

  Interpretter Properties
  =======================

  Required found                                  $requiredFound
  Required missing                                $requiredMissing
  Default found                                   $defaultFound
  Default missing                                 $defaultMissing
  Option found                                    $optionFound
  Option missing                                  $optionMissing
"""

  import Interpretter._

  def requiredFound =
    run(option[String]('a', ""), List("-a", "b")) ==== "b".right

  def requiredMissing =
    run(option[String]('a', ""), List()).toEither must beLeft

  def defaultFound =
    run(option[String]('a', "").default("c"), List("-a", "b")) ==== "b".right

  def defaultMissing =
    run(option[String]('a', "").default("c"), List()) ==== "c".right

  def optionFound =
    run(option[String]('a', "").option, List("-a", "b")) ==== Some("b").right

  def optionMissing =
    run(option[String]('a', "").option, List()) ==== None.right
}
