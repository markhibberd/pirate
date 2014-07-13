package io.mth.pirate

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
    run(flag[String]('a', ""), List("-a", "b")) ==== "b".right

  def requiredMissing =
    run(flag[String]('a', ""), List()).toEither must beLeft

  def defaultFound =
    run(flag[String]('a', "").default("c"), List("-a", "b")) ==== "b".right

  def defaultMissing =
    run(flag[String]('a', "").default("c"), List()) ==== "c".right

  def optionFound =
    run(flag[String]('a', "").option, List("-a", "b")) ==== Some("b").right

  def optionMissing =
    run(flag[String]('a', "").option, List()) ==== None.right
}
