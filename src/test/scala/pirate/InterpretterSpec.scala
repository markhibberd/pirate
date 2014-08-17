package pirate

import org.scalacheck.{Arbitrary, Gen}, Arbitrary.arbitrary
import pirate._, Pirate._
import scalaz._, Scalaz._, scalaz.scalacheck.ScalaCheckBinding._

class InterpretterSpec extends test.Spec { def is = s2"""

  Interpretter Properties
  =======================

  Required found                                  $requiredFound
  Required missing                                $requiredMissing
  Default found                                   $defaultFound
  Default missing                                 $defaultMissing
  Option found                                    $optionFound
  Option missing                                  $optionMissing
  Assignment found                                $assignmentFound
  Assignment missing                              $assignmentMissing
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

  def assignmentFound = prop((name: LongNameString, value: String) => {
    run(flag[String](LongName(name.s), ""), List(s"--${name.s}=$value")) ==== value.right
  })

  def assignmentMissing = prop((name: Name, lname: LongNameString, value: String) => name.long != Some(lname.s) ==> {
    run(flag[String](name, ""), List(s"--${lname.s}=$value")).toEither must beLeft
  })

  case class LongNameString(s: String)
  implicit def NonEmptyStringArbitrary: Arbitrary[LongNameString] = Arbitrary(
    (arbitrary[Char] tuple arbitrary[String].filter(!_.contains("="))).map { case (c, s) => LongNameString(c.toString + s) }
  )

  implicit def NameArbitrary: Arbitrary[Name] = Arbitrary(
    Gen.oneOf(
      arbitrary[Char].map(ShortName),
      arbitrary[LongNameString].map(_.s).map(LongName),
      (arbitrary[Char] tuple arbitrary[LongNameString].map(_.s)).map { case (c, s) => BothName(c, s) }
    )
  )
}
