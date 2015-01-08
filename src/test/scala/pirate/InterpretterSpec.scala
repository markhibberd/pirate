package pirate

import org.scalacheck.{Arbitrary, Gen}, Arbitrary.arbitrary
import Pirate._
import scalaz._, Scalaz._, scalaz.scalacheck.ScalaCheckBinding._


case class TestWrapper(
  cmd: TestCommand
)
sealed trait TestCommand
case class TestA(list: String) extends TestCommand
case class TestB(list: String) extends TestCommand

class InterpretterSpec extends spec.Spec { def is = s2"""

  Interpretter Properties
  =======================

  Basic interpretters
  ====
  Required found                                  $requiredFound
  Required missing                                $requiredMissing
  Default found                                   $defaultFound
  Default missing                                 $defaultMissing
  Option found                                    $optionFound
  Option missing                                  $optionMissing
  Assignment found                                $assignmentFound
  Assignment missing                              $assignmentMissing
  Switches toggle on                              $switchesOn
  Switches are off unless toggled                 $switchesOff
  Position arguments work                         $positionalArgs
  Many arguments work                             $manyArgs
  Many arguments work after a positional          $positionalFollowMany
  Some fails on empty                             $someFailsOnEmpty

  Composite interpretters
  ====
  Interpretter handles the first multiple cases   $orFirst
  Interpretter handles the second multiple cases  $orSecond
  Interpretter handles wrapped commands well      $wrappers

"""

  import Interpretter._

  def testA(name: String): Parse[TestCommand] = (TestA |*| (
    flag[String](name, "")
  )).map(x => x)

  def testB(name: String): Parse[TestCommand] = (TestB |*| (
    flag[String](name, "")
  )).map(x => x)

  def wrap(cmd: Parse[TestCommand]): Parse[TestWrapper] =
    cmd.map(TestWrapper(_))

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

  def switchesOn =
    run(switch('a'), List("-a")) ==== true.right

  def switchesOff =
    run(switch('a'), List("-b")) ==== false.right

  def positionalArgs =
    run((arguments.one[String]("src") |@| arguments.one[String]("dst"))(_ -> _), List("/tmp/src", "tmp/dst")) ==== ("/tmp/src", "tmp/dst").right

  def manyArgs = prop((args: List[String]) =>
    run(arguments.many[String]("files"), args) ==== args.right
  )

  def positionalFollowMany = prop((args: List[String]) => args.length >= 1 ==> {
    run((arguments.one[String]("src") |@| arguments.many[String]("dst"))(_ -> _), args) ==== (args.head, args.tail).right
  })

  def someFailsOnEmpty = run(arguments.some[String]("files"), List()).toEither must beLeft

  def orFirst = prop((nameOne: LongNameString, nameTwo: LongNameString, value: String) => nameOne.s != nameTwo.s ==> {
    run((testA(nameOne.s) ||| testB(nameTwo.s)) , List(s"--${nameOne.s}=$value")) must_== TestA(value).right
  })

  def orSecond = prop((nameOne: LongNameString, nameTwo: LongNameString, value: String) => nameOne.s != nameTwo.s ==> {
    run((testA(nameOne.s) ||| testB(nameTwo.s)) , List(s"--${nameTwo.s}=$value")) must_== TestB(value).right
  })

  def wrappers = prop((name: LongNameString, value: String) => {
    run(wrap(testA(name.s)), name.s :: value :: Nil).toEither must_== TestWrapper(TestA(value)).right
  }).pendingUntilFixed

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
