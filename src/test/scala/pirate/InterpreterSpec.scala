package pirate

import org.scalacheck.{Arbitrary, Gen}, Arbitrary.arbitrary
import Pirate._
import pirate.internal._
import scalaz._, Scalaz._, scalaz.scalacheck.ScalaCheckBinding._

sealed trait TestCommand
case class TestWrapper(cmd: TestCommand)
case object TestA extends TestCommand
case object TestB extends TestCommand

class InterpreterSpec extends spec.Spec { def is = s2"""

  Interpreter Properties
  ======================

  Basic interpreters
  ====
  Required found                                  $requiredFound
  Required missing applicatives both              $requiredMissingA
  Required missing applicatives first             $requiredMissingB
  Required missing applicatives second            $requiredMissingC
  Two alternatives missing                        $requiredMissingAlts
  Default found                                   $defaultFound
  Default missing                                 $defaultMissing
  Option found                                    $optionFound
  Option missing                                  $optionMissing
  Assignment found                                $assignmentFound
  Assignment missing                              $assignmentMissing
  Switches toggle on                              $switchesOn
  Switches are off unless toggled                 $switchesOff
  Multiple switches work in a single entry        $multipleSwitches
  Short option flag can come at the end of switch $flagAfterSwitch
  Can measure the length of a set of flags        $flagsLength
  Short option flag args works without spaces     $shortFlagPost
  Position arguments work                         $positionalArgs
  Many arguments work                             $manyArgs
  Many arguments work after a positional          $positionalFollowingMany
  Many arguments work before a positional         $manyFollowingPositional
  Some fails on empty                             $someFailsOnEmpty
  Invalid options produces reasonable error       $invalidOpt
  Invalid argument produces reasonable error      $invalidArg
  Arguments which parse poorly produces reasonable error  $intArgString
  Missing arguments produce sane errors           $missingArg

  Composite interpreters
  ====
  Interpreter handles the first multiple cases   $orFirst
  Interpreter handles the second multiple cases  $orSecond
  Interpreter handles wrapped commands well      $wrappers
  Context flows through multiple subcommands     $subcontext

"""

  def run[A](p: Parse[A], args: List[String]): ParseError \/ A = Interpreter.run(p, args)._2

  def testA(name: String): Parse[TestCommand] =
    terminator(long(name), Flags.empty, TestA)

  def testB(name: String): Parse[TestCommand] =
    terminator(long(name), Flags.empty, TestB)

  def wrap(cmd: Parse[TestCommand]): Parse[TestWrapper] =
    cmd.map(TestWrapper)

  def requiredFound =
    run(flag[String](short('a'), Flags.empty), List("-a", "b")) ==== "b".right

  def requiredMissingA =
    run((flag[String](short('a'), Flags.empty) |@| flag[String](short('b'), Flags.empty))(_ -> _), List()).toEither must beLeft(ParseErrorMissing(ParseTreeAp(List(ParseTreeLeaf(FlagInfo(ShortName('a'), None, None, false, false)), ParseTreeLeaf(FlagInfo(ShortName('b'), None, None, false, false))))))

  def requiredMissingB =
    run((flag[String](short('a'), Flags.empty) |@| flag[String](short('b'), Flags.empty))(_ -> _), List("-b", "c")).toEither must beLeft(ParseErrorMissing(ParseTreeAp(List(ParseTreeLeaf(FlagInfo(ShortName('a'), None, None, false, false))))))

  def requiredMissingC =
    run((flag[String](short('a'), Flags.empty) |@| flag[String](short('b'), Flags.empty))(_ -> _), List("-a", "c")).toEither must beLeft(ParseErrorMissing(ParseTreeAp(List(ParseTreeLeaf(FlagInfo(ShortName('b'), None, None, false, false))))))

  def requiredMissingAlts =
    run(flag[String](short('a'), Flags.empty) ||| flag[String](short('b'), Flags.empty), List()).toEither must beLeft(ParseErrorMissing(ParseTreeAlt(List(ParseTreeLeaf(FlagInfo(ShortName('a'), None, None, false, false)), ParseTreeLeaf(FlagInfo(ShortName('b'), None, None, false, false))))))

  def defaultFound =
    run(flag[String](short('a'), Flags.empty).default("c"), List("-a", "b")) ==== "b".right

  def defaultMissing =
    run(flag[String](short('a'), Flags.empty).default("c"), List()) ==== "c".right

  def optionFound =
    run(flag[String](short('a'), Flags.empty).option, List("-a", "b")) ==== Some("b").right

  def optionMissing =
    run(flag[String](short('a'), Flags.empty).option, List()) ==== None.right

  def assignmentFound = prop((name: LongNameString, value: String) => {
    run(flag[String](long(name.s), Flags.empty), List(s"--${name.s}=$value")) ==== value.right
  })

  def assignmentMissing = prop((name: Name, lname: LongNameString, value: String) => name.long != Some(lname.s) ==> {
    run(flag[String](name, Flags.empty), List(s"--${lname.s}=$value")).toEither must beLeft
  })

  def switchesOn =
    run(switch(short('a'), Flags.empty), List("-a")) ==== true.right

  def switchesOff =
    run(switch(short('a'), Flags.empty), Nil) ==== false.right

  def multipleSwitches =
    run((switch(short('a'), Flags.empty) |@| switch(short('b'), Flags.empty))(_ -> _), List("-ab")) ==== (true, true).right

  def flagAfterSwitch =
    run((switch(short('a'), Flags.empty) |@| flag[String](short('b'), Flags.empty))(_ -> _), List("-ab", "c")) ==== (true, "c").right

  def shortFlagPost =
    run(flag[String](short('a'), Flags.empty), List("-ab")) ==== "b".right

  def flagsLength =
    run(terminator(short('t'), Flags.empty, ()).many.map(_.length), List("-ttt")) ==== 3.right

  def positionalArgs =
    run((argument[String](metavar("src")) |@| argument[String](metavar("dst")))(_ -> _), List("/tmp/src", "tmp/dst")) ==== ("/tmp/src", "tmp/dst").right

  def manyArgs = prop((args: List[String]) =>
    run(arguments[String](metavar("files")), "--" :: args) ==== args.right
  )

  def positionalFollowingMany = prop((args: List[String]) => args.length >= 1 ==> {
    run((argument[String](metavar("src")) |@| arguments[String](metavar("dst")))(_ -> _), "--" :: args) ==== (args.head, args.tail).right
  })

  def manyFollowingPositional = prop((args: List[String]) => args.length >= 1 ==> {
    run((arguments[String](metavar("dst")) |@| argument[String](metavar("src")))(_ -> _), "--" :: args) ==== (args.init, args.last).right
  }).pendingUntilFixed

  def someFailsOnEmpty = run(argument[String](metavar("files")).some, List()).toEither must beLeft

  def invalidOpt = {
    run(flag[String](short('a'), Flags.empty), List("-c")) ==== ParseErrorInvalidOption("-c").left
  }

  def invalidArg = {
    run(flag[String](short('a'), Flags.empty), List("file.txt")) ==== ParseErrorInvalidArgument("file.txt").left
  }

  def intArgString = {
    run(argument[Int](metavar("src")), List("file.txt")) ==== ParseErrorMessage("Error parsing `file.txt` as `Int`").left
  }

  def missingArg = {
    run(argument[Int](metavar("src")), Nil).toEither must beLeft
  }

  def orFirst = prop((nameOne: LongNameString, nameTwo: LongNameString) => nameOne.s != nameTwo.s ==> {
    run((testA(nameOne.s) ||| testB(nameTwo.s)) , List(s"--${nameOne.s}")) must_== TestA.right
  })

  def orSecond = prop((nameOne: LongNameString, nameTwo: LongNameString) => nameOne.s != nameTwo.s ==> {
    run((testA(nameOne.s) ||| testB(nameTwo.s)) , List(s"--${nameTwo.s}")) must_== TestB.right
  })

  def wrappers = prop((name: LongNameString) => {
    run(wrap(testA(name.s)), List(s"--${name.s}")) must_== TestWrapper(TestA).right
  })

  def subcontext = Interpreter.run(subcommand(subcommand(subcommand(().pure[Parse] ~ "third" ) ~ "second" ) ~ "first"),
    "first" :: "second" :: "third" :: Nil) must_== (("first" :: "second" :: "third" :: Nil) -> ().right)

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
