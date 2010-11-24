package io.mth.pirate

import org.scalatest.FunSuite

// FIX Replace with an automated scalacheck test.
class ParserManualTest extends FunSuite {
  import Flag._
  import Data.CannedFlags._

  test("simple flags") {
    check(f,  List("-a", "-b"),           List("-b"),            Map(("a", "")))
    check(f,  List("--aa", "--bb", "-c"), List("--bb", "-c"),    Map(("a", "")))
    check(f1, List("-b", "x", "-a"),      List("-a"),            Map(("b", "x")))
    check(f1, List("--bb", "x", "--aa"),  List("--aa"),          Map(("b", "x")))

    check(s,  List("-c", "-b"),           List("-b"),            Map(("c", "")))
    check(s1, List("-d", "x", "-a"),      List("-a"),            Map(("d", "x")))

    check(l,  List("--ee", "--bb", "-c"), List("--bb", "-c"),    Map(("e", "")))
    check(l1, List("--ff", "x", "--aa"),  List("--aa"),          Map(("f", "x")))

    bomb(s, List("--cc"))
    bomb(f, List("a", "b", "c"))
  }

  test("combined flags") {
    check(c1, List("-a", "-c", "--ee"),       List(),                Map(("a", ""), ("c", ""), ("e", "")))
    check(c1, List("-a", "-c", "-d", "--ee"), List("-d", "--ee"),    Map(("a", ""), ("c", "")))
    check(c1, List("--aa", "-c", "--ee"),     List(),                Map(("a", ""), ("c", ""), ("e", "")))

    check(c2, List("-b", "x", "-d", "y", "--ff", "z"),        List(),                 Map(("b", "x"), ("d", "y"), ("f", "z")))
    check(c2, List("-b", "x", "-d", "y", "-a", "--ff", "z"),  List("-a", "--ff", "z"), Map(("b", "x"), ("d", "y")))
    check(c2, List("--bb", "x", "-d", "y", "--ff", "z"),      List(),                 Map(("b", "x"), ("d", "y"), ("f", "z")))

    check(c3, List("-a", "-b", "x"),     List(), Map(("a", ""), ("b", "x")))
    check(c3, List("--aa", "--bb", "x"), List(), Map(("a", ""), ("b", "x")))
    check(c3, List("-a", "--bb", "x"),   List(), Map(("a", ""), ("b", "x")))
    check(c3, List("--aa", "-b", "x"),   List(), Map(("a", ""), ("b", "x")))
    check(c3, List("-b", "x", "-a"),     List(), Map(("a", ""), ("b", "x")))
    check(c3, List("-b", "x", "--aa"),   List(), Map(("a", ""), ("b", "x")))
    check(c3, List("--bb", "x", "-a"),   List(), Map(("a", ""), ("b", "x")))
    check(c3, List("--bb", "x", "--aa"), List(), Map(("a", ""), ("b", "x")))

    check(c6, List("-a", "-b", "x", "-c", "-d", "y", "--ee", "--ff", "z"), List(), Map(("a", ""), ("c", ""), ("e", ""), ("b", "x"), ("d", "y"), ("f", "z")))
    check(c6, List("-b", "x", "-a", "-c", "-d", "y", "--ee", "--ff", "z"), List(), Map(("a", ""), ("c", ""), ("e", ""), ("b", "x"), ("d", "y"), ("f", "z")))
    check(c6, List("--ee", "--ff", "z", "-b", "x", "-a", "-c", "-d", "y"), List(), Map(("a", ""), ("c", ""), ("e", ""), ("b", "x"), ("d", "y"), ("f", "z")))
    check(c6, List("--ff", "z", "-b", "x", "--ee", "-a", "-c", "-d", "y"), List(), Map(("a", ""), ("c", ""), ("e", ""), ("b", "x"), ("d", "y"), ("f", "z")))
  }

  test("break args") {
    check(c6, List("-a", "-b", "x", "--", "-c", "-d", "y", "--ee", "--ff", "z"), List("-c", "-d", "y", "--ee", "--ff", "z"), Map(("a", ""), ("b", "x")))
  }

//  test("simple positional args") {
//    val one = c6 | positional1("FRED")((s: String) => (m: Map[String, String]) => m + (("FRED", s)))
//    val two = one | positionalN("BARNEY")((ss: List[String]) => (m: Map[String, String]) => m + (("BARNEY", ss.mkString(","))))
//
//    check(one, List("fred"),                         List(), Map(("FRED", "fred")))
//
//    check(two, List("fred", "barney"),               List(), Map(("FRED", "fred"), ("BARNEY", "barney")))
//    check(two, List("fred"),                         List(), Map(("FRED", "fred"), ("BARNEY", "")))
//    check(two, List("fred", "b1", "b2"),             List(), Map(("FRED", "fred"), ("BARNEY", "b1,b2")))
//    check(two, List("-a", "--", "-c", "--ff", "z"),  List(), Map(("FRED", "-c"), ("BARNEY", "--ff,z"), ("a", "")))
//  }

  def bomb(f: Flag[Map[String, String]], args: List[String]) =
    assert(p(f, args).fold(_ => true, _ => false))

  def check(f: Flag[Map[String, String]], args: List[String], rest: List[String], expected: Map[String, String]) = {
    val result = p(f, args)
    assert(result.fold(
        _ => false,
        { case (r, f) => r == rest && f(Map()) == expected}
      ), result.fold(
        msg => "failed to parse [" + msg + "]",
        { case (r, f) => "rest[" + r + "], expected[" + rest + "], [" + (r == rest) + "]\nvalue[" + f(Map()) + "], expected[" + expected + "], [" + (f(Map()) == expected) + "]\n\n" }
      ))
  }

  def p(f: Flag[Map[String, String]], args: List[String]) =
      f.toParser.parse(args.toList)

}