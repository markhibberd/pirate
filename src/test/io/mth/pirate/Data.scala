package io.mth.pirate

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Gen.{frequency, choose, listOfN, value, oneOf}

import scalaz._
import Scalaz._
import Parser._

object Data {
  implicit def ArbitraryPirate: Arbitrary[Command[Map[String, String]]] = {
    val c1 = for {
      name <- arbitrary[String]
      flags <- arbitrary[Flag[Map[String, String]]]
    } yield Command.commandfpl(name, flags.toList, List())

    val c2 = for {
      name <- arbitrary[String]
      flags <- arbitrary[Flag[Map[String, String]]]
    } yield Command.commandfpl(name, flags.toList, List())

    Arbitrary(frequency((1, c1), (1, c2)))
  }

  object CannedFlags {
    val f = Flag.full[Map[String, String]]('a', "--aa", "a/aa")(_ + (("a", "")))
    val f1 = Flag.full1[Map[String, String]]('b', "--bb", "b/bb", "B")((m, s) => m + (("b", s)))
    val s = Flag.short[Map[String, String]]('c', "c")(_ + (("c", "")))
    val s1 = Flag.short1[Map[String, String]]('d', "d", "D")((m, s) => m + (("d", s)))
    val l = Flag.long[Map[String, String]]("--ee", "ee")(_ + (("e", "")))
    val l1 = Flag.long1[Map[String, String]]("--ff", "ff", "F")((m, s) => m + (("f", s)))
    val c1 = f <|> s <|> l
    val c2 = f1 <|> s1 <|> l1
    val c3 = f <|> f1
    val c4 = s <|> s1
    val c5 = l <|> l1
    val c6 = f <|> f1 <|> s <|> s1 <|> l <|> l1
  }

  implicit def ArbitraryFlag: Arbitrary[Flag[Map[String, String]]] = {
    import CannedFlags._
    
    Arbitrary(frequency(
      (1, value(f)),
      (1, value(f1)),
      (1, value(s)),
      (1, value(s1)),
      (1, value(l)),
      (1, value(l1)),
      (1, value(c1)),
      (1, value(c2)),
      (1, value(c3)),
      (1, value(c4)),
      (1, value(c5)),
      (1, value(c6))))
  }
}