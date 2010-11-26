package io.mth.pirate

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import org.scalacheck.Gen.{frequency, choose, listOfN, value, oneOf}

import scalaz._
import Scalaz._
import Parser._

object Data {
  object CannedFlags {
    val f = Flag.flag[Map[String, String]]('a', "aa", "a/aa")(_ + (("a", "")))
    val f1 = Flag.flag1[Map[String, String]]('b', "bb", "b/bb", "B")((m, s) => m + (("b", s)))
    val s = Flag.short[Map[String, String]]('c', "cc")(_ + (("c", "")))
    val s1 = Flag.short1[Map[String, String]]('d', "d", "D")((m, s) => m + (("d", s)))
    val l = Flag.long[Map[String, String]]("ee", "ee")(_ + (("e", "")))
    val l1 = Flag.long1[Map[String, String]]("ff", "ff", "F")((m, s) => m + (("f", s)))
    val c1 = f <|> s <|> l
    val c2 = f1 <|> s1 <|> l1
    val c3 = f <|> f1
    val c4 = s <|> s1
    val c5 = l <|> l1
    val c6 = f <|> f1 <|> s <|> s1 <|> l <|> l1
  }


}