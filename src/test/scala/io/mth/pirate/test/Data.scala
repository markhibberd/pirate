package scala.io.mth.pirate.test

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary
import io.mth.pirate.{Flag, Pirate}
import org.scalacheck.Gen.{frequency, choose, listOfN, value, oneOf}

object Data {
  implicit def ArbitraryPirate: Arbitrary[Pirate[Map[String, String]]] = {
    val c1 = for {
      name <- arbitrary[String]
      flags <- arbitrary[Flag[Map[String, String]]]
    } yield Pirate.command(name, flags)

    val c2 = for {
      name <- arbitrary[String]
      flags <- arbitrary[Flag[Map[String, String]]]
    } yield Pirate.command(name, flags)

    val c3 = for {
      x <- c1
      y <- c2
    } yield x >>= y

    Arbitrary(frequency((1, c1), (1, c2), (1, c3)))
  }

  object CannedFlags {
    val f = Flag.full[Map[String, String]]('a', "--aa", "a/aa")((m: Map[String, String]) => m + (("a", "")))
    val f1 = Flag.full1[Map[String, String]]('b', "--bb", "b/bb", "B")((s: String) => (m: Map[String, String]) => m + (("b", s)))
    val s = Flag.short[Map[String, String]]('c', "c")((m: Map[String, String]) => m + (("c", "")))
    val s1 = Flag.short1[Map[String, String]]('d', "d", "D")((s: String) => (m: Map[String, String]) => m + (("d", s)))
    val l = Flag.long[Map[String, String]]("--ee", "ee")((m: Map[String, String]) => m + (("e", "")))
    val l1 = Flag.long1[Map[String, String]]("--ff", "ff", "F")((s: String) => (m: Map[String, String]) => m + (("f", s)))
    val c1 = f >>= s >>= l
    val c2 = f1 >>= s1 >>= l1
    val c3 = f >>= f1
    val c4 = s >>= s1
    val c5 = l >>= l1
    val c6 = f >>= f1 >>= s >>= s1 >>= l >>= l1
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