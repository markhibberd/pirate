package scala.io.mth.pirate.test

import org.scalacheck.Prop._
import org.scalacheck.Properties

object ParserTest extends Properties("Parser") {
  property("usage string") = 
       forAll((s: String) =>
            true)
}