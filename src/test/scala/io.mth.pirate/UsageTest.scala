package io.mth.pirate

import org.scalacheck.Prop._
import org.scalacheck.Properties

object UsageTest extends Properties("Usage") {
  property("place holder -- test me you dodgy bastard") = 
       forAll((s: String) =>
            true)
}