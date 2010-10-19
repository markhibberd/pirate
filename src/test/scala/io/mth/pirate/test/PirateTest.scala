package scala.io.mth.pirate.test



import org.scalacheck.Prop._
import org.scalacheck.Properties


object PirateTest extends Properties("Pirate") {
  property("hello") =
        forAll((s: String) =>
            true)
}