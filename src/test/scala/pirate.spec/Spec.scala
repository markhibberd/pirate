package pirate.spec

import scalaz._, Scalaz._
import org.specs2._, matcher._, specification._
import org.scalacheck.{Arbitrary, Gen, Prop, Properties}, Prop.forAll

abstract class Spec extends Specification with ScalaCheck
