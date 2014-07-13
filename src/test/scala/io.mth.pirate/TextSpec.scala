package io.mth.pirate

import org.scalacheck._, Prop.forAll

class TextSpec extends test.Spec { def is = s2"""

  Text Properties
  ===============

  all spaces                                      $spaces
  correct length                                  $length
  wrap no longer than width                       $width
  wrap no longer than width + ident               $ident
  never lose content                              $safe

"""

  import Text._

  private val shortInt = Gen.choose(0, 1000)
  private val longLine = for {i <- Gen.choose(70, 1000); cs <- Gen.listOfN(i, Arbitrary.arbChar.arbitrary)} yield cs.mkString

  def spaces = forAll(shortInt)(i => space(i).forall(_ == ' '))

  def length = forAll(shortInt)(i => space(i).length == i )

  def width = forAll(longLine)(s => wrap(s, 80, 0).split('\n').forall(_.length <= 80))

  def ident = forAll(longLine)(s => wrap(s, 80, 10).split('\n').forall(_.length <= 90))

  def safe = forAll(longLine)(s => drains(s, wrap(s, 80, 0)))

  def drains(orig: String, modded: String): Boolean = {
    var i = 0
    modded.foreach(c =>
      if (i < orig.length && (orig.charAt(i) == c || (orig.charAt(i) == ' ' && c == '\n')))
        i += 1
    )
    orig.length == i
  }
}
