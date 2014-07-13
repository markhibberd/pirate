package io.mth.pirate

import io.mth.pirate.test.Arbitraries._

class TextSpec extends test.Spec { def is = s2"""

  Text Properties
  ===============

  all spaces                                      $spaces
  correct length                                  $length
  wrap no longer than width                       $width
  wrap no longer than width + indent              $indent
  never lose content                              $safe

"""

  import Text._

  def spaces = prop((n: SmallInt) => space(n.value).forall(_ == ' '))

  def length = prop((n: SmallInt) => space(n.value).length == n.value)

  def width = prop((l: LongLine) => wrap(l.value, 80, 0).split('\n').forall(_.length <= 80))

  def indent = prop((l: LongLine) => wrap(l.value, 80, 10).split('\n').forall(_.length <= 90))

  def safe = prop((l: LongLine) => drains(l.value, wrap(l.value, 80, 0)))

  def drains(orig: String, modded: String): Boolean = {
    var i = 0
    modded.foreach(c =>
      if (i < orig.length && (orig.charAt(i) == c || (orig.charAt(i) == ' ' && c == '\n')))
        i += 1
    )
    orig.length == i
  }
}
