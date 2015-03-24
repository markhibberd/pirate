package pirate

import pirate.spec.Arbitraries._

class TextSpec extends spec.Spec { def is = s2"""

  Text Properties
  ===============

  all spaces                                      $spaces
  correct length                                  $length
  wrap no longer than width                       $width
  wrap no longer than width + indent              $indent
  handle negative widths                          $negativeWidth
  never lose content                              $safe
  text with new lines has proper gutter           $gutter

"""

  import Text._

  def spaces = prop((n: SmallInt) => space(n.value).forall(_ == ' '))

  def length = prop((n: SmallInt) => space(n.value).length == n.value)

  def width = prop((l: LongLine) => wrap("", 0)(l.value, 80, 0).split('\n').forall(_.length <= 80))

  def indent = prop((l: LongLine) => wrap("", 0)(l.value, 80, 10).split('\n').forall(_.length <= 90))

  def negativeWidth = prop((l: LongLine, s: SmallInt) =>
    wrap("", 0)(l.value, 50 - s.value, 10).split('\n').forall(_.length <= 90)
  )

  def safe = prop((l: LongLine) => drains(l.value, wrap("", 0)(l.value, 80, 0)))

  def gutter = prop((ls: (List5[LongLine])) =>
    wrap("", 0)(ls.value.map(_.value).mkString("\n"), 80, 10).split('\n').map(_.take(10)).mkString("").trim ==== "")

  def drains(orig: String, modded: String): Boolean = {
    var i = 0
    modded.foreach(c =>
      if (i < orig.length && (orig.charAt(i) == c || (orig.charAt(i) == ' ' && c == '\n')))
        i += 1
    )
    orig.length == i
  }
}
