package io.mth.pirate

class TextSpec extends test.Spec { def is = s2"""

  Text Properties
  ===============

  all spaces                                      $spaces
  correct length                                  $length
  wrap no longet than width                       $width
  wrap no longer than width + ident               $ident
  never lose content                              $safe

"""

  import Text._

  def spaces = prop((i: Int) => (i >= 0 && i <= 1000) ==> {
    space(i).forall(_ == ' ') })

  def length = prop((i: Int) => (i >= 0 && i <= 1000) ==> {
    space(i).length == i })

  def width = prop((s: String) => s.length > 70 ==> {
    wrap(s, 80, 0).split('\n').forall(_.length <= 80) })

  def ident = prop((s: String) => s.length > 70 ==> {
    wrap(s, 80, 10).split('\n').forall(_.length <= 90) })

  def safe = prop((s: String) => s.length > 70 ==> {
    drains(s, wrap(s, 80, 0)) })

  def drains(orig: String, modded: String): Boolean = {
    val sb = new StringBuilder(orig)
    modded.foreach(c =>
      if (sb.length() > 0 && sb.charAt(0) == c)
        sb.deleteCharAt(0)
    )
    sb.length == 0
  }
}
