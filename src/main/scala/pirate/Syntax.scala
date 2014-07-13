package pirate

/**
 * A fairly dirty but unobtrusive way of consuming Parse as a DSL.
 *
 * eg.
 * {{{
 *   flag[Int]('x') |||
 *     flag[String]("something") |||
 *     flag[String]('e' -> "example")
 * }}}
 *
 * Instead of:
 *
 * {{{
 *   flag[Int](ShortName('x')) |||
 *     flag[String](LongName("something")) |||
 *     flag[String](BothName('e', "example"))
 * }}}
 */
trait Syntax {

  implicit def ShortNameSyntax(s: Char) = ShortName(s)
  implicit def LongNameSyntax(l: String) = LongName(l)
  implicit def BothNameSyntax(b: (Char, String)) = BothName(b._1, b._2)
}
