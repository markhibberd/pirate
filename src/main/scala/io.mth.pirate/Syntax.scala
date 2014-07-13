package io.mth.pirate

/**
 * A fairly dirty but unobtrusive way of consuming Parse as a DSL.
 *
 * eg.
 * {{{
 *   option[Int]('x') |||
 *     option[String]("something") |||
 *     option[String]('e' -> "example")
 * }}}
 *
 * Instead of:
 *
 * {{{
 *   option[Int](Short('x')) |||
 *     option[String](Long("something")) |||
 *     option[String](Both('e', "example"))
 * }}}
 */
trait Syntax {

  implicit def ShortNameSyntax(s: Char) = Short(s)
  implicit def LongNameSyntax(l: String) = Long(l)
  implicit def BothNameSyntax(b: (Char, String)) = Both(b._1, b._2)
}
