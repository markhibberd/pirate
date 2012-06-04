package io.mth.pirate

/**
 * Parsers for command line arguments.
 */
object CommandParsers {
  import scalaz._
  import Scalaz._
  import Parser._

  /**
   * Constructs a parser for short flag notation.
   */
  def short(s: Char) = is("-" + s)

  /**
   * Constructs a parser for long flag notation
   */
  def long(l: String) = is("--" + l)

  /**
   * Constructs a parser for either short or long notation.
   */
  def both(s: Char, l: String) = short(s) | long(l)

  /**
   * Constructs a parser for a single flag.
   */
  def flag[A](f: Flag[A]): Parser[A => A] = f.fold(
    (s, l, d, f) => both(s, l) map (_ => f),
    (s, d, f) => short(s) map (_ => f),
    (l, d, f) => long(l) map (_ => f),
    (s, l, d, m, f) => (both(s, l) >> string) map (v => f(_, v)),
    (s, d, m, f) => (short(s) >> string) map (v => f(_, v)),
    (l, d, m, f) =>  (long(l) >> string) map (v => f(_, v))
  )

  /**
   * A parser that consumes no input and produces an empty list.
   */
  def empty[A]: Parser[List[A]] = value(List())

  /**
   * Constructs a parser that will repeat until either the
   * parser fails or the end of flags marker is reached.
   */
  def untilEndOfFlags[A](p: Parser[A]): Parser[List[A]] =
    (is("--") map (_ => List[A]())) |
       (p.lift2(untilEndOfFlags(p))(_::_) | empty[A])

  /**
   * Constructs a parser that will match any flag.
   */
  def anyFlag[A](f: Flags[A]) = choiceN(f.toList.map(flag(_)))

  /**
   * Constructs a parser that will consume all flags.
   */
  def flags[A](f: Flags[A]) = untilEndOfFlags(anyFlag(f))

  /**
   * Constructs a parser for a positioanl parameter.
   *
   * TODO try to detect ambiguities?
   */
  def positional[A](p: Positional[A]): Parser[A => A] = p.fold(
    (m, f) => string map (v => f(_, v)),
    (n, m, f) => (for (_ <- 1 to n) yield string).toList.sequence map (v => f(_, v)),
    (m, f) => (string*) map (v => f(_, v)),
    (m, f) => (string+) map (v => f(_, v))
  )

  /**
   * Constructs a parser that will consume all positional parameters.
   */
  def positionals[A](p: Positionals[A]): Parser[List[A => A]] =
    p.toList.map(positional(_)).sequence

  /**
   * Constructs a parser that will consume all flags then all positional
   * parameters.
   */
  def commandline[A](f: Flags[A], p: Positionals[A]) =
    flags(f).lift2(positionals(p))(_:::_).map(_.foldRight(identity[A]_)(_ compose _))
}
