package io.mth.pirate

object FlagParsers {
  import Parser._

  def endargs = is("--")

  def flag0(short: Option[Char], long: Option[String]) =  (short, long) match {
    case (Some(s), Some(l)) => is("-" + s) | is(l)
    case (Some(s), None) => is("-" + s)
    case (None, Some(l)) => is(l)
    case (None, None) => throw new IllegalStateException("Flag must have at least one of a short of long form.")
  }

  def flag1(short: Option[Char], long: Option[String]) =
    flag0(short, long) >>> string

  def positional1 = string

  def positionalN = string*

  def flagParser[A](p: Parser[A]): Parser[List[A]] =
    (endargs >>> value(List())) | (p.lift2(flagParser(p))(_::_) | value(List()))

  // FIX naive (and ugly as) implementation of positionals, should be able to have variable arity at any (single) position.
  def commandline[A](flags: List[Parser[A => A]], positional: List[Parser[A => A]]): Parser[A => A] =
    ((flagParser(choiceN(flags)).lift2(sequence(positional))(_:::_)) | sequence(positional)).lift(_.foldRight(id[A]_)(_ compose _))

  def id[A](a: A) = a
}