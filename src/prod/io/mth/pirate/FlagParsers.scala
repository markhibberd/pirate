package io.mth.pirate

object FlagParsers {
  import scalaz._
  import Scalaz._
  import Parser._

  def endargs = is("--")

  def flag0(short: Option[Char], long: Option[String]): Parser[String] =  (short, long) match {
    case (Some(s), Some(l)) => is("-" + s) | is(l)
    case (Some(s), None) => is("-" + s)
    case (None, Some(l)) => is(l)
    case (None, None) => throw new IllegalStateException("Flag must have at least one of a short of long form.")
  }

  def flag1(short: Option[Char], long: Option[String]): Parser[String] =
    flag0(short, long) >>=| string

  def positional1 = string

  def positionalN = string*

  def empty[A]: Parser[List[A]]  = value(List())

  def flagParser[A](p: Parser[A]): Parser[List[A]] = 
    (endargs >>=| empty[A]) | (p.lift2(flagParser(p))(_::_) | empty[A])

  // FIX naive (and ugly as) implementation of positionals, should be able to have variable arity at any (single) position.
  def commandline[A](flags: List[Parser[A => A]], positional: List[Parser[A => A]]): Parser[A => A] =
    (flagParser(choiceN(flags)).lift2(positional.sequence)(_:::_)).map(_.foldRight(identity[A]_)(_ compose _))

  def commandlinex[A](flags: Parser[A => A], positional: Parser[A => A]): Parser[A => A] =
    flags.lift2(positional)(_ compose _)
}
