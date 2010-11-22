package io.mth.pirate

import scalaz._
import Scalaz._

object FlagParsers {
  import Parser._

  def flag0(short: Option[Char], long: Option[String]) =  (short, long) match {
    case (Some(s), Some(l)) => is("-" + s) | is(l)
    case (Some(s), None) => is("-" + s)
    case (None, Some(l)) => is(l)
    case (None, None) => throw new IllegalStateException("Flag must have at least one of a short of long form.")
  }

  def flag1(short: Option[Char], long: Option[String]) =  (short, long) match {
    case (Some(s), Some(l)) => (is("-" + s) | is(l)) >>> string
    case (Some(s), None) => is("-" + s) >>> string
    case (None, Some(l)) => is(l) >>> string
    case (None, None) => throw new IllegalStateException("Flag must have at least one of a short of long form.")
  }

  def positional1 = string

  def positionalN = string*

  def endargs = is("--")

  def flagParser[A](p: Parser[A]): Parser[List[A]] = new Parser[List[A]] {
    def parse(args: List[String]) = endargs.parse(args) match {
      case Success((rest, value)) => Success((rest, List()))
      case Failure(error) => (flagParserx(p) | value(List())).parse(args)
    }
  }

  def flagParserx[A](p: Parser[A]): Parser[List[A]] = p.lift2(flagParser(p))((x, xs) => x :: xs)

  def commandline[A](flags: List[Parser[A => A]], positional: List[Parser[A => A]]) = new Parser[List[A => A]] {
    def parse(args: List[String]) = flagParser(choiceN(flags)).parse(args) match {
      case Success((rest, value)) => sequence(positional).parse(rest) match {
         case Success((rest2, value2)) => Success(rest2, value ::: value2)
         case Failure(error) => Failure(error)
      }
      case Failure(error) => sequence(positional).parse(args)
    }
  }

  def flatCommandline[A](flags: List[Parser[A => A]], positional: List[Parser[A => A]]) =
    commandline(flags, positional).lift(_.foldRight((a: A) => a)(_.compose(_)))
}