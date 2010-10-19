package io.mth.pirate

import scalaz._
import Scalaz._

trait Parser[+A] {
  def parse(args: List[String]): Validation[String, (List[String], A)]

  // FIX work out why I can't point free these without type annotating up the wazoo.....
  def lift[B](f: A => B) : Parser[B] = Parser.lift(this)(f)

  def lift2[B, C](p: Parser[B])(f: A => B => C) = Parser.lift2(this)(p)(f)
}

object Parser {
  def value[A](v: A) = new Parser[A] {
    def parse(args: List[String]) = Success((args, v))
  }

  def failed[A](msg: String) = new Parser[A] {
    def parse(args: List[String]) = Failure(msg)
  }

  def string = new Parser[String] {
    def parse(args: List[String]) = args match {
      case Nil => Failure("Unexpected end of input.")
      case head :: tail => Success((tail, head))
    }
  }

  def bind[A, B](p: Parser[A], f: A => Parser[B]) = new Parser[B] {
    def parse(args: List[String]) = p.parse(args) match {
      case Success((rest, value)) => f(value).parse(rest)
      case Failure(error) => Failure(error)
    }
  }

  def oneOf[A](p1: Parser[A], p2: Parser[A]) = new Parser[A] {
    def parse(args: List[String]) = p1.parse(args) match {
      case Success((rest, value)) => Success((rest, value))
      case Failure(error) => p2.parse(args)
    }
  }

  // FIX should be using fold... ?
  def anyOneOf[A](ps: List[Parser[A]]): Parser[A] = ps.reduceRight((p: Parser[A], acc: Parser[A]) => oneOf(acc, p))

  def sequence[A](ps: List[Parser[A]]): Parser[List[A]] =
    if (ps.empty) value(List[A]())
    else lift2(ps.head)(sequence(ps.tail))(x => xs => x :: xs)

  def satisfy(pred: String => Boolean) = bind(string, (s: String) => if (pred(s)) value(s) else failed("Unexpected string [" + s + "]"))

  def is(s: String) = satisfy(_ == s)

  def list[A](p: Parser[A]): Parser[List[A]] = oneOf(many(p), value(List[A]()))

  def many[A](p: Parser[A]): Parser[List[A]] = lift2(p)(list(p))(x => xs => x :: xs)

  def lift[A, B](p: Parser[A])(f: A => B) = bind(p, (a: A) => value(f(a)))

  def lift2[A, B, C](pa: Parser[A])(pb: Parser[B])(f: A => B => C) =  bind(pa, (a: A) => lift(pb)(f(a)))

  //----------------------------------------------------------------------------------------------



  def flag(short: String, long: String) =  oneOf(is(short), is(long))

  def endargs = is("--")

  // FIX can this be defined in terms of many/list
  def flagParser[A](p: Parser[A]): Parser[List[A]] = new Parser[List[A]] {
    def parse(args: List[String]) = endargs.parse(args) match {
      case Success((rest, value)) => Failure("End of flags reached")
      case Failure(error) => oneOf(flagParserx(p), value(List[A]())).parse(args)
    }
  }

  def flagParserx[A](p: Parser[A]): Parser[List[A]] = lift2(p)(flagParser(p))(x => xs => x :: xs)

  def commandline[A](flags: List[Parser[A => A]], positional: List[Parser[A => A]]) = new Parser[List[A => A]] {
    def parse(args: List[String]) = flagParser(anyOneOf(flags)).parse(args) match {
      case Success((rest, value)) => sequence(positional).parse(rest) match {
         case Success((rest2, value2)) => Success(rest2, value ::: value2)
         case Failure(error) => Failure(error)
      }
      case Failure(error) => sequence(positional).parse(args)
    }
  }

  def flatCommandline[A](flags: List[Parser[A => A]], positional: List[Parser[A => A]]) =
    lift(commandline(flags, positional))(_.foldRight((a: A) => a)(_.compose(_)))
}
