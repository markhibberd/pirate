package io.mth.pirate

import scalaz._
import Scalaz._

trait PirateParser[+A] {
  def parse(args: List[String]): Validation[String, (List[String], A)]

  // FIX work out why I can't point free these without type annotating up the wazoo.....
  def lift[B]: PirateParser[B] = PirateParser.lift(this)(f)

  def lift2[B, C](p: PirateParser[B])(f: A => B => C) = PirateParser.lift2(this)(p)(f)
}

object PirateParser {
  def value[A](v: A) = new PirateParser[A] {
    def parse(args: List[String]) = Success((args, v))
  }

  def failed[A](msg: String) = new PirateParser[A] {
    def parse(args: List[String]) = Failure(msg)
  }

  def string = new PirateParser[String] {
    def parse(args: List[String]) = args match {
      case Nil => Failure("Unexpected end of input.")
      case head :: tail => Success((tail, head))
    }
  }

  def bind[A, B](p: PirateParser[A], f: A => PirateParser[B]) = new PirateParser[B] {
    def parse(args: List[String]) = p.parse(args) match {
      case Success((rest, value)) => f(value).parse(rest)
      case Failure(error) => Failure(error)
    }
  }

  def oneOf[A](p1: PirateParser[A], p2: PirateParser[A]) = new PirateParser[A] {
    def parse(args: List[String]) = p1.parse(args) match {
      case Success((rest, value)) => Success((rest, value))
      case Failure(error) => p2.parse(args)
    }
  }

  // FIX should be using fold... ?
  def anyOneOf[A](ps: List[PirateParser[A]]): PirateParser[A] = ps.reduceRight((p: PirateParser[A], acc: PirateParser[A]) => oneOf(acc, p))

  def sequence[A](ps: List[PirateParser[A]]): PirateParser[List[A]] =
    if (ps.empty) value(List[A]())
    else lift2(ps.head)(sequence(ps.tail))(x => xs => x :: xs)

  def satisfy(pred: String => Boolean) = bind(string, (s: String) => if (pred(s)) value(s) else failed("Unexpected string [" + s + "]"))

  def is(s: String) = satisfy(_ == s)

  def list[A](p: PirateParser[A]): PirateParser[List[A]] = oneOf(many(p), value(List[A]()))

  def many[A](p: PirateParser[A]): PirateParser[List[A]] = lift2(p)(list(p))(x => xs => x :: xs)

  def lift[A, B](p: PirateParser[A])(f: A => B) = bind(p, (a: A) => value(f(a)))

  def lift2[A, B, C](pa: PirateParser[A])(pb: PirateParser[B])(f: A => B => C) =  bind(pa, (a: A) => lift(pb)(f(a)))

  //----------------------------------------------------------------------------------------------



  def flag(short: String, long: String) =  oneOf(is(short), is(long))

  def endargs = is("--")

  // FIX can this be defined in terms of many/list
  // FIX lossy end of flags vs hard fail (i.e. unexpected flag)
  def flagParser[A](p: PirateParser[A]): PirateParser[List[A]] = new PirateParser[List[A]] {
    def parse(args: List[String]) = endargs.parse(args) match {
      case Success((rest, value)) => Failure("End of flags reached")
      case Failure(error) => oneOf(flagParserx(p), value(List[A]())).parse(args)
    }
  }

  def flagParserx[A](p: PirateParser[A]): PirateParser[List[A]] = lift2(p)(flagParser(p))(x => xs => x :: xs)

  def commandline[A](flags: List[PirateParser[A => A]], positional: List[PirateParser[A => A]]) = new PirateParser[List[A => A]] {
    def parse(args: List[String]) = flagParser(anyOneOf(flags)).parse(args) match {
      case Success((rest, value)) => sequence(positional).parse(rest) match {
         case Success((rest2, value2)) => Success(rest2, value ::: value2)
         case Failure(error) => Failure(error)
      }
      case Failure(error) => sequence(positional).parse(args)
    }
  }

  def flatCommandline[A](flags: List[PirateParser[A => A]], positional: List[PirateParser[A => A]]) =
    lift(commandline(flags, positional))(_.foldRight((a: A) => a)(_.compose(_)))
}
