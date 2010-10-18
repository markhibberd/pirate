package io.mth.pirate

import scalaz._
import Scalaz._

trait PirateParser[+A] {
  import PirateParser._

  def parse(args: List[String]): Validation[String, (List[String], A)]
  
  def lift[B](f: A => B): PirateParser[B] = bind(this, (a: A) => value(f(a)))
}

object PirateParser {
  def value[A](v: A) = new PirateParser[A] {
    def parse(args: List[String]) = Success((args, v))
  }

  def failed[A](msg: String) = new PirateParser[A] {
    def parse(args: List[String]) = Failure(msg)
  }

  def arg = new PirateParser[String] {
    def parse(args: List[String]) = if (args.empty) Failure("Unexpected end of input.") else Success((args.tail, args.head))
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

  def anyOf[A](ps: List[PirateParser[A]]): PirateParser[A] = ps.reduceRight((p: PirateParser[A], acc: PirateParser[A]) => oneOf(acc, p))

  def sequence[A](ps: List[PirateParser[A]]): PirateParser[List[A]] =
    if (ps.empty) value(List[A]())
    else bind(ps.head, (a: A) =>
         bind(sequence(ps.tail), (aa: List[A]) =>
         value(a :: aa)))

  def satisfy(pred: String => Boolean) = bind(arg, (s: String) => if (pred(s)) value(s) else failed("Unexpected string [" + s + "]"))

  def is(s: String) = satisfy(_ == s)

  def flag(short: String, long: String) =  oneOf(is(short), is(long))

  def endargs() = is("--")

  def list[A](p: PirateParser[A]): PirateParser[List[A]] = oneOf(many(p), value(List[A]()))

  def many[A](p: PirateParser[A]): PirateParser[List[A]] = bind(p, (a: A) =>
                                                           bind(list(p), (aa: List[A]) =>
                                                           value(a :: aa)))



  // FIX can this be defined in terms of many/list
  // FIX lossy end of flags vs hard fail (i.e. unexpected flag)
  def flagParser[A](p: PirateParser[A]): PirateParser[List[A]] = new PirateParser[List[A]] {
    def parse(args: List[String]) = is("--").parse(args) match {
      case Success((rest, value)) => Failure("End of flags reached")
      case Failure(error) => oneOf(flagParserx(p), value(List[A]())).parse(args)
    }
  }

  def flagParserx[A](p: PirateParser[A]): PirateParser[List[A]] = bind(p, (a: A) =>
                                                                  bind(flagParser(p), (aa: List[A]) =>
                                                                  value(a :: aa)))

  def commandline[A](flags: List[PirateParser[A => A]], positional: List[PirateParser[A => A]]) = new PirateParser[List[A => A]] {
    def parse(args: List[String]) = flagParser(anyOf(flags)).parse(args) match {
      case Success((rest, value)) => sequence(positional).parse(rest) match {
         case Success((rest2, value2)) => Success(rest2, value ::: value2)
         case Failure(error) => Failure(error)
      }
      case Failure(error) => sequence(positional).parse(args)
    }
  }

  def flatCommandline[A](flags: List[PirateParser[A => A]], positional: List[PirateParser[A => A]]) =
    commandline(flags, positional).lift(_.foldRight((a: A) => a)(_.compose(_)))
}
