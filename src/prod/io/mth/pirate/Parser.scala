package io.mth.pirate

import scalaz._
import Scalaz._
import Monad._

// FIX This is a simple (poor mans) combinator library - not very generic.
// FIX Wire in scalaz.Monad and kill the dupe.
trait Parser[+A] {
  def parse(args: List[String]): Validation[String, (List[String], A)]

  def lift[B](f: A => B) = Parser.lift(this)(f)

  def lift2[B, C](p: => Parser[B])(f: (A, B) => C) = Parser.lift2(this)(p)(f)

  def >>=[B](f: A => Parser[B]) = Parser.bind(this, f)

  def >>>[B](f: => Parser[B]) = Parser.bind(this, (_:A) => f)

  def |[B >: A](p: => Parser[B]) = Parser.choice(this, p)

  def * = Parser.list(this)

  def + = Parser.many1(this)

  def rep(n: Int) = Parser.thisMany(n, this)
}

object Parser {
  def value[A](v: A) = new Parser[A] {
    def parse(args: List[String]) = Success((args, v))
  }

  def bind[A, B](p: Parser[A], f: A => Parser[B]) = new Parser[B] {
    def parse(args: List[String]) = p.parse(args) match {
      case Success((rest, value)) => f(value).parse(rest)
      case Failure(error) => Failure(error)
    }
  }

  def failed[A](msg: String): Parser[A] = new Parser[A] {
    def parse(args: List[String]) = Failure(msg)
  }

  def zerox[A] = failed[A]("zero")

  def string = new Parser[String] {
    def parse(args: List[String]) = args match {
      case Nil => Failure("Unexpected end of input.")
      case head :: tail => Success((tail, head))
    }
  }

  def satisfyFor(pred: String => Boolean)(s: String) = 
    if (pred(s))
      value(s)
    else
      failed("Unexpected string [" + s + "]")

  def satisfy(pred: String => Boolean) =
    string >>= satisfyFor(pred)

  def is(s: String) =
    satisfy(_ == s)
  
  def choice[A](p1: Parser[A], p2: Parser[A]) = new Parser[A] {
    def parse(args: List[String]) = p1.parse(args) match {
      case Success((rest, value)) => Success((rest, value))
      case Failure(error) => p2.parse(args)
    }
  }

  def choiceN[A](ps: List[Parser[A]]): Parser[A] =
    ps.foldRight(zerox[A])(_ | _)

  def sequence[A](ps: List[Parser[A]]): Parser[List[A]] =
    if (ps.isEmpty)
      value(List[A]())
    else
      ps.head.lift2(sequence(ps.tail))(_::_)

  def thisMany[A](n: Int, p1: Parser[A]) =
    sequence((for (i <- 1 to n) yield p1).toList)

  def list[A](p: Parser[A]): Parser[List[A]] =
      many1(p) | value(List[A]())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    p.lift2(list(p))(_::_)

  def lift[A, B](p: => Parser[A])(f: A => B) =
    p >>= { (a: A) => value(f(a)) }

  def lift2[A, B, C](pa: => Parser[A])(pb: => Parser[B])(f: (A, B) => C) =
    pa >>= { (a: A) => pb.lift(f(a, _)) } 

  // FIX Complete and use this properly to remove some of the dupe ^^^^
  implicit def ParserPure: Pure[Parser] = new Pure[Parser] {
    def pure[A](a: => A) = value(a)
  }

  implicit def ParserBind: Bind[Parser] = new Bind[Parser] {
    def bind[A, B](p: Parser[A], f: A => Parser[B]) = p >>= f
  }

  implicit def ParserMonad[A] = monad[Parser](ParserBind, ParserPure)

  implicit def ParserZero[A]: Zero[Parser[A]] = zero(zerox)

  implicit def ParserFunctor: Functor[Parser] = new Functor[Parser] {
    def fmap[A, B](a: Parser[A], f: A => B) = a.lift(f)
  }
}
