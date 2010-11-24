package io.mth.pirate

import scalaz._
import Scalaz._

/**
 * Minimal parser combinator library, pimped with scalaz MA[M[_], A].
 */
sealed trait Parser[A] {
  import Parser._

  def parse(args: List[String]): Validation[String, (List[String], A)]

  def |(p: => Parser[A]): Parser[A] = this <+> p
  
  def list: Parser[List[A]] = many1 | value(List[A]())

  def many1: Parser[List[A]]  = lift2(list)(_::_)

  def rep(n: Int) = this.replicateM[List](n)

  def * = list

  def + = many1

  // FIX because scalaz impl of lift2 <**> is not lazy -- need to submit a patch for this...
  def lift[B](f: A => B): Parser[B] =
     this >>= { (a: A) => value(f(a)) }

  def lift2[B, C](p: => Parser[B])(f: (A, B) => C): Parser[C] =
    this >>= { (a: A) => p.lift(f(a, _)) }
}

object Parser {
  def parser[A](f: List[String] => Validation[String, (List[String], A)]): Parser[A] = new Parser[A] {
    def parse(args: List[String]) = f(args)
  }

  def value[A](v: A): Parser[A] = parser(Success(_, v))

  def failure[A](msg: String): Parser[A] = parser(_ => Failure(msg))


  implicit def ParserFunctor: Functor[Parser] = new Functor[Parser] {
    def fmap[A, B](a: Parser[A], f: A => B) = parser(a.parse(_) map (_ map f))
  }

  implicit def ParserPure: Pure[Parser] = new Pure[Parser] {
    def pure[A](a: => A) = value(a)
  }

  implicit def ParserBind: Bind[Parser] = new Bind[Parser] {
    def bind[A, B](p: Parser[A], f: A => Parser[B]) = parser(p.parse(_) match {
      case Success((rest, value)) => f(value).parse(rest)
      case Failure(error) => Failure(error)
    })
  }

  implicit def ParserApply: Apply[Parser] = new Apply[Parser] {
    def apply[A, B](f: Parser[A => B], a: Parser[A]) = f flatMap { k => a map (k(_)) }
  }

  implicit def ParserZero[A]: Zero[Parser[A]] = zero(failure[A]("empty"))

  implicit def ParserPlus[A]: Plus[Parser] = new Plus[Parser] {
    def plus[A](p1: Parser[A], p2: => Parser[A]) =
      parser((args: List[String]) => p1.parse(args) match {
        case Success((rest, value)) => Success((rest, value))
        case Failure(_) => p2.parse(args)
      })
  }

  def string: Parser[String] =
    parser({
      case Nil => Failure("Unexpected end of input.")
      case head :: tail => Success((tail, head))
    })
    
  def satisfy(pred: String => Boolean): Parser[String] =
    string >>=
      (s => if (pred(s)) value(s) else failure("unexpected value [" + s + "]"))

  def is(s: String): Parser[String] =
    satisfy(_ == s)

  def choiceN[A](ps: List[Parser[A]]): Parser[A] =
    ps.foldRight(failure[A]("empty"))(_ | _)
}
