package io.mth.pirate

import scalaz._
import Scalaz._

/**
 * Minimal parser combinator library, pimped with scalaz MA[M[_], A].
 */
sealed trait Parser[A] {
  import Parser._

  def parse(args: List[String]): Validation[String, (List[String], A)]

  /**
   * Choice combinator, either this or that.
   */
  def |(p: => Parser[A]): Parser[A] = this <+> p

  /**
   * List combinator, zero or more times.
   */
  def list: Parser[List[A]] = many1 | value(List[A]())

  /**
   * List combinator, one or more times.
   */
  def many1: Parser[List[A]]  = lift2(list)(_::_)

  /**
   * Fixed repitition combinator, exactly n times.
   */
  def rep(n: Int) = this.replicateM[List](n)

  /**
   * Symbolic representation of list combinator, zero or more times.
   *
   * See: list.
   */
  def * = list

  /**
   * Symbolic representation of list combinator, one or more times.
   *
   * See many1.
   */
  def + = many1

  /**
   * Lazy implementation of lift 1 / map.
   *
   * FIX used only by lazy implementation of lift 2, due to a
   *     deficiency in the scalaz api. Remove once scalaz is
   *     improved.
   */
  def lift[B](f: A => B): Parser[B] =
     this >>= { (a: A) => value(f(a)) }

  /**
   * Lazy implementation of lift 2.
   *
   * FIX this implementation is only required as scalaz
   *     implementation - <**> - is strict. Remove once
   *     scalaz is improved.
   */
  def lift2[B, C](p: => Parser[B])(f: (A, B) => C): Parser[C] =
    this >>= { (a: A) => p.lift(f(a, _)) }
}

object Parser {
  /**
   * Type constructor for parser.
   */
  def parser[A](f: List[String] => Validation[String, (List[String], A)]): Parser[A] = new Parser[A] {
    def parse(args: List[String]) = f(args)
  }

  /**
   * Value/Pure implementation of Parser monad.
   */
  def value[A](v: A): Parser[A] = parser(Success(_, v))

  /**
   * Failure/Zero implementation of Parser.
   */
  def failure[A](msg: String): Parser[A] = parser(_ => Failure(msg))


  /**
   * Scalaz Functor instance, for monad MA pimps.
   */
  implicit def ParserFunctor: Functor[Parser] = new Functor[Parser] {
    def fmap[A, B](a: Parser[A], f: A => B) = parser(a.parse(_) map (_ map f))
  }

  /**
   * Scalaz Pure instance, for monad MA pimps.
   */
  implicit def ParserPure: Pure[Parser] = new Pure[Parser] {
    def pure[A](a: => A) = value(a)
  }

  /**
   * Scalaz Bind instance, for monad MA pimps.
   */
  implicit def ParserBind: Bind[Parser] = new Bind[Parser] {
    def bind[A, B](p: Parser[A], f: A => Parser[B]) = parser(p.parse(_) match {
      case Success((rest, value)) => f(value).parse(rest)
      case Failure(error) => Failure(error)
    })
  }

  /**
   * Scalaz Apply instance, for applicative functor MA pimps.
   */
  implicit def ParserApply: Apply[Parser] = new Apply[Parser] {
    def apply[A, B](f: Parser[A => B], a: Parser[A]) = f flatMap { k => a map (k(_)) }
  }

  /**
   * Scalaz Zero instance, for monad plus MA pimps.
   */
  implicit def ParserZero[A]: Zero[Parser[A]] = zero(failure[A]("empty"))

  /**
   * Scalaz Plus instance, for monad plus MA pimps.
   */
  implicit def ParserPlus[A]: Plus[Parser] = new Plus[Parser] {
    def plus[A](p1: Parser[A], p2: => Parser[A]) =
      parser((args: List[String]) => p1.parse(args) match {
        case Success((rest, value)) => Success((rest, value))
        case Failure(_) => p2.parse(args)
      })
  }

  /**
   * String parser, consumes a single string off the input, fails
   * on empty input.
   */
  def string: Parser[String] =
    parser({
      case Nil => Failure("Unexpected end of input.")
      case head :: tail => Success((tail, head))
    })

  /**
   * Consumes a string off the input, iff pred is true for that
   * string.
   */
  def satisfy(pred: String => Boolean): Parser[String] =
    string >>=
      (s => if (pred(s)) value(s) else failure("unexpected value [" + s + "]"))

  /**
   * Consumes a string off the input, iff it eqauls string s.
   */
  def is(s: String): Parser[String] =
    satisfy(_ == s)

  /**
   * Multi-way choice, first success proceeds.
   */
  def choiceN[A](ps: List[Parser[A]]): Parser[A] =
    ps.foldRight(failure[A]("empty"))(_ | _)
}
