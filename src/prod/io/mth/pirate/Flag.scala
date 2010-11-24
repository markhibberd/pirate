package io.mth.pirate

import scalaz.{Success, Failure, Validation, X}

/**
 * A data type representing argument values.
 */
trait Flag[A] {
  import scalaz._
  import Scalaz._
  import Parser._
  import Flag._

  def fold[B](
    flag0: Option[Char] => Option[String] => String => (A => A) => B,
    flag1: Option[Char] => Option[String] => String => String => (String => A => A) => B,
    flags: List[Flag[A]] => B
  ): B

  def isFlag0 = fold(
      s => l => d => f => true,
      s => l => d => m => f => false,
      f => false
    )

  def isFlag1 = fold(
      s => l => d => f => false,
      s => l => d => m => f => true,
      f => false
    )

  def isPositional1 = fold(
      s => l => d => f => false,
      s => l => d => m => f => false,
      f => false
    )

  def isPositionalN = fold(
      s => l => d => f => false,
      s => l => d => m => f => false,
      f => false
    )


  def isFlags = fold(
      s => l => d => f => false,
      s => l => d => m => f => false,
      f => true
    )

  def toList: List[Flag[A]] =
    fold(
      s => l => d => f => List(this),
      s => l => d => m => f => List(this),
      f => f.flatMap(_.toList)
    )

  def |(flag: Flag[A]) = flags(toList ::: flag.toList)

  def toParser: Parser[A => A] = fold(
        s => l => d => f => FlagParsers.flag0(s, l) map (_ => f),
        s => l => d => m => f =>  FlagParsers.flag1(s, l) map (v => f(v)(_)),
        fs => FlagParsers.flagParser(choiceN(fs.map(_.toParser))).map(_.foldRight(identity[A]_)(_ compose _))
      )
}

object Flag {
  def short[A](short: Char, description: String)(f: A => A) =
    flag0(Some(short), None, description, f)

  def long[A](long: String, description: String)(f: A => A) =
    flag0(None, Some(long), description, f)

  def full[A](short: Char, long: String, description: String)(f: A => A) =
    flag0(Some(short), Some(long), description, f)

  def short1[A](short: Char, description: String, meta: String)(f: String => A => A) =
    flag1(Some(short), None, description, meta, f)

  def long1[A](long: String, description: String, meta: String)(f: String => A => A) =
    flag1(None, Some(long), description, meta, f)

  def full1[A](short: Char, long: String, description: String, meta: String)(f: String => A => A) =
    flag1(Some(short), Some(long), description, meta, f)

  def flags[A](flags: List[Flag[A]]): Flag[A] = new Flag[A] {
      def fold[B](
        flag0: Option[Char] => Option[String] => String => (A => A) => B,
        flag1: Option[Char] => Option[String] => String => String => (String => A => A) => B,
        flagsx: List[Flag[A]] => B
      ): B = flagsx(flags)
    }


  private def flag0[A](s: Option[Char], l: Option[String], d: String, f: A => A): Flag[A] = new Flag[A] {
    def fold[B](
       flag0: Option[Char] => Option[String] => String => (A => A) => B,
       flag1: Option[Char] => Option[String] => String => String => (String => A => A) => B,
       flags: List[Flag[A]] => B
    ): B = flag0(s)(l)(d)(f)
  }

  private def flag1[A](s: Option[Char], l: Option[String], d: String, m: String, f: String => A => A): Flag[A] = new Flag[A] {
    def fold[B](
       flag0: Option[Char] => Option[String] => String => (A => A) => B,
       flag1: Option[Char] => Option[String] => String => String => (String => A => A) => B,
       flags: List[Flag[A]] => B
    ): B = flag1(s)(l)(d)(m)(f)
  }
}

