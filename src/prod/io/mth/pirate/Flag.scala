package io.mth.pirate

/**
 * A data type representing argument values.
 *
 * Can represent:
 *   -f
 *   --flag
 *   -f|--flag
 *    -f=PARAM
 *    --flag=PARAM
 *    -f|--flag=PARAM
 *
 * Consider support for:
 *    -f param[,param ...]
 */
sealed trait Flag[A] {
  import scalaz._
  import Scalaz._
  import Parser._
  import Flag._

  def fold[B](
    flag0: (Option[Char], Option[String], String, (A => A)) => B,
    flag1: (Option[Char], Option[String], String, String, ((A, String) => A)) => B,
    flags: List[Flag[A]] => B
  ): B

  def toList: List[Flag[A]] =
    fold(
      (_, _, _, _) => List(this),
      (_, _, _, _, _) => List(this),
      _.flatMap(_.toList)
    )

  def <|>(flag: Flag[A]) =
    flags(toList ::: flag.toList)

  // FIX add ParseMode, configure up the flag syntax.
  def toParser: Parser[A => A] = fold(
    (s, l, d, f) => FlagParsers.flag0(s, l) map (_ => f),
    (s, l, d, m, f) =>  FlagParsers.flag1(s, l) map (v => f(_, v)),
    fs => FlagParsers.flagParsers(fs.map(_.toParser)).map(_.foldRight(identity[A]_)(_ compose _))
  )
}

object Flag {
  def short[A](short: Char, description: String)(f: A => A) =
    flag0(Some(short), None, description, f)

  def long[A](long: String, description: String)(f: A => A) =
    flag0(None, Some(long), description, f)

  def full[A](short: Char, long: String, description: String)(f: A => A) =
    flag0(Some(short), Some(long), description, f)

  def short1[A](short: Char, description: String, meta: String)(f: (A, String) => A) =
    flag1(Some(short), None, description, meta, f)

  def long1[A](long: String, description: String, meta: String)(f: (A, String) => A) =
    flag1(None, Some(long), description, meta, f)

  def full1[A](short: Char, long: String, description: String, meta: String)(f: (A, String) => A) =
    flag1(Some(short), Some(long), description, meta, f)

  def flags[A](flags: List[Flag[A]]): Flag[A] = new Flag[A] {
      def fold[B](
        flag0: (Option[Char], Option[String], String, (A => A)) => B,
        flag1: (Option[Char], Option[String], String, String, ((A, String) => A)) => B,
        flagsx: List[Flag[A]] => B
      ): B = flagsx(flags.flatMap(_.toList))
    }

  /*
   * These need to be hidden to maintain integrity of datatype. Is there a better way to
   * do it with types?
   */
  
  private def flag0[A](s: Option[Char], l: Option[String], d: String, f: A => A): Flag[A] = new Flag[A] {
    def fold[B](
      flag0: (Option[Char], Option[String], String, (A => A)) => B,
      flag1: (Option[Char], Option[String], String, String, ((A, String) => A)) => B,
      flags: List[Flag[A]] => B
    ): B = flag0(s, l, d, f)
  }

  private def flag1[A](s: Option[Char], l: Option[String], d: String, m: String, f: (A, String) => A): Flag[A] = new Flag[A] {
    def fold[B](
      flag0: (Option[Char], Option[String], String, (A => A)) => B,
      flag1: (Option[Char], Option[String], String, String, ((A, String) => A)) => B,
      flags: List[Flag[A]] => B
    ): B = flag1(s, l, d, m, f)
  }
}

