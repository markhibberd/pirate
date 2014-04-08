package io.mth.pirate

/**
 * Flag data type. Can represent one of:
 *  - flag with short and long identifier, and no argument
 *  - flag with only short identifier, and no argument
 *  - flag with only long identifier, and no argument
 *  - flag with short and long identifier, and an argument
 *  - flag with only short identifier, and an argument
 *  - flag with only long identifier, and an argument
 *
 * Each of the variants of this type include a function
 * for transforming a type if it is succeeds in parsing.
 */
sealed trait Flag[A] {
  import scalaz._
  import Scalaz._
  import Parser._
  import Flag._

  /**
   * Catamorphism for the Flag data type.
   */
  def fold[B](
    flag0: (Char, String, String, (A => A)) => B,
    flag0s: (Char, String, (A => A)) => B,
    flag0l: (String, String, (A => A)) => B,
    flag1: (Char, String, String, String, ((A, String) => A)) => B,
    flag1s: (Char, String, String, ((A, String) => A)) => B,
    flag1l: (String, String, String, ((A, String) => A)) => B
  ): B


  /**
   * Combine this flag with a new flag, and return as
   * a new flags. The operation to combine flags is
   * associative.
   */
  def <|>(flag: Flag[A]) = toFlags <|> flag

  /**
   * Combine this flag with a set of flags, and return as
   * a new flags. The operation to combine flags is
   * associative.
   */
  def <<|>>(flags: Flags[A]) = toFlags <<|>> flags


  /**
   * Convert this Flag into a Flags.
   */
  def toFlags = Flags.flags(this)

  /**
   * The description for this flag.
   */
  def description = fold(
    (_, _, d, _) => d,
    (_, d, _) => d,
    (_, d, _) => d,
    (_, _, d, _, _) =>  d,
    (_, d, _, _) =>  d,
    (_, d, _, _) =>  d
  )
}

object Flag {
  /**
   * Data constructor for a Flag with only a short identifier, and no argument.
   */
  def short[A](short: Char, desc: String)(f: A => A): Flag[A] = new Flag[A] {
    def fold[B](
      flag0: (Char, String, String, (A => A)) => B,
      flag0s: (Char, String, (A => A)) => B,
      flag0l: (String, String, (A => A)) => B,
      flag1: (Char, String, String, String, ((A, String) => A)) => B,
      flag1s: (Char, String, String, ((A, String) => A)) => B,
      flag1l: (String, String, String, ((A, String) => A)) => B
    ): B = flag0s(short, desc, f)
  }

  /**
   * Data constructor for a Flag with only a long identifier, and no argument.
   */
  def long[A](long: String, desc: String)(f: A => A): Flag[A] = new Flag[A] {
    def fold[B](
      flag0: (Char, String, String, (A => A)) => B,
      flag0s: (Char, String, (A => A)) => B,
      flag0l: (String, String, (A => A)) => B,
      flag1: (Char, String, String, String, ((A, String) => A)) => B,
      flag1s: (Char, String, String, ((A, String) => A)) => B,
      flag1l: (String, String, String, ((A, String) => A)) => B
    ): B = flag0l(long, desc, f)
  }

  /**
   * Data constructor for a Flag with both a short and long identifier, and no argument.
   */
  def flag[A](short: Char, long: String, desc: String)(f: A => A): Flag[A] = new Flag[A] {
    def fold[B](
      flag0: (Char, String, String, (A => A)) => B,
      flag0s: (Char, String, (A => A)) => B,
      flag0l: (String, String, (A => A)) => B,
      flag1: (Char, String, String, String, ((A, String) => A)) => B,
      flag1s: (Char, String, String, ((A, String) => A)) => B,
      flag1l: (String, String, String, ((A, String) => A)) => B
    ): B = flag0(short, long, desc, f)
  }

  /**
   * Data constructor for a Flag with only a short identifier, and with an argument.
   */
  def short1[A](short: Char, desc: String, meta: String)(f: (A, String) => A): Flag[A] = new Flag[A] {
    def fold[B](
      flag0: (Char, String, String, (A => A)) => B,
      flag0s: (Char, String, (A => A)) => B,
      flag0l: (String, String, (A => A)) => B,
      flag1: (Char, String, String, String, ((A, String) => A)) => B,
      flag1s: (Char, String, String, ((A, String) => A)) => B,
      flag1l: (String, String, String, ((A, String) => A)) => B
    ): B = flag1s(short, desc, meta, f)
  }

  /**
   * Data constructor for a Flag with only a long identifier, and with an argument.
   */
  def long1[A](long: String, desc: String, meta: String)(f: (A, String) => A): Flag[A] = new Flag[A] {
    def fold[B](
      flag0: (Char, String, String, (A => A)) => B,
      flag0s: (Char, String, (A => A)) => B,
      flag0l: (String, String, (A => A)) => B,
      flag1: (Char, String, String, String, ((A, String) => A)) => B,
      flag1s: (Char, String, String, ((A, String) => A)) => B,
      flag1l: (String, String, String, ((A, String) => A)) => B
    ): B = flag1l(long, desc, meta, f)
  }

  /**
   * Data constructor for a Flag with both a long and short identifier, and with an argument.
   */
  def flag1[A](short: Char, long: String, desc: String, meta: String)(f: (A, String) => A): Flag[A] = new Flag[A] {
    def fold[B](
      flag0: (Char, String, String, (A => A)) => B,
      flag0s: (Char, String, (A => A)) => B,
      flag0l: (String, String, (A => A)) => B,
      flag1: (Char, String, String, String, ((A, String) => A)) => B,
      flag1s: (Char, String, String, ((A, String) => A)) => B,
      flag1l: (String, String, String, ((A, String) => A)) => B
    ): B = flag1(short, long, desc, meta, f)
  }

}
