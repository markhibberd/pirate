package io.mth.pirate

/**
 * Flags data type. Represents a set of flags.
 */
sealed trait Flags[A] {
  import scalaz._
  import Scalaz._
  import Parser._
  import Flags._

  /**
   * Catamorphism for the Flags data type.
   */
  def fold[X](
    flags: List[Flag[A]] => X
  ): X

  /**
   * Combine this flags with a new flag, and return as
   * a new flags. The operation to combine flags is
   * associative, i.e. not order dependant.
   */
  def <|>(flag: Flag[A]): Flags[A] = fold(
      flags => flagz(flags ::: List(flag))
    )

  /**
   * Convert this Flags[A] to a list of Flag[A].
   */
  def toList = fold(identity)
}

object Flags {
  /**
   * Type constructor for the empty set of flags.
   */
  def emptyflags[A] = flagz[A](List())

  /**
   * Type constructor for a single flag.
   */
  def flags[A](flag: Flag[A]) = emptyflags <|> flag

  /**
   * Restricted type constructor for a set of flags.
   */
  private def flagz[A](flagz: List[Flag[A]]): Flags[A] = new Flags[A] {
    def fold[X](
      flags: List[Flag[A]] => X
    ):X = flags(flagz) 
  }
}