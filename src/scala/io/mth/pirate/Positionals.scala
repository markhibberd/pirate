package io.mth.pirate

/**
 * Positionals data type. Represents a set of positional parameters..
 */
sealed trait Positionals[A] {
  import Positionals._
  import scalaz._
  import Scalaz._
  import Parser._

  /**
   * Catamorphism for the Positionals data type.
   */
  def fold[X](
    positional: List[Positional[A]] => X
  ): X

  /**
   * Combine this positional parameters with a new positional,
   * parameter and return the positional parameters. The operation
   * to combine positional parameters is NOT associative. Parsing
   * is heavily dependent on the order in which positional parameters
   * added to the command.
   */
  def >|(positional: Positional[A]): Positionals[A] =
    this >>| positionals(positional)

  /**
   * Combine this positional parameters with a set of positionals,
   * parameter and return the positional parameters. The operation
   * to combine positional parameters is NOT associative. Parsing
   * is heavily dependent on the order in which positional parameters
   * added to the command.
   */
  def >>|(positionals: Positionals[A]): Positionals[A] = fold(
    current => positionalz(current ::: positionals.toList)
  )

  /**
   * Convert this Positionals[A] to a list of Positional[A].
   */
  def toList = fold(identity)
}

object Positionals {
  /**
   * Data constructor for the empty set of positional parameters.
   */
  def emptypositionals[A] = positionalz[A](List())

  /**
   * Data constructor for a single positional parameter.
   */
  def positionals[A](positional: Positional[A]) = emptypositionals >| positional

  /**
   * Restricted type constructor for a set of positional parameters.
   */
  private def positionalz[A](positionalz: List[Positional[A]]): Positionals[A] = new Positionals[A] {
    def fold[X](
      positional: List[Positional[A]] => X
    ):X = positional(positionalz)
  }
}
