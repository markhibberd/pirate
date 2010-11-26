package io.mth.pirate

/**
 * Positional data type. Represents positional parameters,
 * and their arity.
 *
 * Each of the variants of this type include a function
 * for transforming a type if it is succeeds in parsing.
 */
sealed trait Positional[A] {
  import Positional._
  import scalaz._
  import Scalaz._
  import Parser._

  /**
   * Catamorphism for the Positional data type.
   */
  def fold[X](
    one: (String, ((A, String) => A)) => X,
    n: (Int, String, ((A, List[String]) => A)) => X,
    zeroplus: (String, ((A, List[String]) => A)) => X,
    oneplus: (String, ((A, List[String]) => A)) => X
  ): X

  /**
   * Combine this positional parameter with a new positional,
   * parameter and return the positional parameters. The operation
   * to combine positional parameters is NOT associative. Parsing
   * is heavily dependent on the order in which positional parameters
   * added to the command.
   */
  def >|(p: Positional[A]): Positionals[A] = toPositionals >| p


  /**
   * Convert this Positional into a Positionals.
   */
  def toPositionals = Positionals.positionals(this)
}

object Positional {
  /**
   * Type constructor for a single positional parameter.
   */
  def positional[A](meta: String)(f: (A, String) => A): Positional[A] = new Positional[A] {
    def fold[X](
      one: (String, ((A, String) => A)) => X,
      n: (Int, String, ((A, List[String]) => A)) => X,
      zeroplus: (String, ((A, List[String]) => A)) => X,
      oneplus: (String, ((A, List[String]) => A)) => X
    ): X = one(meta, f)
  }

  /**
   * Type constructor for a fixed n positional parameter.
   */
  def positionalN[A](number: Int, meta: String)(f: (A, List[String]) => A): Positional[A] = new Positional[A] {
    def fold[X](
      one: (String, ((A, String) => A)) => X,
      n: (Int, String, ((A, List[String]) => A)) => X,
      zeroplus: (String, ((A, List[String]) => A)) => X,
      oneplus: (String, ((A, List[String]) => A)) => X
    ): X = n(number, meta, f)
  }

  /**
   * Type constructor for a variable positional parameter that
   * can occur 0 or more times.
   */
  def positional0plus[A](meta: String)(f: (A, List[String]) => A): Positional[A] = new Positional[A] {
    def fold[X](
      one: (String, ((A, String) => A)) => X,
      n: (Int, String, ((A, List[String]) => A)) => X,
      zeroplus: (String, ((A, List[String]) => A)) => X,
      oneplus: (String, ((A, List[String]) => A)) => X
    ): X = zeroplus(meta, f)
  }

  /**
   * Type constructor for a variable positional parameter that
   * can occur 1 or more times.
   */
  def positional1plus[A](meta: String)(f: (A, List[String]) => A): Positional[A] = new Positional[A] {
    def fold[X](
      one: (String, ((A, String) => A)) => X,
      n: (Int, String, ((A, List[String]) => A)) => X,
      zeroplus: (String, ((A, List[String]) => A)) => X,
      oneplus: (String, ((A, List[String]) => A)) => X
    ): X = oneplus(meta, f)
  }
}
