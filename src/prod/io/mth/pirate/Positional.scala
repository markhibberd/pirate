package io.mth.pirate


sealed trait Positional[A] {
  import Positional._
  import scalaz._
  import Scalaz._
  import Parser._


  def fold[X](
    one: String => ((A, String) => A) => X,
    n: Int => String => ((A, List[String]) => A) => X,
    zeroplus: String => ((A, List[String]) => A) => X,
    oneplus: String => ((A, List[String]) => A) => X,
    combined: List[Positional[A]] => X
  ): X

  def toList: List[Positional[A]] =
    fold(
      m => f => List(this),
      n => m => f => List(this),
      m => f => List(this),
      m => f => List(this),
      ps => ps
    )

    def toParser: Parser[A => A] = fold(
      m => f => FlagParsers.positional1.lift(v => f(_, v)),
      n => m => f => error("todo"),
      m => f => FlagParsers.positionalN.lift(v => f(_, v)),
      m => f => error("todo"),
      ps => ps.map(_.toParser).sequence.map(_.foldRight(identity[A]_)(_ compose _))
    )

  def >|(p: Positional[A]): Positional[A] = positionals(toList ::: p.toList)
}

object Positional {
  def positional[A](meta: String)(f: (A, String) => A): Positional[A] = new Positional[A] {
    def fold[X](
      one: String => ((A, String) => A) => X,
      n: Int => String => ((A, List[String]) => A) => X,
      zeroplus: String => ((A, List[String]) => A) => X,
      oneplus: String => ((A, List[String]) => A) => X,
      combined: List[Positional[A]] => X
    ): X = one(meta)(f)
  }

  def positionals[A](ps: List[Positional[A]]): Positional[A] = new Positional[A] {
    def fold[X](
      one: String => ((A, String) => A) => X,
      n: Int => String => ((A, List[String]) => A) => X,
      zeroplus: String => ((A, List[String]) => A) => X,
      oneplus: String => ((A, List[String]) => A) => X,
      combined: List[Positional[A]] => X
    ): X = combined(ps)
  }

  def positionalN[A](n: Int, meta: String)(f: (A, List[String]) => A): Positional[A] = new Positional[A] {
    def fold[X](
      one: String => ((A, String) => A) => X,
      i: Int => String => ((A, List[String]) => A) => X,
      zeroplus: String => ((A, List[String]) => A) => X,
      oneplus: String => ((A, List[String]) => A) => X,
      combined: List[Positional[A]] => X
    ): X = i(n)(meta)(f)
  }

  def positional0plus[A](meta: String)(f: (A, List[String]) => A): Positional[A] = new Positional[A] {
    def fold[X](
      one: String => ((A, String) => A) => X,
      n: Int => String => ((A, List[String]) => A) => X,
      zeroplus: String => ((A, List[String]) => A) => X,
      oneplus: String => ((A, List[String]) => A) => X,
      combined: List[Positional[A]] => X
    ): X = zeroplus(meta)(f)
  }

  def positional1plus[A](meta: String)(f: (A, List[String]) => A): Positional[A] = new Positional[A] {
    def fold[X](
      one: String => ((A, String) => A) => X,
      n: Int => String => ((A, List[String]) => A) => X,
      zeroplus: String => ((A, List[String]) => A) => X,
      oneplus: String => ((A, List[String]) => A) => X,
      combined: List[Positional[A]] => X
    ): X = oneplus(meta)(f)
  }
}
