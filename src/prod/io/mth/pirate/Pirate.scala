package io.mth.pirate

import scalaz._

trait Pirate[A] {
  def fold[B](
    command: String => Flag[A] => B,
    compound: List[Pirate[A]] => B
  ): B

  def bind(p: Pirate[A]) = new Pirate[A] {
    def fold[B](
      single: String => Flag[A] => B,
      compound: List[Pirate[A]] => B
    ): B = compound(List(this, p))
  }

  def >>=(p: Pirate[A]) = bind(p)

  def dispatch(args: List[String], a: A)(succ: A => Unit)(err:String => Unit) = {
    toParser.parse(args) match {
      case Success((rest, f)) => if (rest.isEmpty) succ(f(a)) else err("Not all arguments could be processed: " + rest)
      case Failure(msg) => err(msg)
    }
  }

  def toParser: Parser[A => A] = fold(
      name => f => f.toParser,
      ps => Parser.choiceN(ps.map(_.toParser))
    )

  def parse(args: List[String], a: A): Option[A] =
    toParser.parse(args) match {
      case Success((rest, f)) => if (rest.isEmpty) Some(f(a)) else None
      case Failure(msg) => None
    }
}

object Pirate {
  def command[A](name: String, flags:Flag[A]) = new Pirate[A] {
    def fold[B](
      single: String => Flag[A] => B,
      compound: List[Pirate[A]] => B
    ): B = single(name)(flags)
  }
}

