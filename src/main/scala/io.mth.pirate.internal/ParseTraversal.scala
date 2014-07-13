package io.mth.pirate.internal

import io.mth.pirate._

case class OptHelpInfo(multi: Boolean, dfault: Boolean)

sealed trait ParseTree[A] {
  def flatten: List[A] = this match {
    case ParseTreeLeaf(value) => List(value)
    case ParseTreeAp(children) => children.flatMap(_.flatten)
    case ParseTreeAlt(children) => children.flatMap(_.flatten)
  }

}
case class ParseTreeLeaf[A](value: A) extends ParseTree[A]
case class ParseTreeAp[A](children: List[ParseTree[A]]) extends ParseTree[A]
case class ParseTreeAlt[A](children: List[ParseTree[A]]) extends ParseTree[A]

/* A universally qualified function for handling the existantial parsers in the tree */
trait OptionRunner[F[+_]] {
  def run[A](options: Parser[A]): NondetT[F, A]
}

trait TreeTraverseF[A] {
  def run[X](info: OptHelpInfo, p: Parser[X], m: Metadata): A
}
