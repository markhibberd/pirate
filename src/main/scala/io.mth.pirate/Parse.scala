package io.mth.pirate

import io.mth.pirate.internal._
import scalaz._, Scalaz._, \&/._

sealed trait Parse[A] {
  def ~(name: String): Command[A] =
    Command(name, None, this)

  def map[B](f: A => B): Parse[B] = this match {
    case ValueParse(o) =>
      ValueParse(o.map(f))
    case PiratedParse(p, m) =>
      PiratedParse(p.map(f), m)
    case ApParse(k, a) =>
      ApParse(k.map(ff => ff.map(f)), a)
    case AltParse(a, b) =>
      AltParse(a.map(f), b.map(f))
    case BindParse(k, a) =>
      BindParse(k.map(_.map(f)), a)
  }

  def flatMap[B](f: A => Parse[B]): Parse[B] =
    BindParse(f, this)

  def |||(other: Parse[A]): Parse[A] =
    AltParse(this, other)

  def eval: Option[A] = this match {
    case ValueParse(o) =>
      o
    case PiratedParse(p, m) =>
      None
    case ApParse(k, a) =>
      a.eval <*> k.eval
    case AltParse(a, b) =>
      a.eval <+> b.eval
    case BindParse(k, a) =>
      a.eval >>= k.map(_.eval)
  }

  def mapTraverse[B](f: TreeTraverseF[B]): List[B] =
    treeTraverse(f).flatten

  def treeTraverse[B](f: TreeTraverseF[B]): ParseTree[B] = {
    def hasDefault[X](p: Parse[X]): Boolean =
      p.eval.isDefined

    def go[C](multi: Boolean, dfault: Boolean, f: TreeTraverseF[B], p: Parse[C]): ParseTree[B] = p match {
      case ValueParse(_) =>
        ParseTreeAp(Nil)
      case PiratedParse(p, m) =>
        ParseTreeLeaf(f.run(OptHelpInfo(multi, dfault), p, m))
      case ApParse(p1, p2) =>
        ParseTreeAp(List(go(multi, dfault, f, p1), go(multi, dfault, f, p2)))
      case AltParse(p1, p2) =>
        val dfaultx = dfault || hasDefault(p1) || hasDefault(p2)
        ParseTreeAlt(List(go(multi, dfaultx, f, p1), go(multi, dfaultx, f, p2)))
      case BindParse(k, p) =>
        go(true, dfault, f, p)
    }

    def simplify[X](x: ParseTree[X]): ParseTree[X] = x match {
      case ParseTreeLeaf(a) => ParseTreeLeaf(a)
      case ParseTreeAp(xs) => ParseTreeAp(xs.map(simplify).flatMap({
        case ParseTreeAp(ys) => ys
        case ParseTreeAlt(Nil) => Nil
        case x => List(x)
      }))
      case ParseTreeAlt(xs) => ParseTreeAlt(xs.map(simplify).flatMap({
        case ParseTreeAlt(ys) => ys
        case ParseTreeAp(Nil) => Nil
        case x => List(x)
      }))
    }

    simplify(go(false, false, f, this))
  }

  def option: Parse[Option[A]] =
    map(_.some) ||| ValueParse(Some(None))

  def default(fallback: => A): Parse[A] =
    option.map(_.getOrElse(fallback))
}

case class ValueParse[A](m: Option[A]) extends Parse[A]
case class PiratedParse[A](p: Parser[A], m: Metadata) extends Parse[A]
case class ApParse[A, B](f: Parse[A => B], a: Parse[A]) extends Parse[B]
case class AltParse[A](a: Parse[A], b: Parse[A]) extends Parse[A]
case class BindParse[A, B](f: A => Parse[B], a: Parse[A]) extends Parse[B]

object Parse {
  implicit def ParseMonad: Monad[Parse] with Plus[Parse] = new Monad[Parse] with Plus[Parse] {
    def point[A](a: => A) = ValueParse(Some(a))
    override def map[A, B](a: Parse[A])(f: A => B) = a map f
    def bind[A, B](a: Parse[A])(f: A => Parse[B]) = a flatMap f
    override def ap[A, B](a: => Parse[A])(f: => Parse[A => B]) = ApParse(f, a)
    def plus[A](a: Parse[A], b: => Parse[A]) = AltParse(a, b)
  }
}
