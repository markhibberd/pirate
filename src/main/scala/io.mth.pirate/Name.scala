package io.mth.pirate

sealed trait Name {
  def short: Option[Char] = this match {
    case ShortName(s) => Some(s)
    case LongName(_) => None
    case BothName(s, _) => Some(s)
  }

  def long: Option[String] = this match {
    case ShortName(_) => None
    case LongName(l) => Some(l)
    case BothName(_, l) => Some(l)
  }

  def hasShort(s: Char): Boolean =
    short.map(_ == s).getOrElse(false)

  def hasLong(l: String): Boolean =
    long.map(_ == l).getOrElse(false)
}

case class ShortName(s: Char) extends Name
case class LongName(l: String) extends Name
case class BothName(s: Char, l: String) extends Name
object Name { def apply(s: Char, l: String) = BothName(s, l) }
