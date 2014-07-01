package io.mth.pirate

sealed trait Name {
  def short: Option[Char] = this match {
    case Short(s) => Some(s)
    case Long(_) => None
    case Both(s, _) => Some(s)
  }

  def long: Option[String] = this match {
    case Short(_) => None
    case Long(l) => Some(l)
    case Both(_, l) => Some(l)
  }

  def hasShort(s: Char): Boolean =
    short.map(_ == s).getOrElse(false)

  def hasLong(l: String): Boolean =
    long.map(_ == l).getOrElse(false)
}
case class Short(s: Char) extends Name
case class Long(l: String) extends Name
case class Both(s: Char, l: String) extends Name
object Name { val apply = Both.apply _ }
