package pirate

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
    short.exists(_ == s)

  def hasLong(l: String): Boolean =
    long.exists(_ == l)

  def render: String = this match {
    case ShortName(s) => s"-${s}"
    case LongName(l) => s"--${l}"
    case BothName(s, l) => s"-${s}|--${l}"
  }
}

case class ShortName(s: Char) extends Name
case class LongName(l: String) extends Name
case class BothName(s: Char, l: String) extends Name
object Name { 
  def apply(s: Char, l: String) = BothName(s, l) 
  def combine (first: Name, second: Name) = (first, second) match {
    case (ShortName(s), LongName(l)) => BothName(s, l)
    case (LongName(l), ShortName(s)) => BothName(s, l)
    case (_, second) => second
  }

  def combineOpt (first: Option[Name], second: Option[Name]) = (first, second) match {
    case (Some(a), Some(b)) => Some(combine(a, b))
    case (Some(a), None)    => Some(a)
    case (None, Some(a))    => Some(a)
    case (None, None)       => None
  }
}
