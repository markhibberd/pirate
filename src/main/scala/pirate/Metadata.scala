package pirate

import scalaz._

case class Metadata(names: Option[Name], description: Option[String], metavar: Option[String], visible: Boolean) {
  def hasShort(s: Char): Boolean = names match  {
    case Some(n) => n.hasShort(s)
    case None    => false
  }
  def hasLong(l: String): Boolean = names match  {
    case Some(n) => n.hasLong(l)
    case None    => false
  }
}

object Metadata {
  implicit def MetadataMonoid: Monoid[Metadata] = new Monoid[Metadata] {
    def zero = Metadata(None, None, None, true)
    def append(m1: Metadata, m2: => Metadata) = Metadata(Name.combineOpt(m1.names, m2.names), m1.description orElse m2.description, m1.metavar orElse m2.metavar, m1.visible && m2.visible)
  }
}