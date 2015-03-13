package pirate

import scalaz._

case class Metadata(description: Option[String], metavar: Option[String], visible: Boolean)

object Metadata {
  implicit def MetadataMonoid: Monoid[Metadata] = new Monoid[Metadata] {
    def zero = Metadata(None, None, true)
    def append(m1: Metadata, m2: => Metadata) = Metadata(m1.description orElse m2.description, m1.metavar orElse m2.metavar, m1.visible && m2.visible)
  }
}
