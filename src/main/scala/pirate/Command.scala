package pirate

case class Command[A](name: String, description: Option[String], parse: Parse[A]) {
  def ~~(description: String): Command[A] =
    copy(description = Some(description))
}
