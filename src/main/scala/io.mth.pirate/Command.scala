package io.mth.pirate

case class Command[A](description: Option[String], parse: Parse[A])
