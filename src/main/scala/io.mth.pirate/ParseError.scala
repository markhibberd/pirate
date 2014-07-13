package io.mth.pirate

sealed trait ParseError
case object ParseErrorNoMessage extends ParseError
case class ParseErrorMessage(s: String) extends ParseError
