package pirate

import pirate.internal._

sealed trait ParseError
case object ParseErrorNoMessage extends ParseError
case class ParseErrorShowHelpText(sub: Option[String]) extends ParseError
case class ParseErrorMessage(s: String) extends ParseError
case class ParseErrorMissing(s: ParseTree[Info]) extends ParseError
case class ParseErrorInvalidOption(s: String) extends ParseError
case class ParseErrorInvalidArgument(s: String) extends ParseError
