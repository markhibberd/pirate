package pirate

sealed trait ParseError
case object ParseErrorNoMessage extends ParseError
case object ParseErrorShowHelpText extends ParseError
case class ParseErrorMessage(s: String) extends ParseError
