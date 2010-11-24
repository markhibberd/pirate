package io.mth.pirate


/**
 * Work in progress command data structure, with combinators for slapping together
 * flags and positional arguments. Possible merge with 'Program'.
 */
sealed trait Command[A] {
  import scalaz._
  import Command._
  import Flag._
  import Positional._

  def fold[X](
    x: String => Option[String] => List[Flag[A]] => List[Positional[A]] => X 
  ): X

  def <|>(flag: Flag[A]): Command[A] = fold(
      n => d => f => p => commandfpo(n, d, flags(f) <|> flag, positionals(p))
    )

  def >|(positional: Positional[A]): Command[A] = fold(
      n => d => f => p => commandfpo(n, d, flags(f), positionals(p) >| positional)
    )

  def ~(description: String): Command[A] = fold(
      n => d => f => p => commandfpo(n, Some(description), flags(f), positionals(p))  
  )

  def usage = Usage.usage(this)

  def toParser: Parser[A => A] = fold(
      n => d => f => p => FlagParsers.commandlinex(flags(f).toParser, positionals(p).toParser)
    )

  def parse(args: List[String], a: A): Validation[String, A] =
    toParser.parse(args) match {
      case Success((rest, f)) => if (rest.isEmpty) Success(f(a)) else Failure("Too many arguments / Arguments could not be parsed: " + rest)
      case Failure(msg) => Failure(msg)
    }
}

object Command {
  def command[A](name: String): Command[A] =
    commandfpl(name, List(), List())

  def commandd[A](name: String, description: String): Command[A] =
    commandfpld(name, description, List(), List())

  
  def commandf[A](name: String, flag: Flag[A]): Command[A] =
    commandfpl(name, flag.toList, List())

  def commandfd[A](name: String, description: String, flag: Flag[A]): Command[A] =
    commandfpld(name, description, flag.toList, List())

  def commandp[A](name: String, positional: Positional[A]): Command[A] =
    commandfpl(name, List(), positional.toList)

  def commandpd[A](name: String, description: String, positional: Positional[A]): Command[A] =
    commandfpld(name, description, List(), positional.toList)

  def commandfpl[A](name: String, flags: List[Flag[A]], positionals: List[Positional[A]]): Command[A] =
    commandfp(name, Flag.flags(flags), Positional.positionals(positionals))

  def commandfpld[A](name: String, description: String, flags: List[Flag[A]], positionals: List[Positional[A]]): Command[A] =
    commandfpd(name, description, Flag.flags(flags), Positional.positionals(positionals))

  def commandfp[A](name: String, flag: Flag[A], positional: Positional[A]): Command[A] =
    commandfpo(name, None, flag, positional)

  def commandfpd[A](name: String, description: String, flag: Flag[A], positional: Positional[A]): Command[A] =
    commandfpo(name, Some(description), flag, positional)

  def commandfpo[A](name: String, description: Option[String], flag: Flag[A], positional: Positional[A]): Command[A] = new Command[A] {
    def fold[X](
      x: String => Option[String] => List[Flag[A]] => List[Positional[A]] => X
    ): X = x(name)(description)(flag.toList)(positional.toList)
  }
}

sealed trait MultiFormCommand[A] {
  def fold[X](
    x: String => List[(Flag[A], Positional[A])],
    y: String => String => List[(Flag[A], Positional[A])]
  ): X
}

sealed trait MultiModeCommand[A] {
  def fold[X](
    a: String => List[Command[A]] => X,                // program [subcommands]
    b: String => String => List[Command[A]] => X,      // program description [subcommands]
    c: String => Flag[A] => List[Command[A]] => X,     // program flags [subcommands]
    d: String => String => Flag[A] => List[Command[A]] => X, // program description flags [subcommands]
    e: String => Command[A] => List[Command[A]]        // program defaultcommand [subcommands]
  ): X
}