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
    x: String => List[Flag[A]] => List[Positional[A]] => X 
  ): X

  def <|>(flag: Flag[A]): Command[A] = fold(
      n => f => p => commandfp(n, flags(f) <|> flag, positionals(p))
    )

  def >|(positional: Positional[A]): Command[A] = fold(
      n => f => p => commandfp(n, flags(f), positionals(p) >| positional)
    )

//  def ~(description: String): Program[A] = Program.programWithDescription(this, description)

  def usage = Usage.usage(this)

  def toParser: Parser[A => A] = fold(
      n => f => p => FlagParsers.commandlinex(flags(f).toParser, positionals(p).toParser)
    )

  def parse(args: List[String], a: A): Validation[String, A] =
    toParser.parse(args) match {
      case Success((rest, f)) => if (rest.isEmpty) Success(f(a)) else Failure("Too many arguments / Arguments could not be parsed: " + rest)
      case Failure(msg) => Failure(msg)
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

object Command {
  def command[A](name: String): Command[A] = new Command[A] {
    def fold[X](
      x: String => List[Flag[A]] => List[Positional[A]] => X
    ): X = x(name)(List())(List())
  }

  def commandf[A](name: String, flag: Flag[A]): Command[A] = new Command[A] {
    def fold[X](
      x: String => List[Flag[A]] => List[Positional[A]] => X
    ): X = x(name)(flag.toList)(List())
  }

  def commandp[A](name: String, positional: Positional[A]): Command[A] = new Command[A] {
    def fold[X](
      x: String => List[Flag[A]] => List[Positional[A]] => X
    ): X = x(name)(List())(positional.toList)
  }

  def commandfp[A](name: String, flag: Flag[A], positional: Positional[A]): Command[A] = new Command[A] {
    def fold[X](
      x: String => List[Flag[A]] => List[Positional[A]] => X
    ): X = x(name)(flag.toList)(positional.toList)
  }

  def commandfpl[A](name: String, flags: List[Flag[A]], positionals: List[Positional[A]]): Command[A] =
    commandfp(name, Flag.flags(flags), Positional.positionals(positionals))
}