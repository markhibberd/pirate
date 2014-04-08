package io.mth.pirate

import java.io.PrintStream


trait SubCommand[A] {
  type B <: A
  def seed: B
  def command: Command[B]
}

object SubCommand {
  def apply[A, AA <: A](c: Command[AA], s: AA): SubCommand[A] = new SubCommand[A] {
    type B = AA
    def seed = s
    def command = c
  }
}

/**
 * Commands data type. Represents a command with sub-commands.
 */
sealed trait Commands[A] {
  import scalaz._
  import Commands._

  /**
   * Catamorphism for Command data type.
   */
  def fold[X](
    x: (String, Option[String], List[(A, Command[A])]) => X
  ): X

  /**
   * The name of this command.
   */
  def name: String =
    fold((n, _, _) => n)

  /**
   * The description of this command.
   */
  def description: Option[String] =
    fold((_, d, _) => d)

  /**
   * The subcommands of this command.
   */
  def subcommands: List[Command[A]] =
    fold((_, _, cs) => cs)

  def withSubcommand[B <: A](command: Command[B], seed: B): Commands[A] =


  /**
   * Combine this command with the specified description and
   * return the new description. If a description is already
   * set it shall be replaced.
   */
  def ~(description: String): Commands[A] = fold(
    (n, d, cs) => supercommand(n, Some(description), cs)
  )

  /**
   * The usage string for this command using the default
   * usage mode. Equivalent to usageForMode(DefaultUsageMode).
   */
  def usage = usageForMode(DefaultUsageMode)

  /**
   * The usage string for this command using the specified
   * usage mode.
   */
  def usageForMode(mode: UsageMode) = Usage.commandsusage(mode)(this)

  /**
   * Create an argument parser for this command. This is for advanced
   * usage only. It is expected that the `parse` method is sufficient
   * for most cases.
   */
  def toParser: Parser[A => A] = fold(
    (n, d, cs) => CommandParsers.commands(cs)
  )

  /**
   * Parse a list of arguments based on this command and apply the resultant
   * function to the data object.
   */
  def parse(args: List[String], default: A): Validation[String, A] =
    toParser.parse(args) match {
      case Success((rest, f)) =>
        if (rest.isEmpty) Success(f(default))
        else Failure("Too many arguments / Arguments could not be parsed: " + rest)
      case Failure(msg) => Failure(msg)
    }

  /**
   * Higher order function to handle parse and dispatch. This is
   * a convenience only.
   */
  def dispatchOrUsage(args: List[String], default: A, err: PrintStream = System.err)(f: A => Unit): Int =
    dispatch(args, default)(f)(msg => err.println(msg + "\n\n" + usage))

  /**
   * Higher order function to handle parse and dispatch. This is
   * a convenience only.
   */
  def dispatchOrDie(args: List[String], default: A, err: PrintStream = System.err)(f: A => Unit): Unit =
    sys.exit(dispatchOrUsage(args, default)(f))

  /**
   * Higher order function to handle parse and dispatch. This is
   * a convenience only.
   */
  def dispatch(args: List[String], default: A)(success: A => Unit)(error: String => Unit): Int =
    parse(args, default) match {
      case Success(applied) => success(applied); 0
      case Failure(msg) => error(msg); 1
    }
}

object Commands {
  /**
   * Data constructor for the most super command. It is recommended that this
   * constructor is used with the combinators on Command to build up a command.
   *
   * It is also recommended to explicitly specify a type parameter when using
   * this constructor. This will greatly aid the scala type inference algorithm
   * when applying the combinators. This means use command[ArgType]("name"),
   * rather than command("name").
   */
  def commands[A](name: String): Commands[A] =
    supercommand(name, None, Nil)

  /**
   * Data constructor for a complete super command. This is for advanced usage only.
   * The equivalent commands can be built using the `command` constructor and the
   * combinators.
   */
  def supercommand[A](name: String, description: Option[String], commands: List[Command[A]]): Commands[A] =
    new Commands[A] {
      def fold[X](
        x: (String, Option[String], List[Command[A]]) => X
      ): X = x(name, description, commands)
    }
}
