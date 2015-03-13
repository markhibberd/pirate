package pirate

import scalaz._, Scalaz._

object Flags extends Flags

trait Flags {
  private def parse[A](p: Parser[A]): Parse[A] =
    ParserParse(p)

  // A simple parser to show the help text without disrupting the main parser
  def helper =
    abort(both('h', "help"), description("Prints the synopsis and a list of options and arguments."), ShowHelpText(None))

  // A helper which optionally shows the help text for subcommands.
  def helperX: Parse[Option[Unit]] =
    parse(FlagParser(both('h', "help"), description("Prints the synopsis and a list of the most commonly used commands. If a subcommand is named this option will show the synposis for said command."), Read.string.option.flatMap(s => Read.error[Unit](ShowHelpText(s))))).option

  // A simple parser to show the application version
  def version(v: String): Parse[Option[Unit]] =
    abort(both('v', "version"), description("Prints the application version."), ShowVersion(v))

  def abort(flag: Name, meta: Metadata, error: ReadError): Parse[Option[Unit]] =
    parse(FlagParser(flag, meta, Read.error(error))).option

  def terminator[A](flag: Name, meta: Metadata, a: A): Parse[A] =
    parse(SwitchParser(flag, meta, a))

  def terminatorx[A: Read, B](flag: Name, meta: Metadata, f: Option[A] => B): Parse[B] =
    parse(FlagParser(flag, meta, Read.of[A].option).map(f))

  def switch(flag: Name, meta: Metadata): Parse[Boolean] =
    parse(SwitchParser(flag, meta, true)) ||| false.pure[Parse]

  def flag[A: Read](flag: Name, meta: Metadata): Parse[A] =
    parse(FlagParser(flag, meta, Read.of[A]))

  def argument[A: Read](meta: Metadata): Parse[A] =
    parse(ArgumentParser(meta, Read.of[A]))

  def arguments[A: Read](meta: Metadata): Parse[List[A]] =
    argument(meta).many

  def subcommand[A](sub: Command[A]): Parse[A] =
    parse(CommandParser(sub))

  def empty: Metadata =
    Metadata(None, None, true)
  def hidden: Metadata =
    Metadata(None, None, false)
  def metavar(d: String): Metadata =
    Metadata(None, d.some, true)
  def description(d: String): Metadata =
    Metadata(d.some, None, true)

  def short(s: Char): Name =
    ShortName(s)
  def long(l: String): Name =
    LongName(l)
  def both(s: Char, l: String): Name =
    BothName(s, l)
}
