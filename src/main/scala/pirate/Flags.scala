package pirate

import scalaz._, Scalaz._

object Flags extends Flags

trait Flags {
  private def parse[A](p: Parser[A]): Parse[A] =
    ParserParse(p)

  // A simple parser to show the help text without disrupting the main parser
  def helper =
    abort(short('h') |+| long("help") |+| description("Prints the synopsis and a list of options and arguments."), ShowHelpText(None))

  // A helper which optionally shows the help text for subcommands.
  def helperX: Parse[Option[Unit]] =
    parse(FlagParser(short('h') |+| long("help") |+| description("Prints the synopsis and a list of the most commonly used commands. If a subcommand is named this option will show the synposis for said command."), Read.string.option.flatMap(s => Read.error[Unit](ShowHelpText(s))))).option

  def abort(meta: Metadata, error: ReadError): Parse[Option[Unit]] =
    parse(FlagParser(meta, Read.error(error))).option

  def terminator[A](meta: Metadata, a: A): Parse[A] =
    parse(SwitchParser(meta, a))

  def terminatorx[A: Read, B](meta: Metadata, f: Option[A] => B): Parse[B] =
    parse(FlagParser(meta, Read.of[A].option).map(f))

  def switch(meta: Metadata): Parse[Boolean] =
    parse(SwitchParser(meta, true)) ||| false.pure[Parse]

  def flag[A: Read](meta: Metadata): Parse[A] =
    parse(FlagParser(meta, Read.of[A]))

  def argument[A: Read](meta: Metadata): Parse[A] =
    parse(ArgumentParser(meta, Read.of[A]))

  def arguments[A: Read](meta: Metadata): Parse[List[A]] =
    argument(meta).many

  def subcommand[A](sub: Command[A]): Parse[A] =
    parse(CommandParser(sub))

  def hidden: Metadata = Metadata(None, None, None, false)
  def metavar(d: String): Metadata = Metadata(None, None, d.some, true)
  def description(d: String): Metadata = Metadata(None, d.some, None, true)
  implicit def NameSyntax(n: Name): Metadata = Metadata(n.some, None, None, true)
  def short(s: Char) = Metadata(ShortName(s).some, None, None, true)
  def long(l: String) = Metadata(LongName(l).some, None, None, true)
}
