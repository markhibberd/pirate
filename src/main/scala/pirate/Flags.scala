package pirate

import scalaz._, Scalaz._

object Flags extends Flags

trait Flags {
  private def parse[A](p: Parser[A]): Parse[A] =
    ParserParse(p)

  def terminator[A](meta: Metadata, a: A): Parse[A] =
    parse(SwitchParser(meta, a))

  def terminatorx[A: Read, B](meta: Metadata, f: Option[A] => B): Parse[B] =
    parse(FlagParser(meta, Read.of[A].option).map(f))

  def switch(meta: Metadata): Parse[Boolean] =
    parse(SwitchParser(meta, true)) ||| false.pure[Parse]

  def flag[A: Read](meta: Metadata): Parse[A] =
    parse(FlagParser(meta, Read.of[A])) ||| ValueParse(None)

  object arguments {
    def one[A: Read](meta: Metadata): Parse[A] =
      parse(ArgumentParser(meta, Read.of[A])) ||| ValueParse(None)

    def some[A: Read](meta: Metadata): Parse[List[A]] = for {
      a <- one(meta.copy(metavar = meta.metavar.map(_ + "...")))
      b <- many(meta)
    } yield (a :: b)

    def many[A: Read](meta: Metadata): Parse[List[A]] =
      some(meta) ||| ValueParse(Some(Nil))
  }

  object command {
    def of[A](name: String, p: Parse[A]): Parse[A] =
      parse(CommandParser(name, p)) ||| ValueParse(None)
  }


  def hidden: Metadata = Metadata(None, None, None, false)
  def metavar(d: String): Metadata = Metadata(None, None, d.some, true)
  def description(d: String): Metadata = Metadata(None, d.some, None, true)
  implicit def NameSyntax(n: Name): Metadata = Metadata(n.some, None, None, true)
  def short(s: Char) = Metadata(ShortName(s).some, None, None, true)
  def long(l: String) = Metadata(LongName(l).some, None, None, true)
}