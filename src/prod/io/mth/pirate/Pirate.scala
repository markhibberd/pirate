package io.mth.pirate

import scalaz._

trait Pirate[A] {
  def fold[T](
    flag: String => String => String => (A => A) => T,
    positional: String => (A => String => A) => T,
    line: String => List[Pirate[A]] => List[Pirate[A]] => T
  ): T

  def usage: String = fold(
     short => long => desc => f => short + ", " + long + "\t\t" + desc,
     meta => f => meta,
     command => flags => positional => command + " [OPTIONS] " + positional.map(_.usage).mkString(" ") + "\n\t\t" + flags.map(_.usage).mkString("\n\t\t")
  )

  def parserise: PirateParser[A => A] = {
    import PirateParser._
    fold(
      short => long => desc => f => flag(short, long).lift(_ => f),
      meta => f => string.lift(s => f(_)(s)),
      command => flags => positional => flatCommandline(flags.map(_.parserise), positional.map(_.parserise))
    )
  }
}


object Pirate {
  def flag[A](short: String, long: String, desc: String, f: A => A): Pirate[A] = new Pirate[A] {
    def fold[T](
      flag: String => String => String => (A => A) => T,
      positional: String => (A => String => A)  => T,
      line: String => List[Pirate[A]] => List[Pirate[A]] => T
    ) = flag(short)(long)(desc)(f)
  }

  def positional[A](desc: String, f: A => String => A): Pirate[A] = new Pirate[A] {
    def fold[T](
      flag: String => String => String => (A => A) => T,
      positional: String => (A => String => A)  => T,
      line: String => List[Pirate[A]] => List[Pirate[A]] => T
    ) = positional(desc)(f)
  }

  def line[A](command: String, flags: List[Pirate[A]], positionals: List[Pirate[A]]): Pirate[A] = new Pirate[A] {
    def fold[T](
      flag: String => String => String => (A => A)  => T,
      positional: String => (A => String => A)  => T,
      line: String => List[Pirate[A]] => List[Pirate[A]] => T
    ) = line(command)(flags)(positionals)
  }

  def parse[A](p: Pirate[A], args: List[String], a: A): Option[A] =
    p.parserise.parse(args) match {
      case Success((rest, f)) => Some(f(a))
      case Failure(msg) => None
    }
}


