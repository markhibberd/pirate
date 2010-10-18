package io.mth.pirate

import scalaz._

trait Pirate[A] {
  def fold[T](
    flag: String => String => String => (A => A) => T,
    line: String => List[Pirate[A]] => T
  ): T

  def usage: String = fold(
     short => long => desc => f => short + ", " + long + "\t\t" + desc,
     command => flags => command + " [OPTIONS]\n\t\t" + flags.map(_.usage).mkString("\n\t\t")
  )

  def parserise: PirateParser[A => A] = {
    import PirateParser._
    fold(
      short => long => desc => f => flag(short, long).lift(_ => f),
      command => flags => flatCommandline(flags.map(_.parserise), List())
    )
  }
}


object Pirate {
  def flag[A](short: String, long: String, desc: String, f: A => A): Pirate[A] = new Pirate[A] {
    def fold[T](
      flag: String => String => String => (A => A) => T,
      line: String => List[Pirate[A]] => T
    ) = flag(short)(long)(desc)(f)
  }

  def line[A](command: String, flags: List[Pirate[A]]): Pirate[A] = new Pirate[A] {
    def fold[T](
      flag: String => String => String => (A => A)  => T,
      line: String => List[Pirate[A]] => T
    ) = line(command)(flags)
  }

  def parse[A](p: Pirate[A], args: List[String], a: A): Option[A] =
    p.parserise.parse(args) match {
      case Success((rest, f)) => Some(f(a))
      case Failure(msg) => None
    }
}


