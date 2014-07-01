package io.mth.pirate

import scalaz._, Scalaz._, effect.IO

trait PirateMain[A] {
  def command: Command[A]

  def run(a: A): Unit

  def main(args: Array[String]): Unit =
    Runners.unsafeRunOrFail(args.toList, command, run)
}

trait PirateMainIO[A] {
  def command: Command[A]

  def run(a: A): IO[Unit]

  def main(args: Array[String]): Unit =
    Runners.runOrFail(args.toList, command, run).unsafePerformIO
}
