package pirate

import scalaz._, effect.IO

trait PirateMain[A] {
  def command: Command[A]

  def run(a: A): Unit

  def main(args: Array[String]): Unit =
    Runners.runOrFail(args.toList, command).map(run).unsafePerformIO
}

trait PirateMainIO[A] {
  def command: Command[A]

  def run(a: A): IO[Unit]

  def main(args: Array[String]): Unit =
    Runners.runOrFail(args.toList, command).flatMap(run).unsafePerformIO
}
