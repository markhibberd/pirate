package pirate

import scalaz._, effect.IO

trait PirateMain[A] {
  def command: Command[A]

  def run(a: A): Unit

  def prefs: Prefs = NullPrefs

  def main(args: Array[String]): Unit =
    Runners.runOrFail(args.toList, command, prefs).map(run).unsafePerformIO
}

trait PirateMainIO[A] {
  def command: Command[A]

  def run(a: A): IO[Unit]

  def prefs: Prefs = NullPrefs

  def main(args: Array[String]): Unit =
    Runners.runOrFail(args.toList, command, prefs).flatMap(run).unsafePerformIO
}
