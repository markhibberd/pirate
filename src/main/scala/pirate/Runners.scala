package pirate

import scalaz._, Scalaz._, effect._

trait Runners {
  def run[A](args: List[String], command: Command[A], prefs: Prefs): ParseError \/ A =
    Interpreter.run(command.parse, args, prefs)._2

  def runOrFail[A](args: List[String], command: Command[A], prefs: Prefs): IO[A] =
    runWithExit(args, command, prefs)
      .flatMap(_.fold(ExitCode.exitWith, IO(_)))

  def runWithExit[A](args: List[String], command: Command[A], prefs: Prefs): IO[ExitCode \/ A] =
    Interpreter.run(command.parse, args, prefs) match {
      case (ctx, -\/(e)) => IO(
        Usage.printError(command, ctx, e, prefs).fold(
          l => {l.foreach(Console.err.println); ExitCode.failure(1)},
          l => {l.foreach(Console.out.println); ExitCode.success}
        ).left
      )
      case (_, \/-(v)) =>
        IO(v.right)
    }
}

object Runners extends Runners
