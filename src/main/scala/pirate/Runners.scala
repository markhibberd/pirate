package pirate

import scalaz._, Scalaz._, effect._

trait Runners {
  def run[A](args: List[String], command: Command[A]): ParseError \/ A =
    Interpreter.run(command.parse, args)._2

  def runOrFail[A](args: List[String], command: Command[A]): IO[A] =
    runWithExit(args, command)
      .flatMap(_.fold(ExitCode.exitWith, IO(_)))

  def runWithExit[A](args: List[String], command: Command[A]): IO[ExitCode \/ A] =
    Interpreter.run(command.parse, args) match {
      case (ctx, -\/(e)) => IO(
        Usage.printError(command, ctx, e).fold(
          l => {l.foreach(Console.err.println); ExitCode.failure(1)},
          l => {l.foreach(Console.out.println); ExitCode.success}
        ).left
      )
      case (_, \/-(v)) =>
        IO(v.right)
    }
}

object Runners extends Runners
