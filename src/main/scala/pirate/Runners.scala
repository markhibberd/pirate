package pirate

import scalaz._, effect.IO

trait Runners {
  def run[A, B](args: List[String], command: Command[A], f: A => B): (List[String], ParseError \/ B) = {
    val (ctx, a) = Interpreter.run(command.parse, args)
    ctx -> a.map(f)
  }

  def runOrFail[A](args: List[String], command: Command[A], f: A => IO[Unit]): IO[Unit]  =
    run[A, IO[Unit]](args, command, f) match {
      case (ctx, -\/(e)) => e match {
        case ParseErrorNoMessage => IO {
          Console.err.print(Usage.print(command, ctx))
        }
        case ParseErrorShowHelpText(s) => IO {
          s match {
            case None      => Console.err.print(Usage.print(command, ctx))
            case Some(sub) => Console.err.print(Usage.print(command, sub :: ctx))
          }
        }
        case ParseErrorMessage(s) => IO {
          Console.err.println(s)
          Console.err.print(Usage.print(command, ctx))
        }
        case ParseErrorMissing(s) => IO {
          Console.err.print(Usage.missing(command, s))
          Console.err.print(Usage.print(command, ctx))
        }
        case ParseErrorInvalidOption(s) => IO {
          Console.err.println(Usage.invalid(s, true))
          Console.err.print(Usage.print(command, ctx))
        }
        case ParseErrorInvalidArgument(s) => IO {
          Console.err.println(Usage.invalid(s, false))
          Console.err.print(Usage.print(command, ctx))
        }
      }
      case (_,\/-(v)) => v
    }

  def unsafeRunOrFail[A](args: List[String], command: Command[A], f: A => Unit): Unit =
    runOrFail[A](args, command, a => IO { f(a) }).unsafePerformIO
}

object Runners extends Runners
