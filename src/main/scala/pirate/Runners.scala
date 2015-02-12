package pirate

import scalaz._, effect.IO

trait Runners {
  def run[A, B](args: List[String], command: Command[A], f: A => B): ParseError \/ B  =
    Interpretter.run(command.parse, args).map(f)

  def runOrFail[A](args: List[String], command: Command[A], f: A => IO[Unit]): IO[Unit]  =
    run[A, IO[Unit]](args, command, f) match {
      case -\/(e) => e match {
        case ParseErrorNoMessage => IO {
          Console.err.print(Usage.print(command))
        }
        case ParseErrorShowHelpText(s) => IO {
          s match {
            case None      => Console.err.print(Usage.print(command))
            case Some(sub) => Console.err.print(Usage.printSub(command, sub))
          }
        }
        case ParseErrorMessage(s) => IO {
          Console.err.println(s)
          Console.err.print(Usage.print(command))
        }
        case ParseErrorMissing(s) => IO {
          Console.err.print(Usage.missing(command, s))
          Console.err.print(Usage.print(command))
        }
        case ParseErrorInvalidOption(s) => IO {
          Console.err.println(Usage.invalid(s, true))
          Console.err.print(Usage.print(command))
        }
        case ParseErrorInvalidArgument(s) => IO {
          Console.err.println(Usage.invalid(s, false))
          Console.err.print(Usage.print(command))
        }
      }
      case \/-(v) => v
    }

  def unsafeRunOrFail[A](args: List[String], command: Command[A], f: A => Unit): Unit =
    runOrFail[A](args, command, a => IO { f(a) }).unsafePerformIO
}

object Runners extends Runners
