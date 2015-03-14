package pirate

import scalaz._, effect.IO

trait Runners {
  def run[A](args: List[String], command: Command[A]): ParseError \/ A =
    Interpreter.run(command.parse, args)._2

  def runOrFail[A](args: List[String], command: Command[A]): IO[A]  =
    Interpreter.run(command.parse, args) match {
      case (ctx, -\/(e)) => e match {
        case ParseErrorNoMessage => IO {
          Console.err.print(Usage.print(command, ctx))
          sys.exit(1)
        }
        case ParseErrorShowHelpText(s) => IO {
          s match {
            case None      => Console.out.print(Usage.print(command, ctx))
            case Some(sub) => Console.out.print(Usage.print(command, sub :: ctx))
          }
          sys.exit(0)
        }
        case ParseErrorShowVersion(version) => IO {
          Console.out.print("version "+version)
          sys.exit(0)
        }
        case ParseErrorMessage(s) => IO {
          Console.err.println(s)
          Console.err.print(Usage.print(command, ctx))
          sys.exit(1)
        }
        case ParseErrorMissing(s) => IO {
          Console.err.print(Usage.missing(command, s))
          Console.err.print(Usage.print(command, ctx))
          sys.exit(1)
        }
        case ParseErrorInvalidOption(s) => IO {
          Console.err.println(Usage.invalid(s, true))
          Console.err.print(Usage.print(command, ctx))
          sys.exit(1)
        }
        case ParseErrorInvalidArgument(s) => IO {
          Console.err.println(Usage.invalid(s, false))
          Console.err.print(Usage.print(command, ctx))
          sys.exit(1)
        }
      }
      case (_,\/-(v)) =>
        IO(v)
    }
}

object Runners extends Runners
