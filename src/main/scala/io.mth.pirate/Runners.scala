package io.mth.pirate

import scalaz._, Scalaz._, effect.IO

trait Runners {
  def run[A, B](args: List[String], command: Command[A], f: A => B): PError \/ B  =
    Interpretter.run(command.parse, args).map(f)

  def runOrFail[A](args: List[String], command: Command[A], f: A => IO[Unit]): IO[Unit]  =
    run[A, IO[Unit]](args, command, f) match {
      case -\/(e) => e match {
        case PErrorNoMessage => IO {
          Console.err.print(Usage.print(command))
        }
        case PErrorMessage(s) => IO {
          Console.err.println(s)
          Console.err.print(Usage.print(command))
        }
      }
      case \/-(v) => v
    }

  def unsafeRunOrFail[A](args: List[String], command: Command[A], f: A => Unit): Unit =
    runOrFail[A](args, command, a => IO { f(a) }).unsafePerformIO
}

object Runners extends Runners
