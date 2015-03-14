package pirate

import scalaz.effect._

sealed trait ExitCode {

  def fold[A](success: => A, failure: Int => A): A = this match {
    case ExitSuccess =>
      success
    case ExitFailure(code) =>
      failure(code)
  }
}

case object ExitSuccess extends ExitCode
case class ExitFailure(code: Int) extends ExitCode

object ExitCode {

  def success: ExitCode =
    ExitSuccess

  def failure(code: Int): ExitCode =
    ExitFailure(code)

  def exitWith[A](code: ExitCode): IO[A] =
    IO(code.fold(sys.exit(0), sys.exit(_)))
}
