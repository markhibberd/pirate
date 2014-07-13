package io.mth.pirate.example

import io.mth.pirate._, Pirate._
import scalaz._, Scalaz._, effect.IO

case class Git(
  cwd: String,
  conf: String, // FIX support a read like thing for x=y
  exec: String,
  cmd: GitCommand
)
trait GitCommand
object GitVersion extends GitCommand
object GitHtmlPath extends GitCommand
object GitManPath extends GitCommand
object GitInfoPath extends GitCommand
case class GitHelp(command: Option[String]) extends GitCommand


object GitExample extends PirateMainIO[Git] {
  val version: Parse[GitCommand] =
    terminator("version", GitVersion)

  val help: Parse[GitCommand] =
    terminatorx("help", GitHelp.apply)

  val cwd: Parse[String] =
    flag('C', "<path>")

  val conf: Parse[String] =
    flag('c', "<name>=<value>")

  val exec: Parse[String] =
    flag("exec-path", "<path>") // FIX fork on arg terminator vs option

  val html: Parse[GitCommand] =
    terminator("html-path", GitHtmlPath)

  val man: Parse[GitCommand] =
    terminator("man-path", GitManPath)

  val info: Parse[GitCommand] =
    terminator("info-path", GitInfoPath)

  def git(cmd: Parse[GitCommand]): Parse[Git] =
    Git |*| (cwd, conf, exec, cmd)

  val command: Command[Git] =
    (git { version } |||
     git { help } |||
     git { html } |||
     git { man } |||
     git { info }) ~ "git"

  def run(a: Git) = a.cmd match {
    case GitVersion => IO.putStrLn("git the pirate version")
    case GitHtmlPath => IO.putStrLn("html-path")
    case GitManPath => IO.putStrLn("man-path")
    case GitInfoPath => IO.putStrLn("info-path")
    case GitHelp(x) => x match {
      case None => IO.putStrLn(Usage.print(command))
      case Some(c) => sys.error("Implement sub-command usage printing.")
    }
  }
}
