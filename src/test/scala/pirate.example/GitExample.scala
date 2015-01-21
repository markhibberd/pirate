package pirate.example

import pirate._, Pirate._
import scalaz._, Scalaz._, effect.IO

import java.io.File

case class Git(
  cwd: Option[String],
  conf: Option[String],
  exec: Option[String],
  cmd: GitCommand
)
trait GitCommand
object GitVersion extends GitCommand
object GitHtmlPath extends GitCommand
object GitManPath extends GitCommand
object GitInfoPath extends GitCommand
case class GitHelp(command: Option[String]) extends GitCommand
case class GitAdd(force: Boolean, interactive: Boolean, patch: Boolean, edit: Boolean, pathspec: List[File]) extends GitCommand

object GitMain extends PirateMainIO[Git] {
  val version: Parse[GitCommand] =
    terminator(long("version"), GitVersion)

  val help: Parse[GitCommand] =
    terminatorx(long("help"), GitHelp.apply)

  val cwd: Parse[Option[String]] =
    flag[String](short('C') |+| description("<path>")).option

  val conf: Parse[Option[String]] =
    flag[String](short('c') |+| description("<name>=<value>")).option

  val exec: Parse[Option[String]] =
    flag[String](long("exec-path") |+| description("<path>")).option

  val html: Parse[GitCommand] =
    terminator(long("html-path"), GitHtmlPath)

  val man: Parse[GitCommand] =
    terminator(long("man-path"), GitManPath)

  val info: Parse[GitCommand] =
    terminator(long("info-path"), GitInfoPath)

  // Applicitive style if GitAdd |*| (switch ...) leads to invariance issues.
  val add: Parse[GitCommand] = (switch(long("force") |+| short('f'))
                            |@| switch(long("interactive") |+| short('i'))
                            |@| switch(long("patch") |+| short('p'))
                            |@| switch(long("edit") |+| short('e'))
                            |@| arguments.many[File](metavar("paths")))(GitAdd(_, _, _, _, _))

  def git(cmd: Parse[GitCommand]): Parse[Git] =
    Git |*| (cwd, conf, exec, cmd)

  val command: Command[Git] =
    (git { version } |||
     git { help } |||
     git { html } |||
     git { man } |||
     git { info } |||
     git { Flags.command.of("add", add) }) ~ "git" ~~
      "This is a demo of the git command line"

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

class GitExample extends spec.Spec { def is = s2"""

  Git Examples
  ============

  git --version                            $version
  git --help                               $help
  git --help status                        $helpAt
  git add files                            $gitAdd

  Git Checks
  ==========

  Name is set                              ${GitMain.command.name == "git"}
  Description is set                       ${GitMain.command.description == Some("This is a demo of the git command line")}

"""

  def run(args: String*): ParseError \/ Git =
    Interpretter.run(GitMain.command.parse, args.toList)

  def version = {
    run("--version") must_==
      Git(None, None, None, GitVersion).right
  }

  def help = {
    run("--help") must_==
      Git(None, None, None, GitHelp(None)).right
  }

  def helpAt = {
    run("--help", "status") must_==
      Git(None, None, None, GitHelp(Some("status"))).right
  }

  def gitAdd = {
    run("add", "one", "two", "three", "-f", "--interactive") must_==
      Git(None, None, None, GitAdd(true, true, false, false, List(new File("one"), new File("two"), new File("three")))).right
  }
}
