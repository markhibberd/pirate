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
case class GitRm(force: Boolean, dryRun: Boolean, recurse: Boolean, cached: Boolean, pathspec: List[File]) extends GitCommand

object GitMain extends PirateMainIO[Git] {
  val version: Parse[GitCommand] =
    terminator(long("version") |+| short('v') |+| description("Prints the Git suite version that the git program came from."), GitVersion)

  val help: Parse[GitCommand] =
    terminatorx(long("help") |+| description("Prints the synopsis and a list of the most commonly used commands. If a Git command is named this option will bring up the manual page for that command."), GitHelp.apply)

  val cwd: Parse[Option[String]] =
    flag[String](short('C') |+| metavar("<path>") |+| description("Run as if git was started in <path> instead of the current working directory.")).option

  val conf: Parse[Option[String]] =
    flag[String](short('c') |+| metavar("<name>=<value>") |+| description("Pass a configuration parameter to the command.")).option

  val exec: Parse[Option[String]] =
    flag[String](long("exec-path") |+| metavar("<path>") |+| description("Path to wherever your core Git programs are installed.")).option

  val html: Parse[GitCommand] =
    terminator(long("html-path") |+| description("Print the path, without trailing slash, where Git's HTML documentation is installed and exit."), GitHtmlPath)

  val man: Parse[GitCommand] =
    terminator(long("man-path") |+| description("Print the manpath (see man(1)) for the man pages for this version of Git and exit."), GitManPath)

  val info: Parse[GitCommand] =
    terminator(long("info-path"), GitInfoPath)

  // Applicitive style GitAdd |*| (switch ...) leads to invariance issues.
  val add: Parse[GitCommand] = (switch(long("force") |+| short('f'))
                            |@| switch(long("interactive") |+| short('i'))
                            |@| switch(long("patch") |+| short('p'))
                            |@| switch(long("edit") |+| short('e'))
                            |@| arguments.many[File](metavar("paths")))(GitAdd)

  val rm: Parse[GitCommand] = (switch(long("force") |+| short('f'))
                            |@| switch(long("dry-run") |+| short('n'))
                            |@| switch(short('r'))
                            |@| switch(long("cached"))
                            |@| arguments.many[File](metavar("paths")))(GitRm)

  def git(cmd: Parse[GitCommand]): Parse[Git] =
    Git |*| (cwd, conf, exec, cmd)

  val command: Command[Git] =
    git { version ||| help ||| html ||| man ||| info ||| Flags.command.of(add ~ "add" ~~ "Add file contents to the index") ||| Flags.command.of(rm ~ "rm" ~~ "Remove files from the working tree and from the index") } ~ "git" ~~ "This is a demo of the git command line"

  def run(a: Git) = a.cmd match {
    case GitVersion => IO.putStrLn("git the pirate version")
    case GitHtmlPath => IO.putStrLn("html-path")
    case GitManPath => IO.putStrLn("man-path")
    case GitInfoPath => IO.putStrLn("info-path")
    case GitHelp(x) => x match {
      case None => IO.putStrLn(Usage.print(command))
      case Some(c) => IO.putStrLn(Usage.printSub(command, c))
    }
  }
}

class GitExample extends spec.Spec { def is = s2"""

  Git Examples
  ============

  git --version                            $version
  git --help                               $help
  git --help text                          $helpText
  git --help add                           $helpAdd
  git --help add text                      $helpAddText
  git add files                            $gitAdd
  git rm --dry-run file                    $gitRm

  Git Checks
  ==========

  Name is set                              ${GitMain.command.name == "git"}
  Description is set                       ${GitMain.command.description == Some("This is a demo of the git command line")}

"""

  def run(args: String*): ParseError \/ Git =
    Interpretter.run(GitMain.command.parse, args.toList)

  def version = {
    run("-c", "thing", "--version") must_==
      Git(None, Some("thing"), None, GitVersion).right
  }

  def help = {
    run("-c", "thing", "--help") must_==
      Git(None, Some("thing"), None, GitHelp(None)).right
  }

  def helpAdd = {
    run("--help", "add") must_==
      Git(None, None, None, GitHelp(Some("add"))).right
  }

  def helpText = {
    Usage.print(GitMain.command) must_== ""
  }

  def helpAddText = {
    Usage.printSub(GitMain.command, "add") must_== ""
  }

  def gitAdd = {
    run("add", "one", "two", "three", "-f", "--interactive") must_==
      Git(None, None, None, GitAdd(true, true, false, false, List(new File("one"), new File("two"), new File("three")))).right
  }

  def gitRm = {
    run("rm", "--dry-run", "file", "-c", "thing") must_==
      Git(None, Some("thing"), None, GitRm(false, true, false, false, List(new File("file")))).right
  }
}
