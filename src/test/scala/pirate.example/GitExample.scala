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
case class GitAdd(force: Boolean, interactive: Boolean, patch: Boolean, edit: Boolean, pathspec: List[File]) extends GitCommand
case class GitRm(force: Boolean, dryRun: Boolean, recurse: Boolean, cached: Boolean, pathspec: List[File]) extends GitCommand

object GitMain extends PirateMainIO[Git] {
  val version: Parse[GitCommand] =
    terminator(long("version") |+| short('v') |+| description("Prints the Git suite version that the git program came from."), GitVersion)

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

  val add: Parse[GitCommand] = GitAdd |*| (switch(long("force") |+| short('f'))
                             , switch(long("interactive") |+| short('i'))
                             , switch(long("patch") |+| short('p'))
                             , switch(long("edit") |+| short('e'))
                             , arguments[File](metavar("paths")))

  val rm: Parse[GitCommand] = GitRm |*| (switch(long("force") |+| short('f'))
                             , switch(long("dry-run") |+| short('n'))
                             , switch(short('r'))
                             , switch(long("cached"))
                             , arguments[File](metavar("paths")))

  def git(cmd: Parse[GitCommand]): Parse[Git] =
    (Git |*| (cwd, conf, exec, cmd)) <* helperX

  val command: Command[Git] =
    git { version ||| html ||| man ||| info ||| subcommand(add ~ "add" ~~ "Add file contents to the index") ||| subcommand(rm ~ "rm" ~~ "Remove files from the working tree and from the index") } ~ "git" ~~ "This is a demo of the git command line"

  def run(a: Git) = a.cmd match {
    case GitVersion => IO.putStrLn("git the pirate version")
    case GitHtmlPath => IO.putStrLn("html-path")
    case GitManPath => IO.putStrLn("man-path")
    case GitInfoPath => IO.putStrLn("info-path")
    case GitAdd(force, interactive, patch, edit, pathspec) => IO.putStrLn("Removing files")
    case GitRm(force, dryRun, recurse, cached, pathspec) => IO.putStrLn("Removing files")
  }
}

class GitExample extends spec.Spec { def is = s2"""

  Git Examples
  ============

  git --version                            $version
  git --help                               $help
  git --help text                          $helpText
  git --help add                           $helpAdd
  git add                                  $helpContext
  git --help add text                      $helpAddText
  git add files                            $gitAdd
  git rm --dry-run file                    $gitRm

  Git Checks
  ==========

  Name is set                              ${GitMain.command.name == "git"}
  Description is set                       ${GitMain.command.description == Some("This is a demo of the git command line")}

"""

  def run(args: String*): (List[String], ParseError \/ Git) =
    Interpreter.run(GitMain.command.parse, args.toList)

  def version = {
    run("-c", "thing", "--version") must_==
      Nil -> Git(None, Some("thing"), None, GitVersion).right
  }

  def help = {
    run("-c", "thing", "--help") must_==
      Nil -> ParseErrorShowHelpText(None).left
  }

  def helpContext = {
    run("--help", "add") must_==
      Nil -> ParseErrorShowHelpText(Some("add")).left
  }

  def helpAdd = {
    run("add", "--help") must_==
      List("add") -> ParseErrorShowHelpText(None).left
  }

  def helpText = {
    Usage.print(GitMain.command, Nil) must_== ""
  }.pendingUntilFixed

  def helpAddText = {
    Usage.print(GitMain.command, List("add")) must_== ""
  }.pendingUntilFixed

  def gitAdd = {
    run("add", "one", "two", "three", "-f", "--interactive") must_==
      List("add") -> Git(None, None, None, GitAdd(true, true, false, false, List(new File("one"), new File("two"), new File("three")))).right
  }

  def gitRm = {
    run("rm", "--dry-run", "file", "-c", "thing") must_==
      List("rm") -> Git(None, Some("thing"), None, GitRm(false, true, false, false, List(new File("file")))).right
  }
}
