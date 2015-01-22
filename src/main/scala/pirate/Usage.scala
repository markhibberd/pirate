package pirate

import pirate.internal._

import scalaz._, Scalaz._

object Usage {
  def print[A](command: Command[A]): String =
    printWith(command, DefaultUsageMode)

  def printWith[A](command: Command[A], mode: UsageMode): String =
    Render.infos(command.name, command.description, info(command.parse), mode)

  def info[A](parse: Parse[A]): List[Info] =
    ParseTraversal.treeTraverse(parse, new TreeTraverseF[Option[Info]] {
      def run[X](info: OptHelpInfo, p: Parser[X]): Option[Info] =
        flags(p, info)
    }) match {
      case ParseTreeAlt(children) => children.map(_.flatten.flatten.suml)
      case x => List(x.flatten.flatten.suml)
    }

  def flags[X](p: Parser[X], info: OptHelpInfo): Option[Info] = p match {
    case SwitchParser(meta, a) => if (meta.visible)
      Some(Info(SwitchInfo(meta.names.get, meta.description, info.dfault) :: Nil, Nil, Nil, Nil)) else None
    case FlagParser(meta, p) => if (meta.visible)
      Some(Info(Nil, FlagInfo(meta.names.get, meta.description, meta.metavar, info.dfault) :: Nil, Nil, Nil)) else None
    case CommandParser(sub) =>
      Some(Info(Nil, Nil, Nil, CommandInfo(sub.name, sub.description, Usage.info(sub.parse)) :: Nil))
    case ArgumentParser(meta, p) =>
      Some(Info(Nil, Nil, ArgumentInfo(meta.metavar, meta.description, info.multi) :: Nil, Nil))
  }
}

object Render {
  import Text._

  def infos(name: String, description: Option[String], is: List[Info], mode: UsageMode): String =
    is.map(info(name, description, _, mode).full).mkString("\n")

  case class info(name: String, description: Option[String], i: Info, mode: UsageMode) {
    val flagspace = space(mode.flagIndent)

    def synopsis =
      if (mode.condenseSynopsis)
        "[OPTIONS] " + i.arguments.map(argx).mkString(" ")
      else
        (i.switches.map(f => flag(f.flag) |> mDfault(f.dfault)) ++ i.flags.map(o => option(o.flag, o.meta) |> mDfault(o.dfault)) ++ i.arguments.map(argx)).mkString(" ")

    def argx(a: ArgumentInfo): String =
      a.meta.cata(x => x , "ARG") ++ { if (a.multi) "..." else "" }

    def flaginfo(f: SwitchInfo): String =
      wrap(flag(f.flag), mode.flagIndent)(f.description.getOrElse(""), mode.width - mode.descIndent, mode.descIndent)

    def optioninfo(o: FlagInfo): String =
      wrap(option(o.flag, o.meta), mode.flagIndent)(o.description.getOrElse(""), mode.width - mode.descIndent, mode.descIndent)

    def commandinfo(c: CommandInfo): String =
      c.info.map(in => wrap(c.name, mode.flagIndent)(c.description.getOrElse(""), mode.width - mode.descIndent, mode.descIndent)).mkString("\n")

    def flag(f: Name): String = f match {
      case ShortName(s) => s"-${s}"
      case LongName(l) => s"--${l}"
      case BothName(s, l) => s"-${s}|--${l}"
    }

    def option(f: Name, meta: Option[String]): String =
      flag(f) + meta.cata(x => " " + x, "")

    def mDfault(mark: Boolean)(opt: String): String =
      if (mark) s"[${opt}]" else opt

    def availableOptions = if (i.switches.length == 0) "" else
      s"""|Available options:
          |${flagspace}${(i.switches.map(flaginfo) ++ i.flags.map(optioninfo)).mkString("\n" + flagspace)}
          |""".stripMargin

    def availableCommands = if (i.commands.length == 0) "" else
      s"""|Available commands:
          |${flagspace}${(i.commands.map(commandinfo)).mkString("\n" + flagspace)}
          |""".stripMargin

    def full = s"""|Usage:
        |${flagspace}${name} ${synopsis}
        |
        |${description.map(_ + "\n").getOrElse("")}
        |${availableOptions}
        |${availableCommands}
        |""".stripMargin
  }
}


case class Info(
  switches: List[SwitchInfo],
  flags: List[FlagInfo],
  arguments: List[ArgumentInfo],
  commands: List[CommandInfo]
)

case class SwitchInfo(flag: Name, description: Option[String], dfault: Boolean)
case class FlagInfo(flag: Name, description: Option[String], meta: Option[String], dfault: Boolean)
case class ArgumentInfo(meta: Option[String], description: Option[String], multi: Boolean)
case class CommandInfo(name: String, description: Option[String], info: List[Info])
object Info {
  implicit def InfoMonoid: Monoid[Info] = new Monoid[Info] {
    def zero = Info(Nil, Nil, Nil, Nil)
    def append(i1: Info, i2: => Info) = Info(i1.switches ++ i2.switches, i1.flags ++ i2.flags, i1.arguments ++ i2.arguments, i1.commands ++ i2.commands)
  }
}


/**
 * Usage mode provides configuration options for generating
 * a usage string.
 */
case class UsageMode(
  condenseSynopsis: Boolean,
  flagIndent: Int,
  descIndent: Int,
  width: Int,
  tightOneOrManySynopsis: Boolean
)

/**
 * Default usage mode.
 *  - Explicit synopsis.
 *  - 8/16 indents
 *  - 80 width
 */
object DefaultUsageMode extends UsageMode(
  condenseSynopsis = false,
  flagIndent = 2,
  descIndent = 26,
  width = 80,
  tightOneOrManySynopsis = true
)
