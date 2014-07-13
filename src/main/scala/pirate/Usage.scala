package pirate

import pirate.internal._

import scalaz._, Scalaz._

object Usage {
  def print[A](command: Command[A]): String =
    printWith(command, DefaultUsageMode)

  def printWith[A](command: Command[A], mode: UsageMode): String =
    Render.infos(command.name, command.description, info(command), mode: UsageMode)

  def info[A](command: Command[A]): List[Info] =
    ParseTraversal.treeTraverse(command.parse, new TreeTraverseF[Option[Info]] {
      def run[X](info: OptHelpInfo, p: Parser[X], m: Metadata): Option[Info] =
        flags(p, m, info)
    }) match {
      case ParseTreeAlt(children) => children.map(_.flatten.flatten.suml)
      case x => List(x.flatten.flatten.suml)
    }

  def flags[X](p: Parser[X], m: Metadata, info: OptHelpInfo): Option[Info] = p match {
    case SwitchParser(flag, a) if m.visible =>
      Some(Info(SwitchInfo(flag, m.description) :: Nil, Nil, Nil, Nil))
    case FlagParser(flag, metas, p) if m.visible =>
      Some(Info(Nil, FlagInfo(flag, m.description, metas, info.dfault) :: Nil, Nil, Nil))
    case CommandParser(name, p) if m.visible =>
      Some(Info(Nil, Nil, Nil, CommandInfo(name, None) :: Nil))
    case ArgumentParser(p) =>
      Some(Info(Nil, Nil, ArgumentInfo(Nil) :: Nil, Nil))
  }
}

object Render {
  import Text._

  def infos(name: String, description: Option[String], is: List[Info], mode: UsageMode): String =
    is.map(info(name, description, _, mode)).mkString("\n")

  def info(name: String, description: Option[String], i: Info, mode: UsageMode): String = {
    val flagspace = space(mode.flagIndent)

    def synopsis =
      if (mode.condenseSynopsis)
        "[OPTIONS] " + i.arguments.map(_.meta).mkString(" ")
      else
        (i.switches.map(f => flag(f.flag)) ++ i.flags.map(o => option(o.flag, o.metas)) ++ i.arguments.map(_.meta)).mkString(" ")

    def flaginfo(f: SwitchInfo): String =
      flag(f.flag) + "\n" +
        wrap(f.description.getOrElse(""), mode.width - mode.descIndent, mode.descIndent)

    def optioninfo(o: FlagInfo): String =
      option(o.flag, o.metas) + "\n" +
        wrap(o.description.getOrElse(""), mode.width - mode.descIndent, mode.descIndent)

    def flag(f: Name): String = f match {
      case ShortName(s) => s"-${s}"
      case LongName(l) => s"--${l}"
      case BothName(s, l) => s"-${s}|--${l}"
    }

    def option(f: Name, metas: List[String]): String =
      flag(f) + " " + metas.mkString(" ")

    s"""|Usage:
        |${flagspace}${name} ${synopsis}
        |
        |${description.map(_ + "\n").getOrElse("")}
        |Options:
        |${flagspace}${(i.switches.map(flaginfo) ++ i.flags.map(optioninfo)).mkString("\n" + flagspace)}
        |""".stripMargin
  }
}


case class Info(
  switches: List[SwitchInfo],
  flags: List[FlagInfo],
  arguments: List[ArgumentInfo],
  commands: List[CommandInfo]
)

case class SwitchInfo(flag: Name, description: Option[String])
case class FlagInfo(flag: Name, description: Option[String], metas: List[String], dfault: Boolean)
case class ArgumentInfo(meta: List[String])
case class CommandInfo(name: String, description: Option[String])
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
  flagIndent = 8,
  descIndent = 16,
  width = 80,
  tightOneOrManySynopsis = true
)
