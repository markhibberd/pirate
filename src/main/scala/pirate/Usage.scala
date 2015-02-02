package pirate

import pirate.internal._

import scalaz._, Scalaz._

object Usage {
  def print[A](command: Command[A]): String =
    printWith(command, DefaultUsageMode)

  def printWith[A](command: Command[A], mode: UsageMode): String =
    Render.infos(command.name, command.description, tree(command.parse), mode)

  def printSub[A](command: Command[A], sub: String) =
    printSubWith(command: Command[A], sub: String, DefaultUsageMode)

  def printSubWith[A](command: Command[A], sub: String, mode: UsageMode): String =
    Infos(tree(command.parse).flatten).commands.find(c => c.name === sub).map {
      x => Render.infos(command.name + " " + x.name, x.description, x.info, mode)
    }.getOrElse("Invalid subcommand")

  def tree[A](parse: Parse[A]): ParseTree[Info] =
    ParseTraversal.treeTraverse(parse, new TreeTraverseF[Info] {
      def run[X](info: OptHelpInfo, p: Parser[X]): Info =
        flags(p, info)
    })

  def flags[X](p: Parser[X], info: OptHelpInfo): Info = p match {
    case SwitchParser(meta, a) =>
      SwitchInfo(meta.names.get, meta.description, info.dfault)
    case FlagParser(meta, p) =>
      FlagInfo(meta.names.get, meta.description, meta.metavar, info.dfault)
    case CommandParser(sub) =>
      CommandInfo(sub.name, sub.description, Usage.tree(sub.parse))
    case ArgumentParser(meta, p) =>
      ArgumentInfo(meta.metavar, meta.description, info.multi)
  }
}

object Render {
  import Text._

  def infos(name: String, description: Option[String], is: ParseTree[Info], mode: UsageMode): String =
    info(name, description, is, mode).full

  case class info(name: String, description: Option[String], tree: ParseTree[Info], mode: UsageMode) {
    val flagspace = space(mode.flagIndent)
    val i = Infos(tree.flatten)

    def synopsis =
      if (mode.condenseSynopsis)
        "[OPTIONS] " + i.arguments.map(argx).mkString(" ")
      else {
        foldTree(tree)
      }

    def foldTree(x: ParseTree[Info]): String = x match {
      case ParseTreeLeaf(value) => anyInfo(value)
      case ParseTreeAp(children) => children.map(foldTree).mkString(" ")
      case ParseTreeAlt(children) => children.filterNot(_.flatten.isEmpty).map(foldTree) match {
        case l  :: Nil => l
        case ls        => "(" + ls.mkString(" | ") + ")"
      }
    }

    def anyInfo(i: Info): String = i match {
      case f: SwitchInfo   => flagO(f.flag) |> mDfault(f.dfault)
      case o: FlagInfo     => option(o.flag, o.meta) |> mDfault(o.dfault)
      case a: ArgumentInfo => argx(a)
      case c: CommandInfo  => c.name + " ARGS..."
    }

    def argx(a: ArgumentInfo): String =
      a.meta.cata(x => x , "ARG") ++ { if (a.multi) "..." else "" }

    def flaginfo(f: SwitchInfo): String =
      wrap(flag(f.flag), mode.flagIndent)(f.description.getOrElse(""), mode.width - mode.descIndent, mode.descIndent)

    def optioninfo(o: FlagInfo): String =
      wrap(option(o.flag, o.meta), mode.flagIndent)(o.description.getOrElse(""), mode.width - mode.descIndent, mode.descIndent)

    def commandinfo(c: CommandInfo): String =
      wrap(c.name, mode.flagIndent)(c.description.getOrElse(""), mode.width - mode.descIndent, mode.descIndent)

    def flag(f: Name): String = f match {
      case ShortName(s) => s"-${s}"
      case LongName(l) => s"--${l}"
      case BothName(s, l) => s"-${s}|--${l}"
    }

    def flagO(f: Name): String = f match {
      case ShortName(s) => s"-${s}"
      case LongName(l) => s"--${l}"
      case BothName(s, l) => s"(-${s}|--${l})"
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
        |${flagspace}${wrap(name,mode.flagIndent)(synopsis, mode.width - name.length, name.length + mode.flagIndent + 1)}
        |
        |${description.map(_ + "\n").getOrElse("")}
        |${availableOptions}
        |${availableCommands}
        |""".stripMargin
  }
}


case class Infos(l: List[Info]) {
  def switches  = l.collect { case a: SwitchInfo   => a }
  def flags     = l.collect { case a: FlagInfo     => a }
  def arguments = l.collect { case a: ArgumentInfo => a }
  def commands  = l.collect { case a: CommandInfo  => a }
}

sealed trait Info
case class SwitchInfo(flag: Name, description: Option[String], dfault: Boolean) extends Info
case class FlagInfo(flag: Name, description: Option[String], meta: Option[String], dfault: Boolean) extends Info
case class ArgumentInfo(meta: Option[String], description: Option[String], multi: Boolean) extends Info
case class CommandInfo(name: String, description: Option[String], info: ParseTree[Info]) extends Info

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
