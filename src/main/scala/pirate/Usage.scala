package pirate

import pirate.internal._

import scalaz._, Scalaz._

object Usage {

  def printError[A](command: Command[A], ctx: List[String], error: ParseError, mode: Prefs): List[String] \/ List[String] = error match {
    case ParseErrorNoMessage =>
      List(Usage.print(command, ctx, mode)).left
    case ParseErrorShowHelpText(s) =>
      List(s.cata(sub => Usage.print(command, ctx ++ sub.pure[List], mode), Usage.print(command, ctx, mode))).right
    case ParseErrorShowVersion(version) =>
      List("version " + version).right
    case ParseErrorLeftOver(xs) =>
      List(s"Left over arguments: " ++ xs.mkString(", "), Usage.print(command, ctx, mode)).left
    case ParseErrorMessage(s) =>
      List(s, Usage.print(command, ctx, mode)).left
    case ParseErrorMissing(s) =>
      List(Usage.missing(command, s, mode), Usage.print(command, ctx, mode)).left
    case ParseErrorInvalidOption(s) =>
      List(Usage.invalid(s, true), Usage.print(command, ctx, mode)).left
    case ParseErrorInvalidArgument(s) =>
      List(Usage.invalid(s, false), Usage.print(command, ctx, mode)).left
  }

  def print[A](command: Command[A], context: List[String], mode: Prefs): String = context match {
    case Nil => Render.infos(command.name, command.description, tree(command.parse), mode)
    case x :: xs =>
      Infos(tree(command.parse).flatten).commands.find(c => c.name === x).map {
        sub => print(Command(command.name + " " + sub.name, sub.description, sub.parse), xs, mode)
      }.getOrElse("Invalid subcommand")
  }

  def tree[A](parse: Parse[A]): ParseTree[Info] =
    ParseTraversal.treeTraverse(parse, new TreeTraverseF[Info] {
      def run[X](info: OptHelpInfo, p: Parser[X]): Info =
        flags(p, info)
    })

  def flags[X](p: Parser[X], info: OptHelpInfo): Info = p match {
    case SwitchParser(flag, meta, a) =>
      SwitchInfo(flag, meta.description, info.multi, info.dfault)
    case FlagParser(flag, meta, p) =>
      FlagInfo(flag, meta.description, meta.metavar, info.multi, info.dfault)
    case CommandParser(sub) =>
      CommandInfo(sub.name, sub.description, sub.parse.map(_ => ()))
    case ArgumentParser(meta, p) =>
      ArgumentInfo(meta.metavar, meta.description, info.multi, info.dfault)
  }

  def invalid(arg: String, isOption: Boolean): String = isOption match {
    case true  => s"Invalid option `${arg}`"
    case false => s"Invalid argument `${arg}`"
  }

  def missing[A](command: Command[A], fails: ParseTree[Info], mode: Prefs): String = {
    Render.info(command.name, command.description, fails, mode).missing
  }
}

object Render {
  import Text._

  def infos(name: String, description: Option[String], is: ParseTree[Info], mode: Prefs): String =
    info(name, description, is, mode).full

  case class info(name: String, description: Option[String], tree: ParseTree[Info], mode: Prefs) {
    val flagspace = space(mode.flagIndent)
    val i = Infos(tree.flatten)

    def synopsis = {
      def synposisLine(t: ParseTree[Info]) =
        s"${flagspace}${wrap(name,mode.flagIndent)(foldTree(t), mode.width - name.length, name.length + mode.flagIndent + 1)}"
      if (mode.condenseSynopsis)
        "[OPTIONS] " + i.arguments.map(argx).mkString(" ")
      else {
        if (mode.separateTopLevels) {
          tree match {
            case ParseTreeAlt(subTrees) => subTrees.map(synposisLine).mkString("\n")
            case ParseTreeAp(_) => synposisLine(tree)
            case ParseTreeLeaf(_) => synposisLine(tree)
          }
        } else synposisLine(tree)
      }
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
      case a: ArgumentInfo => argx(a) |> mDfault(a.dfault)
      case c: CommandInfo  => c.name + " ARGS..."
    }

    def argx(a: ArgumentInfo): String =
      a.meta.cata(x => x , "ARG") ++ { if (a.multi) "..." else "" }

    def flaginfo(f: SwitchInfo): String =
      wrap(flag(f.flag), mode.flagIndent)(f.description.getOrElse(""), mode.width - mode.descIndent, mode.descIndent)

    def optioninfo(o: FlagInfo): String =
      wrap(option(o.flag, o.meta), mode.flagIndent)(o.description.getOrElse(""), mode.width - mode.descIndent, mode.descIndent)

    def argumentinfo(a: ArgumentInfo): String =
      wrap(argx(a), mode.flagIndent)(a.description.getOrElse(""), mode.width - mode.descIndent, mode.descIndent)

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
      flag(f) + meta.cata(x => " " + x, " OPT")

    def mDfault(mark: Boolean)(opt: String): String =
      if (mark) s"[${opt}]" else opt

    def availableOptions: String =
      if (i.switches.length == 0 && i.flags.length == 0) ""
      else
        s"""|Available options:
            |${flagspace}${(i.switches.map(flaginfo) ++ i.flags.map(optioninfo)).distinct.mkString("\n" + flagspace)}
            |""".stripMargin

    def availableArguments: String =
      if (i.arguments.length == 0) ""
      else
        s"""|Positional arguments:
            |${flagspace}${i.arguments.map(argumentinfo).distinct.mkString("\n" + flagspace)}
            |""".stripMargin

    def availableCommands: String =
      if (i.commands.length == 0) ""
      else
        s"""|Available commands:
            |${flagspace}${(i.commands.map(commandinfo)).distinct.mkString("\n" + flagspace)}
            |""".stripMargin

    def full: String =
      s"""|Usage:
          |${synopsis}
          |
          |${description.map(_ + "\n").getOrElse("")}
          |${availableOptions}
          |${availableArguments}
          |${availableCommands}
          |""".stripMargin

    def missing: String =
      s"""|${wrap("Missing parameters:",0)(synopsis, mode.width - name.length, name.length + mode.flagIndent + 1)}
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
case class SwitchInfo(flag: Name, description: Option[String], multi: Boolean, dfault: Boolean) extends Info
case class FlagInfo(flag: Name, description: Option[String], meta: Option[String], multi: Boolean, dfault: Boolean) extends Info
case class ArgumentInfo(meta: Option[String], description: Option[String], multi: Boolean, dfault: Boolean) extends Info
case class CommandInfo(name: String, description: Option[String], parse: Parse[Unit]) extends Info
