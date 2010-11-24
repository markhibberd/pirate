package io.mth.pirate

/**
 * Hell'a ordinary usage mode printer.
 */
// FIX Clean this mess up
object Usage {
  def usageForMode[A](mode: UsageMode)(pirate: Program[A]): String = {
    import Text._

    def flagspace = space(mode.flagIndent)

    def render(p: Command[A]): String = p.fold(
      command => flags => positionals =>
          "Usage:\n" +
                flagspace + command + " " + wrappedsynopsisp(p, command) + "\n" +
          "Options: \n" +
                flagspace + flags.map(usagef(_)).mkString("\n" + flagspace)
    )

    def renderWithDescription(p: Command[A], d: String): String = p.fold(
      command => flags => positionals =>
          "Usage:\n" +
                flagspace + command + " " + wrappedsynopsisp(p, command) + "\n" +
          d + "\n" +               
          "Options: \n" +
                flagspace + flags.map(usagef(_)).mkString("\n" + flagspace)
    )

    def renderProgram(p: Program[A]): String = p.fold(
      render(_),
      c => d => renderWithDescription(c, d),
      cs => error("todo"),
      p => s => error("todo")
    )


    def usagef(f: Flag[A]) =
        flaguse(f) + "\n" + space(mode.flagIndent + mode.descIndent) + wrap(flagdescription(f),  mode.width - mode.flagIndent - mode.descIndent,  mode.flagIndent + mode.descIndent)

    def synopsisf(f: Flag[A]) = flagsynopsis(f)

    def wrappedsynopsisp(p: Command[A], command: String) =
      wrap(synopsisp(p), mode.width - mode.flagIndent - command.length, mode.flagIndent + command.length)

    def synopsisp(p: Command[A]) =
      if (mode.condenseSynopsis)
        "[OPTIONS] " + p.fold(
          command => flags => positional => positional.map(paramsyopsis(_)).mkString(" ")
        ).mkString(" ")
      else p.fold(
         command => flags => positional => flags.map(synopsisf(_)).mkString(" ") + " " + positional.map(paramsyopsis(_)).mkString(" ")
      )

    def flagdescription[A](f: Flag[A]): String =
      f.fold(
        s => l => d => f => d,
        s => l => d => m => f => d,
        f => ""
      )

    def flaguse[A](f: Flag[A]): String =
      f.fold(
        s => l => d => f => s.map("-" + _).getOrElse("") + (if (s.isDefined && l.isDefined) "," else "") + l.getOrElse(""),
        s => l => d => m => f => s.map("-" + _).getOrElse("") + (if (s.isDefined && l.isDefined) "," else "") + l.getOrElse("") + "=" + m ,
        f => ""
      )

    def flagsynopsis[A](f: Flag[A]): String =
      f.fold(
        s => l => d => f => "[" + s.map("-" + _).getOrElse("") + (if (s.isDefined && l.isDefined) "|" else "") + l.getOrElse("") + "]",
        s => l => d => m => f => "[" + s.map("-" + _).getOrElse("") + (if (s.isDefined && l.isDefined) "|" else "") + l.getOrElse("") + " " + m + "]",
        f => ""
      )

    def paramsyopsis[A](p: Positional[A]): String = p.fold(
        m => f => m,
        n => m => f => (for (_ <- 1 to n) yield m).mkString(" "),
        m => f => " [" + m + " ...]",
        m => f => m + " [" + m + " ...]",
        ps => ""
      )


    renderProgram(pirate)
  }

  def usage[A](pirate: Program[A]) = usageForMode(DefaultUsageMode)(pirate)
}

case class UsageMode(
  condenseSynopsis: Boolean,
  flagIndent: Int,
  descIndent: Int,
  width: Int
)

object DefaultUsageMode extends UsageMode(
  condenseSynopsis = false,
  flagIndent = 8,
  descIndent = 12,
  width = 80
)
