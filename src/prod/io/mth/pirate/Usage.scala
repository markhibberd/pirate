package io.mth.pirate

/**
 * Hell'a ordinary usage mode printer.
 */
// FIX Clean this mess up
object Usage {
  def usageForMode[A](mode: UsageMode)(pirate: Pirate[A]): String = {
    import Text._

    def flagspace = space(mode.flagIndent)

    def render(p: Pirate[A]): String = p.fold(
      command => flags =>
          "Usage:\n" +
                flagspace + command + " " + wrappedsynopsisp(p, command) + "\n" +
          "Options: \n" +
                flagspace + flags.allFlags.map(usagef(_)).mkString("\n" + flagspace),
      commands => commands.map(render(_)).mkString("\n\n")
    )

    def usagef(f: Flag[A]) =
        f.flaguse + "\n" + space(mode.flagIndent + mode.descIndent) + wrap(f.description,  mode.width - mode.flagIndent - mode.descIndent,  mode.flagIndent + mode.descIndent)

    def synopsisf(f: Flag[A]) = f.flagsynopsis

    def wrappedsynopsisp(p: Pirate[A], command: String) =
      wrap(synopsisp(p), mode.width - mode.flagIndent - command.length, mode.flagIndent + command.length)

    def synopsisp(p: Pirate[A]) =
      if (mode.condenseSynopsis)
        "[OPTIONS] " + p.fold(
          command => flags => flags.allPositional.map(synopsisf(_)).mkString(" "),
          commands => ""
        ).mkString(" ")
      else p.fold(
         command => flags => flags.all.map(synopsisf(_)).mkString(" "),
         commands => ""
      )

    render(pirate)
  }

  def usage[A](pirate: Pirate[A]) = usageForMode(DefaultUsageMode)(pirate)
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