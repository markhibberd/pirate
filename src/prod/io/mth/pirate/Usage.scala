package io.mth.pirate

object Usage {
  def usageForMode[A](mode: UsageMode)(pirate: Pirate[A]): String = {

    def space(width: Int) = (for (i <- 1 to width) yield " ").mkString

     def wrap(s: String, width: Int, indent: Int): String = {
       val spacer = space(indent)
       def wrapit(s:String, w: Int): String =
        if (w <= 0 || s.length < w)
          s
        else if  (s.charAt(w) == ' ')
          s.substring(0, w) + "\n" + spacer + wrapit(s.substring(w + 1, s.length), width)
        else
          wrapit(s, w - 1)

       wrapit(s, width)
     }

    def render(p: Pirate[A]): String = p.fold(
      command => flags => "Usage:\n" + space(mode.flagIndent) + command + " " + wrap(synopsisp(p), mode.width - mode.flagIndent - command.length, mode.flagIndent + command.length) + "\nOptions: \n" +
               space(mode.flagIndent) + flags.allFlags.map(usagef(_)).mkString("\n" + space(mode.flagIndent)),

      commands => commands.map(render(_)).mkString("\n\n")
    )

    def usagef(f: Flag[A]) =
        space(mode.flagIndent) + f.flaguse + "\n" + space(mode.flagIndent + mode.descIndent) + wrap(f.description,  mode.width - mode.flagIndent - mode.descIndent,  mode.flagIndent + mode.descIndent)


    def synopsisf(f: Flag[A]) = f.flagsynopsis

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