package io.mth.pirate

object Usage {
  /**
   * Build the usage string for the specified command and mode
   * configuration
   */
  def usage[A](mode: UsageMode)(pirate: Command[A]): String = {
    import Text._

    val flagspace = space(mode.flagIndent)

    def render(p: Command[A]): String = p.fold(
      (command, description, flags, positionals) =>
          "Usage:\n" +
                flagspace + command + " " + synopsis(p) + "\n" + // FIX smart wrap for synopsis...
         description.map(_ + "\n").getOrElse("") +
          "Options: \n" +
                flagspace + flags.toList.map(option(_)).mkString("\n" + flagspace)
    )

    def option(f: Flag[A]) =
      flaguse(f) + "\n" +
        wrap(f.description,  mode.width - mode.descIndent,  mode.descIndent)

    def synopsis(p: Command[A]) =
      if (mode.condenseSynopsis)
        "[OPTIONS] " + p.fold(
          (command, description, flags, positionals) =>
            positionals.toList.map(paramsynopsis(_)).mkString(" ")
        )
      else p.fold(
         (command, description, flags, positionals) =>
           flags.toList.map(flagsynopsis(_)).mkString(" ") + " " + positionals.toList.map(paramsynopsis(_)).mkString(" ")
      )

    def flaguse[A](f: Flag[A]): String =
      f.fold(
        (s, l, d, f) => "-" + s + ",--" + l,
        (s, d, f) => "-" + s,
        (l, d, f) => "--" + l,
        (s, l, d, m, f) =>  "-" + s + ",--" + l + "=" + m,
        (s, d, m, f) =>  "-" + s + "=" + m,
        (l, d, m, f) =>  "--" + l + "=" + m
      )

    def flagsynopsis[A](f: Flag[A]): String =
      f.fold(
        (s, l, d, f) => "[-" + s + "|--" + l + "]",
        (s, d, f) => "[-" + s + "]",
        (l, d, f) => "[--" + l + "]",
        (s, l, d, m, f) =>  "[-" + s + "|--" + l + " " + m + "]",
        (s, d, m, f) =>  "[-" + s + " " + m + "]",
        (l, d, m, f) =>  "[--" + l + " " + m + "]"
      )

    def paramsynopsis[A](p: Positional[A]): String = p.fold(
        (m, f) => m,
        (n, m, f) => (for (_ <- 1 to n) yield m).mkString(" "),
        (m, f) => "[" + m + " ...]",
        (m, f) =>
          if (mode.tightOneOrManySynopsis) m + " ..."
          else m + "[" + m + " ...]"
      )

    render(pirate)
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
