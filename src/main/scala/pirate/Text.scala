package pirate

object Text {
  /**
   * Create a string of spaces exactly n long.
   */
  def space(width: Int) = " " * width

  /**
   * Wrap text at width. Prepend an indent on each line of indent.
   */
  def wrap(firstText: String, flagIndent: Int)(text: String, widthFull: Int, indent: Int): String = {
    val spacer = space(indent)
    // We need to pick a minimum size for the text
    val width = math.max(widthFull, 50)
    val firstSpacer = space(Math.max(1,indent - firstText.length - flagIndent))

    // Add 1 for hyphen + newline
    val sb = new StringBuilder(text.length + (text.length / width * (spacer.length + 2)))
    val s = firstText + firstSpacer + text.trim

    @annotation.tailrec
    def wrapit(o: Int, w: Int): Unit = {
      val i = o + w
      val nl = s.indexOf('\n', o)
      if (nl > -1 && nl < o + w) {
        sb ++= s.substring(o, nl) ++= "\n" ++= spacer
        wrapit(nl + 1, width)
      } else if (s.length <= i) {
        sb ++= s.substring(o)
        ()
      } else if (w <= 0) {
        sb ++= s.substring(o, o + width - 1) ++= "-\n" ++= spacer
        wrapit(o + width - 1, width)
      } else if (s.charAt(i) == ' ') {
        sb ++= s.substring(o, i) ++= "\n" ++= spacer
        wrapit(i + 1, width)
      } else
        wrapit(o, w - 1)
    }

    // Use a bullshit offset because String.substring() copies a new array on Java 1.7+
    wrapit(0, width + indent)
    sb.toString()
  }
}
