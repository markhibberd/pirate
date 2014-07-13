package io.mth.pirate

object Text {
  /**
   * Create a string of spaces exactly n long.
   */
  def space(width: Int) = " " * width

  /**
   * Wrap text at width. Prepend an indent on each line of indent.
   */
  def wrap(text: String, width: Int, indent: Int): String = {
    val spacer = space(indent)

    // Add 1 for hyphen + newline
    val sb = new StringBuilder(text.length + (text.length / width * (spacer.length + 2)))
    val s = spacer + text

    @annotation.tailrec
    def wrapit(o: Int, w: Int): Unit = {
      val i = o + w
      if (s.length <= i)
        sb ++= s.substring(o)
      else if (w <= 0) {
        sb ++= s.substring(o, o + width - 1) ++= "-\n" ++= spacer
        wrapit(o + width - 1, width)
      } else if (s.charAt(i) == ' ') {
        sb ++= s.substring(o, i) ++= "\n" ++= spacer
        wrapit(i + 1, width)
      }
      else
        wrapit(o, w - 1)
    }

    // Use a bullshit offset because String.substring() copies a new array on Java 1.7+
    wrapit(0, width)
    sb.toString()
  }
}