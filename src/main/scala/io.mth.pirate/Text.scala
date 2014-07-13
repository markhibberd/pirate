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

    def wrapit(s: String, w: Int): String =
      if (s.length <= w)
        s
      else if (w <= 0)
        s.substring(0, width - 1) + "-\n" + spacer + wrapit(s.substring(width - 1), width)
      else if (s.charAt(w) == ' ')
        s.substring(0, w) + "\n" + spacer + wrapit(s.substring(w + 1), width)
      else
        wrapit(s, w - 1)

    wrapit(spacer + text, width)
  }
}