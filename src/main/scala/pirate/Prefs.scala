package pirate

/**
 * Prefs provides configuration options for generating
 * a usage string as well as parser behaviour.
 */
case class Prefs(
  backtrack: Boolean,
  separateTopLevels: Boolean,
  condenseSynopsis: Boolean,
  usageOnError: Boolean,
  flagIndent: Int,
  descIndent: Int,
  width: Int
)

/**
 * Default preferences
 *  - Explicit synopsis.
 *  - 8/16 indents
 *  - 80 width
 */
object DefaultPrefs {
  def apply() = Prefs(
    backtrack = true,
    separateTopLevels = true,
    condenseSynopsis = false,
    usageOnError = false,
    flagIndent = 2,
    descIndent = 26,
    width = 80
  )
}
