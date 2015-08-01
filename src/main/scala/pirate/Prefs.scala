package pirate

import com.osinka.i18n.Lang
/**
 * Prefs provides configuration options for generating
 * a usage string as well as parser behaviour.
 */
case class Prefs(
  backtrack: Boolean,
  separateTopLevels: Boolean,
  condenseSynopsis: Boolean,
  usageOnError: Boolean,
  language: Lang,
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
    language = Lang("en"),
    flagIndent = 2,
    descIndent = 26,
    width = 80
  )
}
