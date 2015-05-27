package pirate.example

import pirate._, Pirate._
import scalaz._, Scalaz._, effect.IO
import java.io.File

sealed trait Cut
case class ByteCut(list: String, split: Boolean, files: List[File]) extends Cut
case class CharCut(list: String, files: List[File]) extends Cut
case class FieldCut(list: String, suppress: Boolean, delimiter: Char, files: List[File]) extends Cut

// FIX support multi arguments, i.e. files
// FIX flag type inference, what even is that?
// FIX sub-type handling
// FIX support explicit read, i.e. 'list'
// FIX descriptions on fields
object CutMain extends PirateMainIO[Cut] {
  val byte: Parse[Cut] = (ByteCut |*| (
    flag[String](short('b'), metavar("list") |+| description("The list specifies byte positions."))
  , switch(short('n'), description("Do not split multi-byte characters.")).not
  , arguments[File](metavar("file"))
  )).map(x => x)

  val char: Parse[Cut] = (CharCut |*| (
    flag[String](short('c'), metavar("list") |+| description("The list specifies character positions."))
  , arguments[File](metavar("file"))
  )).map(x => x)

  val field: Parse[Cut] = (FieldCut |*| (
    flag[String](short('f'), metavar("list") |+| description("The list specifies fields, separated in the input by the field delimiter character (see the -d option.)  Output fields are separated by a single occurrence of the field delimiter character."))
  , switch(short('s'), description("Suppress lines with no field delimiter characters."))
  , flag[Char](both('d', "delimiter"), metavar("delim") |+| description("Use delim as the field delimiter character instead of the tab character.")).default('\t')
  , arguments[File](metavar("file"))
  )).map(x => x)

  override def prefs = NullPrefs.copy(separateTopLevels = true)

  def command: Command[Cut] =
    (byte ||| char ||| field) ~ "cut" ~~
     "This is a demo of the unix cut utility"

  def run(c: Cut) = c match {
    case ByteCut(list, split, files) =>
      IO.putStrLn(s"""cut -b $list ${if (split) "" else "-n "}$files""")
    case CharCut(list, files) =>
      IO.putStrLn(s"""cut -c $list $files""")
    case FieldCut(list, suppress, delimiter, files) =>
      IO.putStrLn(s"""cut -f $list ${if (suppress) "" else "-s "}${if (delimiter == '\t') "" else "-d '" + delimiter + "'"}$files""")
  }
}

class CutExample extends spec.Spec { def is = s2"""

  Cut Examples
  ============

  cat -b 1 one                             $byte
  cut -b 2 -n two                          $noSplit
  cut -c 3 three                           $char
  cut -f 4 four                            $field
  cut -f 5 -s five                         $supress
  cut -f 6 -d x six                        $delim
  cut -f 7 -d x -s seven                   $delimSupress
  cut -f 7 -s -d x seven                   $supressDelim
  cut -b 1 many files                      $manyFiles
  cut with invalid args fails              $invalid

  Cut Checks
  ==========

  Name is set                              ${CutMain.command.name === "cut"}
  Description is set                       ${CutMain.command.description === Some("This is a demo of the unix cut utility")}

"""

  def run(args: String*): ParseError \/ Cut =
    Interpreter.run(CutMain.command.parse, args.toList, NullPrefs)._2

  def byte =
    run("-b", "1", "one") must_==
      ByteCut("1", true, new File("one").pure[List]).right

  def noSplit =
    run("-b", "2", "-n", "two") must_==
      ByteCut("2", false, new File("two").pure[List]).right

  def char =
    run("-c", "3", "three") must_==
      CharCut("3", new File("three").pure[List]).right

  def field =
    run("-f", "4", "four") must_==
      FieldCut("4", false, '\t', new File("four").pure[List]).right

  def supress =
    run("-f", "5", "-s", "five") must_==
      FieldCut("5", true, '\t', new File("five").pure[List]).right

  def delim =
    run("-f", "6", "-d", "x", "six") must_==
      FieldCut("6", false, 'x', new File("six").pure[List]).right

  def delimSupress =
    run("-f", "7", "-d", "x", "-s", "seven") must_==
      FieldCut("7", true, 'x', new File("seven").pure[List]).right

  def supressDelim =
    run("-f", "8", "-s", "-d", "x", "eight") must_==
      FieldCut("8", true, 'x', new File("eight").pure[List]).right

  def manyFiles = 
    run("-b", "1", "many", "files") must_==
      ByteCut("1", true, List(new File("many"), new File("files"))).right

  def invalid =
    Interpreter.run(CutMain.command.parse, nil, NullPrefs)._2.toEither must beLeft
}
