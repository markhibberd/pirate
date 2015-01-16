package pirate

import Pirate._
import scalaz._, Scalaz._

class ParseSpec extends spec.Spec { def is = s2"""

  Parse Properties
  ================

  Verify basic name pass-through to command       $name
  Verify description not set on command           $desc
  Verify basic parser help text looks correct     $helpText
  Verify multi args render correctly              $multiArg

"""
  def name = prop((n: String) =>
    (ValueParse(None) ~ n).name == n)

  def desc = prop((n: String) =>
    (ValueParse(None) ~ n).description == None)

  def helpText = Usage.print((flag[String](('h', "help"), "topic") <> "Show help file" |@| (switch('b') <> "Calc file size")) (_ -> _) ~ "Simple" ~~ "A simple parser") ====
    """|Usage:
       |        Simple -b -h|--help topic
       |
       |A simple parser
       |
       |Options:
       |        -b
       |                Calc file size
       |        -h|--help topic
       |                Show help file
       |""".stripMargin

  def multiArg = Usage.print(((arguments.many[String]("src") |@| arguments.one[String]("dst"))(_ -> _)) ~ "ArgTaker" ~~ "A parser with args") ==== 
    """|Usage:
       |        ArgTaker src... dst
       |
       |A parser with args
       |
       |Options:
       |        
       |""".stripMargin
    
}
