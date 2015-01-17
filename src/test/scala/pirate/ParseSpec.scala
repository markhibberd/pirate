package pirate

import Pirate._
import scalaz._, Scalaz._

class ParseSpec extends spec.Spec { def is = s2"""

  Parse Properties
  ================

  Verify basic name pass-through to command       $name
  Verify description not set on command           $desc
  Verify description can be set on command        $descSet
  Verify basic parser help text looks correct     $helpText
  Verify multi args render correctly              $multiArg
  Verify hidden works                             $hide

"""
  def name = prop((n: String) =>
    (ValueParse(None) ~ n).name == n)

  def desc = prop((n: String) =>
    (ValueParse(None) ~ n).description == None)

  def descSet = prop((n: (String, String)) =>
    (ValueParse(None) ~ n._1 ~~ n._2).description == Some(n._2))

  def helpText = Usage.print((flag[String](short('h') |+| long("help") |+| metavar("topic") |+| description("Show help file")) |@| (switch(short('b') |+| description("Calc file size")))) (_ -> _) ~ "Simple" ~~ "A simple parser") ====
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

  def multiArg = Usage.print(((arguments.many[String](metavar("src")) |@| arguments.one[String](metavar("dst")))(_ -> _)) ~ "ArgTaker" ~~ "A parser with args") ==== 
    """|Usage:
       |        ArgTaker src... dst
       |
       |A parser with args
       |
       |Options:
       |        
       |""".stripMargin
    
  def hide = Usage.print((switch(short('h') |+| hidden )) ~ "Hidden") ====
    """|Usage:
       |        Hidden 
       |
       |
       |Options:
       |        
       |""".stripMargin
}
