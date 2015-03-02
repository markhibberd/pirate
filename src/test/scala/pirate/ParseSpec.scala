package pirate

class ParseSpec extends spec.Spec { def is = s2"""

  Parse Properties
  ================

  Verify basic name pass-through to command       $name
  Verify description not set on command           $desc
  Verify description can be set on command        $descSet

"""
  def name = prop((n: String) =>
    (ValueParse(None) ~ n).name == n)

  def desc = prop((n: String) =>
    (ValueParse(None) ~ n).description == None)

  def descSet = prop((n: (String, String)) =>
    (ValueParse(None) ~ n._1 ~~ n._2).description == Some(n._2))

}
