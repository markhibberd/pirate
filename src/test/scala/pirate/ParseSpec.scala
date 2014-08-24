package pirate

class ParseSpec extends spec.Spec { def is = s2"""

  Parse Properties
  ================

  Verify basic name pass-through to command       $name
  Verify description not set on command           $desc

"""
  def name = prop((n: String) =>
    (ValueParse(None) ~ n).name == n)

  def desc = prop((n: String) =>
    (ValueParse(None) ~ n).description == None)
}
