package io.mth.pirate

object GenerateApplicativeStyle extends App {
  val params =
    List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y')

  def tparams(n: Int) =
    (0 until n).map(params.apply).mkString(", ")

  def targs(n: Int) =
    (0 until n).map(params.apply).map(c => s"${c.toLower}: Z[$c]").mkString(", ")

  def tvalues(n: Int) =
    (0 until n).map(params.apply).map(c => c.toLower).mkString(", ")

def template(n: Int) = s"""
  implicit class Function${n}ApplicativeStyle[${tparams(n + 1)}](fab: (${tparams(n)}) => ${params(n)}) {
    def |*|[Z[_]](${targs(n)})(implicit Z: Applicative[Z]): Z[${params(n)}] =
      ${impl(n)}
  }
"""
  def impl(n: Int) =
    if (n <= 12)
      s"""Z.apply${n}(${tvalues(n)})(fab)"""
    else if (n % 2 == 0)
      s"""Z.apply2(Z.apply${n / 2}(${(0 until (n / 2)).map(m => params(m).toLower).mkString(", ")})((${(0 until (n / 2)).map(_ => "_").mkString(", ")})), Z.apply${n / 2}(${(0 until (n / 2)).map(m => params(m + n / 2).toLower).mkString(", ")})((${(0 until (n / 2)).map(_ => "_").mkString(", ")})))((x1, x2) => fab(${(1 to (n / 2)).map(m => "x1._" + m).mkString(", ")}, ${(1 to (n / 2)).map(m => "x2._" + m).mkString(", ")}))"""
    else
      s"""Z.apply3(Z.apply${n / 2}(${(0 until (n / 2)).map(m => params(m).toLower).mkString(", ")})((${(0 until (n / 2)).map(_ => "_").mkString(", ")})), Z.apply${(n / 2)}(${(0 until (n / 2)).map(m => params(m + n / 2).toLower).mkString(", ")})((${(0 until (n / 2)).map(_ => "_").mkString(", ")})), ${params(n - 1).toLower})((x1, x2, x3) => fab(${(1 to (n / 2)).map(m => "x1._" + m).mkString(", ")}, ${(1 to (n / 2)).map(m => "x2._" + m).mkString(", ")}, x3))"""


  val header = s"""
package io.mth.pirate

import scalaz._, Scalaz._

object ApplicativeStyle extends ApplicativeStyle

trait ApplicativeStyle {
"""

  val one = s"""
  implicit class Function1ApplicativeStyle[A, B](fab: A => B) {
    def |*|[Z[_]](a: Z[A])(implicit Z: Applicative[Z]): Z[B] =
      a.map(fab)
  }
  """
  val rest = (2 to 22).map(template).mkString

  val footer = "\n}\n"

  println(header + one + rest + footer)
}
