package pirate

import java.io.File
import java.net.{URI, URL}

import scalaz._, Scalaz._

import org.scalacheck._

class ReadSpec extends spec.Spec { def is = s2"""

  Read Properties
  ===============

  Symmetric
    Char                              ${symmetric[Char]}
    String                            ${symmetric[String]}
    Short                             ${symmetric[Short]}
    Int                               ${symmetric[Int]}
    Long                              ${symmetric[Long]}
    Double                            ${symmetric[Double]}
    Boolean                           ${symmetric[Boolean]}
    BigInt                            ${symmetric[BigInt]}

  File example                        $file
  URI example                         $uri
  URL example                         $url
  Configuration example               $configuration

  Char doesn't parse strings          $charerr
  Numeric doesn't parse strings       $numericerr
  Configuration needs an equals       $configurationerr

  Returning none will fail            $optionFail
  Returning some will pass            $optionPass
  Returning left will fail            $eitherFail
  Returning right will pass           $eitherPass

  Witness (compilation is sufficient) $ok

"""

  def file =
    Read.parse[File](List("some/file")).toOption ==== Some(new File("some/file"))

  def uri =
    Read.parse[URI](List("http://some/file")).toOption ==== Some(new URI("http://some/file"))

  def url =
    Read.parse[URL](List("http://some/file")).toOption ==== Some(new URL("http://some/file"))

  def configuration =prop((s: (String, Int)) => !s._1.contains('=') ==> {
    Read.parse[(String, Int)](List(s"${s._1}=${s._2}")).toOption ==== Some(s) })

  def charerr = prop((c: Char, d: Char, s: String ) =>
    Read.parse[Char](List(c.toString +  d.toString + s)).toOption ==== None)

  def numericerr = prop((s: String) => !s.parseInt.isSuccess ==> {
    Read.parse[Int](List(s)).toOption ==== None})

  def configurationerr = prop((s: String) => !s.contains('=') ==> {
    Read.parse[(String, String)](List(s)).toOption ==== None })

  def optionFail = prop((s: String, e: String) =>
    Read.optionRead(_ => None, e).read(List(s)) ==== ReadErrorInvalidType(s, e).left)

  def optionPass = prop((s: String, r: List[String], e: String) =>
    Read.optionRead(Some(_), e).read(s :: r) ==== (r, s).right)

  def eitherFail = prop((s: String, e: String) =>
    Read.eitherRead(_ => e.left).read(List(s)) ==== ReadErrorInvalidType(s, e).left)

  def eitherPass = prop((s: String, r: List[String]) =>
    Read.eitherRead(_.right).read(s :: r) ==== (r, s).right)

  def symmetric[A: Read: Arbitrary] = prop((a: A) =>
    Read.parse[A](List(a.toString)).toOption ==== Some(a))

  Read.of[Char]
  Read.of[String]
  Read.of[Short]
  Read.of[Int]
  Read.of[Long]
  Read.of[Double]
  Read.of[Boolean]
  Read.of[BigInt]
  Read.of[java.io.File]
  Read.of[java.net.URI]
  Read.of[java.net.URL]
  Read.of[Option[String]]

  Read.of[(Int, Int)]
  Read.of[(String, Int)]
  Read.of[(Int, String)]
  Read.of[(Int, Option[Int])]

  import Read.auto._

  Read.of[(Int, Int, Int, Int, Int)]
}
