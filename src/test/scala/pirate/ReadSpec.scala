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

  Char doesn't parse strings          $charerr
  Numeric doesn't parse strings       $numericerr
  Witness (compilation is sufficient) $ok

"""

  def file =
    Read.parse[File](List("some/file")).toOption ==== Some(new File("some/file"))

  def uri =
    Read.parse[URI](List("http://some/file")).toOption ==== Some(new URI("http://some/file"))

  def url =
    Read.parse[URL](List("http://some/file")).toOption ==== Some(new URL("http://some/file"))

  def charerr = prop((c: Char, d: Char, s: String ) =>
    Read.parse[Char](List(c.toString +  d.toString + s)).toOption ==== None)

  def numericerr = prop((s: String) => !s.parseInt.isSuccess ==> {
    Read.parse[Int](List(s)).toOption ==== None})

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

  import Read.auto._

  Read.of[(Int, Int)]
  Read.of[(String, Int)]
  Read.of[(Int, String)]
  Read.of[(Int, Int, Int, Int, Int)]
  Read.of[(Int, Option[Int])]
}
