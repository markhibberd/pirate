package pirate.example

import scalaz._, Scalaz._
import pirate._, Pirate._

object MthExample {
  sealed trait Args
  case class Example(
    switch: Boolean,
    string: String,
    int: Int
  ) extends Args
  case object Help extends Args
  case object Version extends Args

  val example: Parse[Args] = (Example |*| (
    switch(short('s'), empty)
  , flag[String](short('c'), description("STRING"))
  , flag[Int](short('n'), description("INT"))
  )).map(x => x)

  val all = switch(short('h'), empty).as[Args](Help) ||| switch(short('v'), empty).as(Version) ||| example

 val command = all ~ "example" ~~
   """|An interactive example for pirate with variants.
      |
      |This will allow you to experiment with different
      |parsing options and see common usage.""".stripMargin

  def run(args: String*): (List[String], ParseError \/ Args) =
    Interpreter.run(example, args.toList, DefaultPrefs())

  def main(unused: Array[String]): Unit = {
    println(run("-h"))

    println(run("-v"))

    println(run("-s", "-c", "hello", "-n", "12"))

    println(run("-c", "hello", "-n", "12"))

    println(run("-n", "21", "-c", "hello"))

    println(run("-n", "21", "-s", "-c", "hello"))

    println(Usage.print(command, Nil,DefaultPrefs()))
  }
}
