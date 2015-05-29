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

  def main(unused: Array[String]): Unit = {
    println(Interpreter.run(all, List("-h")))

    println(Interpreter.run(all, List("-v")))

    println(Interpreter.run(all, List("-s", "-c", "hello", "-n", "12")))

    println(Interpreter.run(all, List("-c", "hello", "-n", "12")))

    println(Interpreter.run(all, List("-n", "21", "-c", "hello")))

    println(Interpreter.run(all, List("-n", "21", "-s", "-c", "hello")))

    println(Usage.print(command, Nil))
  }
}
