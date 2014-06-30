package io.mth.pirate.example

import scalaz._, Scalaz._
import io.mth.pirate._, Pirate._

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
    switch.short('s')
  , option.short[String]('c', "STRING")
  , option.short[Int]('n', "INT")
  )).map(x => x)

  val all = switch.short('h').as[Args](Help) ||| switch.short('v').as(Version) ||| example



 val command = all ~ "example" ~~
  s"""|An interactive example for pirate with variants.
      |
      |This will allow you to experiment with different
      |parsing options and see common usage.""".stripMargin

  def main(unused: Array[String]): Unit = {
    println(Interpretter.run(all, List("-h")))

    println(Interpretter.run(all, List("-v")))

    println(Interpretter.run(all, List("-s", "-c", "hello", "-n", "12")))

    println(Interpretter.run(all, List("-c", "hello", "-n", "12")))

    println(Interpretter.run(all, List("-n", "21", "-c", "hello")))

    println(Interpretter.run(all, List("-n", "21", "-s", "-c", "hello")))

    println(Usage.print(command))
  }
}
