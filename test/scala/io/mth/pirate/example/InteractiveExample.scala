package io.mth.pirate.example

import io.mth.pirate._, Pirate._

object InteractiveExample {
  case class Example(
    switch: Boolean,
    string: String,
    int: Int
  )

  val command = Example |*| (
    switch.short('s')
  , option.short[String]('c', "STRING")
  , option.short[Int]('n', "INT")
  )

  def main(args: Array[String]): Unit = {
    println(Interpretter.run(command, List("-s", "-c", "hello", "-n", "12")))

    println(Interpretter.run(command, List("-c", "hello", "-n", "12")))

    println(Interpretter.run(command, List("-n", "21", "-c", "hello")))

    println(Interpretter.run(command, List("-n", "21", "-s", "-c", "hello")))
  }
}
