package io.mth.pirate.example

import io.mth.pirate._, Pirate._
import io.mth.pirate.internal._

// FIX actually implement this, currently just some hard coded examples that use the internals.
object InteractiveExample {
  case class Example(
    switch: Boolean,
    string: String,
    int: Int
  )

  val example = Example |*| (
    switch('s')
  , flag[String]('c', "STRING")
  , flag[Int]('n', "INT")
  )

 val command = example ~ "example" ~~
  s"""|An interactive example for pirate.
      |
      |This will allow you to experiment with different
      |parsing options and see common usage.""".stripMargin

  def main(unused: Array[String]): Unit = {
    println(Interpretter.run(example, List("-s", "-c", "hello", "-n", "12")))

    println(Interpretter.run(example, List("-c", "hello", "-n", "12")))

    println(Interpretter.run(example, List("-n", "21", "-c", "hello")))

    println(Interpretter.run(example, List("-n", "21", "-s", "-c", "hello")))

    println(Usage.print(command))
  }
}
