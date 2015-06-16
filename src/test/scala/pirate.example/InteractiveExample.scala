package pirate.example

import scalaz._
import pirate._, Pirate._

// FIX actually implement this, currently just some hard coded examples that use the internals.
object InteractiveExample {
  case class Example(
    switch: Boolean,
    string: String,
    int: Int
  )

  val example = Example |*| (
    switch(short('s'), empty)
  , flag[String](short('c'), description("STRING"))
  , flag[Int](short('n'), description("INT"))
  )

 val command = example ~ "example" ~~
   """|An interactive example for pirate.
      |
      |This will allow you to experiment with different
      |parsing options and see common usage.""".stripMargin

  def run(args: String*): (List[String], ParseError \/ Example) =
    Interpreter.run(example, args.toList, DefaultPrefs())

  def main(unused: Array[String]): Unit = {
    println(run("-s", "-c", "hello", "-n", "12"))

    println(run("-c", "hello", "-n", "12"))

    println(run("-n", "21", "-c", "hello"))

    println(run("-n", "21", "-s", "-c", "hello"))

    println(Usage.print(command, Nil, DefaultPrefs()))
  }
}
