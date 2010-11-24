package io.mth.pirate.demo

import io.mth.pirate._

object SwitchesDemo {
  case class DemoArgs(help: Boolean, version: Boolean, verbose: Boolean, expression: Option[String], console: Boolean, config: Option[String])

  import Flag._
  import Command._

  val cmd =
    command[DemoArgs]("demo") <|>
      full1('c', "--config", "specify config file.", "FILE")((d, s) => d.copy(config = Some(s))) <|>
      full('h', "--help", "display usage.")(_.copy(help = true)) <|>
      full('V', "--version", "display version.")(_.copy(version = true)) <|>
      full('v', "--verbose", "verbose output.")(_.copy(verbose = true))

  def main(args: Array[String]) {
    println(cmd.usage)

    val result = cmd.parse(List("--version", "-h"), DemoArgs(false, false, false, None, false, None))

    println("Parsed: \n        " + result)
  }
}