package io.mth.pirate.demo

import io.mth.pirate.{Command, Usage, Pirate, Flag}

object SwitchesDemo {
  case class DemoArgs(help: Boolean, version: Boolean, verbose: Boolean, expression: Option[String], console: Boolean, config: Option[String])

  import Flag._
  import Command._

  val cmd =
    command("demo") <|>
      full1('c', "--config", "specify config file.", "FILE")((s: String) => (d: DemoArgs) => d.copy(config = Some(s))) <|>
      full('h', "--help", "display usage.")((d: DemoArgs) => d.copy(help = true)) <|>
      full('V', "--version", "display version.")((d: DemoArgs) => d.copy(version = true)) <|>
      full('v', "--verbose", "verbose output.")((d: DemoArgs) => d.copy(verbose = true))

  def main(args: Array[String]) {
    println(cmd.usage)

    val result = cmd.parse(List("--version", "-h"), DemoArgs(false, false, false, None, false, None))

    println("Parsed: \n        " + result)
  }
}