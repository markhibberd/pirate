package io.mth.pirate.demo

import io.mth.pirate.{Flag, Pirate}

object PositionalArgsDemo {
  case class DemoArgs(help: Boolean, version: Boolean, verbose: Boolean, expression: Option[String], console: Boolean, command: Option[String], config: List[String])

  import Flag._
  import Pirate._

  val flags =
    full('h', "--help", "display usage.")((d: DemoArgs) => d.copy(help = true)) >>=
    full('V', "--version", "display version.")((d: DemoArgs) => d.copy(version = true)) >>=
    full('v', "--verbose", "verbose output.")((d: DemoArgs) => d.copy(verbose = true)) >>=
    positional1("COMMAND")((s: String) => (d: DemoArgs) => d.copy(command = Some(s))) >>=
    positionalN("CONFIG")((ss: List[String]) => (d: DemoArgs) => d.copy(config = ss))

  val cmd =
    command("demo", flags)

  def main(args: Array[String]) {
    println(cmd.usage)

    val result = cmd.parse(List("--version", "-h", "go", "config1.file", "config2.file"), DemoArgs(false, false, false, None, false, None, List()))

    println("Parsed: \n        " + result)
  }
}
