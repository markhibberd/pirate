package io.mth.pirate.demo

import io.mth.pirate.{Usage, Pirate, Flag}

object SwitchesDemo {
  case class DemoArgs(help: Boolean, version: Boolean, verbose: Boolean, expression: Option[String], console: Boolean, config: Option[String])

  import Flag._
  import Pirate._

  val flags =
    full('h', "--help", "display usage.")((d: DemoArgs) => d.copy(help = true)) >>=
    full('V', "--version", "display version.")((d: DemoArgs) => d.copy(version = true)) >>=
    full('v', "--verbose", "verbose output.")((d: DemoArgs) => d.copy(verbose = true))

  val cmd =
    command("demo", flags)

  def main(args: Array[String]) {
    println(Usage.usage(cmd))

    val result = cmd.parse(List("--version", "-h"), DemoArgs(false, false, false, None, false, None))
    
    println(result)
  }
}