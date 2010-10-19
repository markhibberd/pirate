package io.mth.pirate.demo

import io.mth.pirate.Pirate

object SwitchesDemo {
  import Pirate._
  case class DemoArgs(help: Boolean, version: Boolean, verbose: Boolean, expression: Option[String], console: Boolean, config: Option[String])

  val helparg = flag("-h", "--help", "display usage.", (d: DemoArgs) => d.copy(help = true))
  val versionarg = flag("-V", "--version", "display version.", (d: DemoArgs) => d.copy(version = true))
  val verbosearg = flag("-v", "--verbose", "verbose output.", (d: DemoArgs) => d.copy(verbose = true))
  val demo = line("demo", List(helparg, versionarg, verbosearg), List())

  def main(args: Array[String]) {
    println(demo.usage)

    val p = demo.parserise

    val result = demo.parse(List("--version", "-h"), DemoArgs(false, false, false, None, false, None))
    println(result)
  }
}