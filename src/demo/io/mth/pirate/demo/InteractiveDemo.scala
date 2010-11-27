package io.mth.pirate.demo

import scalaz.{Failure, Success}

object InteractiveDemo {
  import io.mth.pirate._
  
  case class DemoArgs(help: Boolean, version: Boolean, verbose: Boolean, things: List[String])

  val cmd =
    command[DemoArgs]("demo") <|>
      flag('h', "help", "display usage.")(_.copy(help = true)) <|>
      flag('V', "version", "display version.")(_.copy(version = true)) <|>
      flag('v', "verbose", "verbose output.")(_.copy(verbose = true)) >|
      positional0plus("THINGS")((d, ss) => d.copy(things = ss))

  val program =
    cmd ~ """
      | This is the interactive demo. Send it an option and
      | get some output.
    """.stripMargin

  def run(args: DemoArgs): Unit = {
    val output =
      if (args.help)
        program.usage
      else if (args.version)
        "demo version 1"
      else if (args.verbose)
        "this is the verbose output of the demo program [\n" + args + "\n]"
      else
        args.toString
    println(output)
  }

  def main(ignored: Array[String]) {
    val args = List("--verbose", "thing.one", "thing.two", "cat", "hat")

    val default = DemoArgs(false, false, false, List())

    val exitcode = cmd.dispatchOrUsage(args, default)(run _)

    exit(exitcode)
  }
}