package scala.io.mth.pirate.demo

import io.mth.pirate.Pirate

object PositionalArgsDemo {
  import Pirate._

  case class DemoArgs(help: Boolean, config: String)

  val helparg = flag("-h", "--help", "display usage.", (d: DemoArgs) => d.copy(help = true))
  val configarg = positional("config.scala", (d: DemoArgs) => (s: String) =>  d.copy(config = s))
  val demo = line("demo", List(helparg), List(configarg))

  def main(args: Array[String]) {
    println(demo.usage)

    val p = demo.parserise

    val result1 = parse(demo, List("-h", "config.scala"), DemoArgs(false, ""))
    println(result1)

    val result2 = parse(demo, List("config.scala"), DemoArgs(false, ""))
    println(result2)

    val result3 = parse(demo, List("-h"), DemoArgs(false, ""))
    println(result3)
  }
}