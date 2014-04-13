package io.mth.pirate.demo

import scalaz._, Scalaz._
import io.mth.pirate._

object ParseDemo {
  def main(ignored: Array[String]): Unit = {
    val args = List("-g", "-f", "asfasf")
    val args2 = List("-g", "-f", "asfasf", "zzzzsss")
    val args3 = List("zzzzz", "-g", "-f", "asfasf")

    val complicated = List("fred", "-g", "-f", "asfasf", "asfasfzzzz")
    val complicated2 = List("barney", "-f", "asfasf")

    import Flags._

    val g = switch.short('g')
    val f = option.short[String]('f', "X")
    val p = positional.one[String]("P")
    val gf = for { gg <- g; ff <- f } yield (gg, ff)
    val gfx = for { ff <- f; gg <- g } yield (gg, ff)
    val gfxx = (f |@| g)(_ -> _)
    val gfxxx = (g |@| f)(_ -> _)
    val zz = (g |@| f |@| p)((gg, ff, pp) => (gg, ff, pp))

    val f2 = f.map(x =>  s"-f is set to $x")
    val zz2 = zz.map({ case (g, f, p) => s"g=$g :: f=$f :: p=$p" })

    val fred = command.of("fred", zz2)
    val barney = command.of("barney", f2)
    val fb = fred <+> barney

    println(Interpretter.run(g, List("-g")))
    println(Interpretter.run(g, List()))
    println(Interpretter.run(f, List()))
    println(Interpretter.run(f, List("-f")))
    println(Interpretter.run(f, List("-f", "ok")))
    println(Interpretter.run(gf, args ))
    println(Interpretter.run(zz, args2))
    println(Interpretter.run(zz, args3))

    println(Interpretter.run(fred, complicated))
    println(Interpretter.run(barney, complicated2))

    println(Interpretter.run(fb, complicated))
    println(Interpretter.run(fb, complicated2))



  }
}
