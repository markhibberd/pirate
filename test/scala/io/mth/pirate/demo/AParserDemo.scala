package io.mth.pirate.demo

import scalaz._, Scalaz._
import io.mth.piratex._

object AParserDemo {
  def main(ignored: Array[String]): Unit = {
    val args = List("-g", "-f", "asfasf")
    val args2 = List("-g", "-f", "asfasf", "asfasf")

    val complicated = List("fred", "-g", "-f", "asfasf", "asfasfzzzz")
    val complicated2 = List("barney", "-f", "asfasf")

    import NewApi._

    val g = switch.short('g')
    val f = option.short('f', "X")
    val p = positional.one("P")
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

//    println(Interpret.run(g, List("-g"), AcceptOptions))
//    println(Interpret.run(g, List(), AcceptOptions))
//    println(Interpret.run(f, List(), AcceptOptions))
//    println(Interpret.run(f, List("-f"), AcceptOptions))
//    println(Interpret.run(f, List("-f", "ok"), AcceptOptions))
//    println(Interpret.run(gf, args, AcceptOptions))
    println(Interpret.run(zz, args2, AcceptOptions))

//    println(Interpret.run(fred, complicated, AcceptOptions))
//    println(Interpret.run(barney, complicated2, AcceptOptions))

//    println(Interpret.run(fb, complicated, AcceptOptions))
//    println(Interpret.run(fb, complicated2, AcceptOptions))



  }
}
