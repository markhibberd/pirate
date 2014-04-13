package io.mth.pirate

import org.scalacheck.Prop._
import org.scalacheck.Properties

object TextTest extends Properties("Text") {
  import Text._

  property("all spaces") =
          forAll((i: Int) =>
               (i >= 0 && i <= 1000) ==>
                  space(i).forall(_ == ' ')
                  )

  property("correct length") =
          forAll((i: Int) =>
               (i >= 0 && i <= 1000) ==>                  
                  (space(i).length == i)
                  )

  property("wrap no longer than width") =
       forAll((s: String) =>
            s.length > 70 ==>
              wrap(s, 80, 0).split('\n').forall(_.length <= 80))

  property("wrap no longer than width + indent") =
       forAll((s: String) =>
            s.length > 70 ==>
              wrap(s, 80, 10).split('\n').forall(_.length <= 90))

  property("never lose content") =
       forAll((s: String) =>
            s.length > 70 ==> drains(s, wrap(s, 80, 0)))

  def drains(orig: String, modded: String) = {
    val sb = new StringBuilder(orig)
    modded.foreach(c =>
      if (sb.length() > 0 && sb.charAt(0) == c)
        sb.deleteCharAt(0)
    )
    sb.length() == 0

  }
}