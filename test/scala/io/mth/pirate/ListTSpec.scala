package io.mth.pirate

import scalaz._, Scalaz._
import io.mth.pirate.test.Laws._
import io.mth.pirate.test.Arbitraries._

class ListTSpec extends test.Spec { def is = s2"""

  ListT Laws
  ==========

  TStep is an equal               ${equal.laws[TStep[Int, Int]]}
  TStep is a bifunctor            ${bifunctor.laws[TStep]}

  ListT is a monad                ${monad.laws[ListX]}
  ListT is a monad plus           ${monadPlus.laws[ListX]}
  ListT is a strong monad plus    ${monadPlus.strongLaws[ListX]}
  ListT is an equal               ${equal.laws[ListT[Identity, Int]]}

"""

  type ListX[A] = ListT[Identity, A]
}
