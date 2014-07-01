package io.mth.pirate
package internal

import scalaz._, Scalaz._
import io.mth.pirate.test.Laws._
import io.mth.pirate.test.Arbitraries._

class ListTSpec extends test.Spec { def is = s2"""

  ListT Laws
  ==========

  TStep is an equal               ${equal.laws[TStep[Int, Int]]}
  TStep is a bifunctor            ${bifunctor.laws[TStep]}

  ListT is an equal (for tests)   ${equal.laws[ListT[Identity, Int]]}
  ListT is a monad                ${monad.laws[ListX]}
  ListT is a monad plus           ${monadPlus.laws[ListX]}
  ListT is a strong monad plus    ${monadPlus.strongLaws[ListX]}

"""

  type ListX[A] = ListT[Identity, A]

  /* testing only instances */
  implicit def ListTEqual[F[+_], A](implicit E: Equal[F[List[A]]], F: Monad[F]): Equal[ListT[F, A]] =
    Equal.equal[ListT[F, A]]((a, b) => a.run === b.run)
}
