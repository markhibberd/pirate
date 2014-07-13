package pirate
package internal

import scalaz._, Scalaz._
import pirate.test.Laws._
import pirate.test.Arbitraries._

class NondetTSpec extends test.Spec { def is = s2"""

  NondetT Laws
  ============

  NondetT is an equal (for tests) ${equal.laws[NondetT[Identity, Int]]}
  NondetT is a monad              ${monad.laws[NondetX]}
  NondetT is a monad plus         ${monadPlus.laws[NondetX]}
  NondetT is a strong monad plus  ${monadPlus.strongLaws[NondetX]}

"""

  type NondetX[A] = NondetT[Identity, A]

  /* testing only instances */
  implicit def NondetTEqual[F[+_], A](implicit E: Equal[F[(Boolean, List[A])]], F: Monad[F]): Equal[NondetT[F, A]] =
    Equal.equal[NondetT[F, A]]((a, b) => a.runNondetT.run.run(true) === b.runNondetT.run.run(true) && a.runNondetT.run.run(false) === b.runNondetT.run.run(false))

}
