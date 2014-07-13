package pirate.internal

import pirate._

import scalaz._, Scalaz._

case class P[+A](run: Prefs => ParseError \/ A) {
  def map[B](f: A => B): P[B] =
    P(p => run(p).map(f))

  def flatMap[B](f: A => P[B]): P[B] =
    P(p => run(p) match {
      case -\/(l) => -\/(l)
      case \/-(a) => f(a).run(p)
    })
}

object P {
  implicit def PMonad: Monad[P] = new Monad[P] {
    def point[A](a: => A) = P(_ => a.right)
    def bind[A, B](a: P[A])(f: A => P[B]) = a flatMap f
  }
}
