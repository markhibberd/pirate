package io.mth.pirate

import scalaz._, Scalaz._

object ApplicativeStyle extends ApplicativeStyle

trait ApplicativeStyle {
  implicit class Function2ApplicativeStyle[A, B](f: A => B) {
    def |*|[F[_]](a: F[A])(implicit F: Applicative[F]): F[B] =
      a.map(f)
  }

  implicit class Function3ApplicativeStyle[A, B, C](f: (A, B) => C) {
    def |*|[F[_]](a: F[A], b: F[B])(implicit F: Applicative[F]): F[C] =
      F.apply2(a, b)(f)
  }

  implicit class Function4ApplicativeStyle[A, B, C, D](f: (A, B, C) => D) {
    def |*|[F[_]](a: F[A], b: F[B], c: F[C])(implicit F: Applicative[F]): F[D] =
      F.apply3(a, b, c)(f)
  }

}
