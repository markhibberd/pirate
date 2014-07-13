package pirate

import scalaz._, Scalaz._

object ApplicativeStyle extends ApplicativeStyle

trait ApplicativeStyle {
  implicit class Function1ApplicativeStyle[A, B](fab: A => B) {
    def |*|[Z[_]](a: Z[A])(implicit Z: Applicative[Z]): Z[B] =
      a.map(fab)
  }

  implicit class Function2ApplicativeStyle[A, B, C](fab: (A, B) => C) {
    def |*|[Z[_]](a: Z[A], b: Z[B])(implicit Z: Applicative[Z]): Z[C] =
      Z.apply2(a, b)(fab)
  }

  implicit class Function3ApplicativeStyle[A, B, C, D](fab: (A, B, C) => D) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C])(implicit Z: Applicative[Z]): Z[D] =
      Z.apply3(a, b, c)(fab)
  }

  implicit class Function4ApplicativeStyle[A, B, C, D, E](fab: (A, B, C, D) => E) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D])(implicit Z: Applicative[Z]): Z[E] =
      Z.apply4(a, b, c, d)(fab)
  }

  implicit class Function5ApplicativeStyle[A, B, C, D, E, F](fab: (A, B, C, D, E) => F) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E])(implicit Z: Applicative[Z]): Z[F] =
      Z.apply5(a, b, c, d, e)(fab)
  }

  implicit class Function6ApplicativeStyle[A, B, C, D, E, F, G](fab: (A, B, C, D, E, F) => G) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F])(implicit Z: Applicative[Z]): Z[G] =
      Z.apply6(a, b, c, d, e, f)(fab)
  }

  implicit class Function7ApplicativeStyle[A, B, C, D, E, F, G, H](fab: (A, B, C, D, E, F, G) => H) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G])(implicit Z: Applicative[Z]): Z[H] =
      Z.apply7(a, b, c, d, e, f, g)(fab)
  }

  implicit class Function8ApplicativeStyle[A, B, C, D, E, F, G, H, I](fab: (A, B, C, D, E, F, G, H) => I) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H])(implicit Z: Applicative[Z]): Z[I] =
      Z.apply8(a, b, c, d, e, f, g, h)(fab)
  }

  implicit class Function9ApplicativeStyle[A, B, C, D, E, F, G, H, I, J](fab: (A, B, C, D, E, F, G, H, I) => J) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I])(implicit Z: Applicative[Z]): Z[J] =
      Z.apply9(a, b, c, d, e, f, g, h, i)(fab)
  }

  implicit class Function10ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K](fab: (A, B, C, D, E, F, G, H, I, J) => K) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I], j: Z[J])(implicit Z: Applicative[Z]): Z[K] =
      Z.apply10(a, b, c, d, e, f, g, h, i, j)(fab)
  }

  implicit class Function11ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L](fab: (A, B, C, D, E, F, G, H, I, J, K) => L) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I], j: Z[J], k: Z[K])(implicit Z: Applicative[Z]): Z[L] =
      Z.apply11(a, b, c, d, e, f, g, h, i, j, k)(fab)
  }

  implicit class Function12ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M](fab: (A, B, C, D, E, F, G, H, I, J, K, L) => M) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I], j: Z[J], k: Z[K], l: Z[L])(implicit Z: Applicative[Z]): Z[M] =
      Z.apply12(a, b, c, d, e, f, g, h, i, j, k, l)(fab)
  }

  implicit class Function13ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M) => N) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I], j: Z[J], k: Z[K], l: Z[L], m: Z[M])(implicit Z: Applicative[Z]): Z[N] =
      Z.apply3(Z.apply6(a, b, c, d, e, f)((_, _, _, _, _, _)), Z.apply6(g, h, i, j, k, l)((_, _, _, _, _, _)), m)((x1, x2, x3) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x3))
  }

  implicit class Function14ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I], j: Z[J], k: Z[K], l: Z[L], m: Z[M], n: Z[N])(implicit Z: Applicative[Z]): Z[O] =
      Z.apply2(Z.apply7(a, b, c, d, e, f, g)((_, _, _, _, _, _, _)), Z.apply7(h, i, j, k, l, m, n)((_, _, _, _, _, _, _)))((x1, x2) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7))
  }

  implicit class Function15ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I], j: Z[J], k: Z[K], l: Z[L], m: Z[M], n: Z[N], o: Z[O])(implicit Z: Applicative[Z]): Z[P] =
      Z.apply3(Z.apply7(a, b, c, d, e, f, g)((_, _, _, _, _, _, _)), Z.apply7(h, i, j, k, l, m, n)((_, _, _, _, _, _, _)), o)((x1, x2, x3) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x3))
  }

  implicit class Function16ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I], j: Z[J], k: Z[K], l: Z[L], m: Z[M], n: Z[N], o: Z[O], p: Z[P])(implicit Z: Applicative[Z]): Z[Q] =
      Z.apply2(Z.apply8(a, b, c, d, e, f, g, h)((_, _, _, _, _, _, _, _)), Z.apply8(i, j, k, l, m, n, o, p)((_, _, _, _, _, _, _, _)))((x1, x2) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8))
  }

  implicit class Function17ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I], j: Z[J], k: Z[K], l: Z[L], m: Z[M], n: Z[N], o: Z[O], p: Z[P], q: Z[Q])(implicit Z: Applicative[Z]): Z[R] =
      Z.apply3(Z.apply8(a, b, c, d, e, f, g, h)((_, _, _, _, _, _, _, _)), Z.apply8(i, j, k, l, m, n, o, p)((_, _, _, _, _, _, _, _)), q)((x1, x2, x3) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8, x3))
  }

  implicit class Function18ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I], j: Z[J], k: Z[K], l: Z[L], m: Z[M], n: Z[N], o: Z[O], p: Z[P], q: Z[Q], r: Z[R])(implicit Z: Applicative[Z]): Z[S] =
      Z.apply2(Z.apply9(a, b, c, d, e, f, g, h, i)((_, _, _, _, _, _, _, _, _)), Z.apply9(j, k, l, m, n, o, p, q, r)((_, _, _, _, _, _, _, _, _)))((x1, x2) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x1._9, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8, x2._9))
  }

  implicit class Function19ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I], j: Z[J], k: Z[K], l: Z[L], m: Z[M], n: Z[N], o: Z[O], p: Z[P], q: Z[Q], r: Z[R], s: Z[S])(implicit Z: Applicative[Z]): Z[T] =
      Z.apply3(Z.apply9(a, b, c, d, e, f, g, h, i)((_, _, _, _, _, _, _, _, _)), Z.apply9(j, k, l, m, n, o, p, q, r)((_, _, _, _, _, _, _, _, _)), s)((x1, x2, x3) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x1._9, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8, x2._9, x3))
  }

  implicit class Function20ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I], j: Z[J], k: Z[K], l: Z[L], m: Z[M], n: Z[N], o: Z[O], p: Z[P], q: Z[Q], r: Z[R], s: Z[S], t: Z[T])(implicit Z: Applicative[Z]): Z[U] =
      Z.apply2(Z.apply10(a, b, c, d, e, f, g, h, i, j)((_, _, _, _, _, _, _, _, _, _)), Z.apply10(k, l, m, n, o, p, q, r, s, t)((_, _, _, _, _, _, _, _, _, _)))((x1, x2) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x1._9, x1._10, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8, x2._9, x2._10))
  }

  implicit class Function21ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I], j: Z[J], k: Z[K], l: Z[L], m: Z[M], n: Z[N], o: Z[O], p: Z[P], q: Z[Q], r: Z[R], s: Z[S], t: Z[T], u: Z[U])(implicit Z: Applicative[Z]): Z[V] =
      Z.apply3(Z.apply10(a, b, c, d, e, f, g, h, i, j)((_, _, _, _, _, _, _, _, _, _)), Z.apply10(k, l, m, n, o, p, q, r, s, t)((_, _, _, _, _, _, _, _, _, _)), u)((x1, x2, x3) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x1._9, x1._10, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8, x2._9, x2._10, x3))
  }

  implicit class Function22ApplicativeStyle[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](fab: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W) {
    def |*|[Z[_]](a: Z[A], b: Z[B], c: Z[C], d: Z[D], e: Z[E], f: Z[F], g: Z[G], h: Z[H], i: Z[I], j: Z[J], k: Z[K], l: Z[L], m: Z[M], n: Z[N], o: Z[O], p: Z[P], q: Z[Q], r: Z[R], s: Z[S], t: Z[T], u: Z[U], v: Z[V])(implicit Z: Applicative[Z]): Z[W] =
      Z.apply2(Z.apply11(a, b, c, d, e, f, g, h, i, j, k)((_, _, _, _, _, _, _, _, _, _, _)), Z.apply11(l, m, n, o, p, q, r, s, t, u, v)((_, _, _, _, _, _, _, _, _, _, _)))((x1, x2) => fab(x1._1, x1._2, x1._3, x1._4, x1._5, x1._6, x1._7, x1._8, x1._9, x1._10, x1._11, x2._1, x2._2, x2._3, x2._4, x2._5, x2._6, x2._7, x2._8, x2._9, x2._10, x2._11))
  }

}
