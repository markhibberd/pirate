package pirate.internal

import scalaz._, Scalaz._

case class ListT[F[_], A](stepListT: F[TStep[A, ListT[F, A]]]) {
  def ++(other: ListT[F, A])(implicit F: Monad[F]): ListT[F, A] = ListT(for {
    s <- stepListT
    r <- s match {
      case TNil() =>
        other.stepListT
      case TCons(a, x) =>
        TStep.cons(a, x ++ other).pure[F]
    }
  } yield r)

  def map[B](f: A => B)(implicit F: Monad[F]): ListT[F, B] =
    ListT(stepListT.map(x => x.bimap(f, _.map(f))))

  def flatMap[B](f: A => ListT[F, B])(implicit F: Monad[F]): ListT[F, B] = ListT(for {
    s <- stepListT
    r <- s match {
      case TNil() =>
        TStep.nil[B, ListT[F, B]].pure[F]
      case TCons(a, x) =>
        (f(a) ++ x.flatMap(f)).stepListT
    }
  } yield r)

  def run(implicit F: Monad[F]): F[List[A]] = for {
    s <- stepListT
    r <- s match {
      case TNil() => Nil.pure[F]
      case TCons(a, x) => x.run.map(a :: _)
    }
  } yield r

  def take(n: Int)(implicit F: Monad[F]): ListT[F, A] =
    if (n == 0)
      ListT.nil[F, A]
    else
      ListT(stepListT.map(x => x.bimap(identity, _.take(n - 1))))
}

object ListT {
  def singleton[F[_]: Monad, A](a: A): ListT[F, A] =
    cons(a, nil[F, A])

  def nil[F[_]: Monad, A]: ListT[F, A] =
    ListT(TStep.nil[A, ListT[F, A]].pure[F])

  def cons[F[_]: Monad, A](a: A, as: ListT[F, A]): ListT[F, A]  =
    ListT(TStep.cons[A, ListT[F, A]](a, as).pure[F])

  def hoist[F[_]: Monad, A](xs: List[A]): ListT[F, A] =
    xs.foldRight(nil[F, A])((el, acc) => cons(el, acc))

  def lift[F[_]: Monad, A](f: F[A]): ListT[F, A] =
    ListT(f.map(x => TStep.cons(x, nil[F, A])))

  implicit def ListTInstances[F[+_]: Monad]: Monad[({ type l[a] = ListT[F, a] })#l] with MonadPlus[({ type l[a] = ListT[F, a] })#l]  = new Monad[({ type l[a] = ListT[F, a] })#l] with MonadPlus[({ type l[a] = ListT[F, a] })#l]{
    def point[A](a: => A) = singleton[F, A](a)
    override def map[A, B](a: ListT[F, A])(f: A => B) = a map f
    def bind[A, B](a: ListT[F, A])(f: A => ListT[F, B]) = a flatMap f
    def empty[A] = nil[F, A]
    def plus[A](a: ListT[F, A], b: => ListT[F, A]) = a ++ b
  }
}

sealed trait TStep[A, X] {
  def bimap[B, Y](f: A => B, g: X => Y): TStep[B, Y] =
    this match {
      case TNil() => TNil()
      case TCons(a, x) => TCons(f(a), g(x))
    }
}

case class TNil[A, X]() extends TStep[A, X]
case class TCons[A, X](a: A, x: X) extends TStep[A, X]

object TStep {
  def nil[A, X]: TStep[A, X] =
    TNil[A, X]()

  def cons[A, X](a: A, x: X): TStep[A, X] =
    TCons(a, x)

  implicit def TStepBifunctor: Bifunctor[TStep] = new Bifunctor[TStep] {
    def bimap[A, B, C, D](ab: TStep[A, B])(f: A => C, g: B => D) =
      ab.bimap(f, g)
  }

  implicit def TStepEqual[A: Equal, X: Equal]: Equal[TStep[A, X]] =
    Equal.equal[TStep[A, X]]((a, b) => (a, b) match {
      case (TNil(), TNil()) => true
      case (TNil(), TCons(_, _)) => false
      case (TCons(_, _), TNil()) => false
      case (TCons(aa, ax), TCons(ba, bx)) => aa === ba && ax === bx
    })
}
