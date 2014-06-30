package io.mth.pirate.internal

import scalaz._, Scalaz._

import NondetT.{Cut, CutT}

case class NondetT[F[+_], +A](runNondetT: ListT[CutT[F]#l, A]) {
  def ++[AA >: A](other: NondetT[F, AA])(implicit F: Monad[F]): NondetT[F, AA] =
    NondetT(runNondetT ++ other.runNondetT)

  def map[B](f: A => B)(implicit F: Monad[F]): NondetT[F, B] =
    NondetT(runNondetT.map(f))

  def flatMap[B](f: A => NondetT[F, B])(implicit F: Monad[F]): NondetT[F, B] =
    NondetT(runNondetT.flatMap(a => f(a).runNondetT))

  def <|>[AA >: A](other: NondetT[F, AA])(implicit F: Monad[F]): NondetT[F, AA] =
    NondetT(runNondetT ++ (for {
      s <- NondetT.getl[F]
      _ <- if (s) ListT.nil[CutT[F]#l, Unit] else ListT.singleton[CutT[F]#l, Unit](())
      r <- other.runNondetT
    } yield r))

  def disamb(implicit F: Monad[F]): F[Option[A]] =
    runNondetT.take(1).run.run(false).map(x => x._2 match {
      case h :: Nil => Some(h)
      case _ => None
    })
}

object NondetT {
  type Cut[F[+_], +A] = StateT[F, Boolean, A]
  trait CutT[F[+_]] { type l[+a] = Cut[F, a] }

  def nil[F[+_]: Monad, A]: NondetT[F, A] =
    NondetT(ListT.nil[CutT[F]#l, A])

  def singleton[F[+_]: Monad, A](a: A): NondetT[F, A] =
    NondetT(ListT.singleton[CutT[F]#l, A](a))

  def gets[F[+_]: Monad]: StateT[F, Boolean, Boolean] =
    StateT[F, Boolean, Boolean](s => (s, s).pure[F])

  def getc[F[+_]: Monad]: Cut[F, TStep[Boolean, ListT[CutT[F]#l, Boolean]]] =
    gets[F].map(b => TStep.cons(b, ListT.nil[CutT[F]#l, Boolean]))

  def getl[F[+_]: Monad]: ListT[CutT[F]#l, Boolean] =
    ListT[CutT[F]#l, Boolean](getc[F])

  def get[F[+_]: Monad]: NondetT[F, Boolean] =
    NondetT[F, Boolean](getl[F])

  def cuts[F[+_]: Monad]: StateT[F, Boolean, Unit] =
    StateT[F, Boolean, Unit](_ => (true, ()).pure[F])

  def cutc[F[+_]: Monad]: Cut[F, TStep[Unit, ListT[CutT[F]#l, Unit]]] =
    cuts[F].map(b => TStep.cons(b, ListT.nil[CutT[F]#l, Unit]))

  def cutl[F[+_]: Monad]: ListT[CutT[F]#l, Unit] =
    ListT[CutT[F]#l, Unit](cutc[F])

  def cut[F[+_]: Monad]: NondetT[F, Unit] =
    NondetT[F, Unit](cutl[F])

  def hoistMaybe[F[+_]: Monad, A](o: Option[A]): NondetT[F, A] =
    o.map(singleton[F, A]).getOrElse(nil[F, A])

  def lift[F[+_]: Monad, A](f: F[A]): NondetT[F, A] =
    NondetT[F, A](ListT.lift[CutT[F]#l, A](f.liftM[Cut]))

  implicit def NondetTMonad[F[+_]: Monad]: Monad[({ type l[a] = NondetT[F, a] })#l] with MonadPlus[({ type l[a] = NondetT[F, a] })#l] = new Monad[({ type l[a] = NondetT[F, a] })#l] with MonadPlus[({ type l[a] = NondetT[F, a] })#l] {
    def point[A](a: => A) = singleton[F, A](a)
    def bind[A, B](a: NondetT[F, A])(f: A => NondetT[F, B]) = a flatMap f
    def empty[A] = nil[F, A]
    def plus[A](a: NondetT[F, A], b: => NondetT[F, A]) = NondetT(a.runNondetT ++ b.runNondetT)
  }
}
