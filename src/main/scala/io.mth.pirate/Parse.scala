package io.mth.pirate

import scalaz._, Scalaz._, \&/._

sealed trait ParseTree[A] {
  def flatten: List[A] = this match {
    case ParseTreeLeaf(value) => List(value)
    case ParseTreeAp(children) => children.flatMap(_.flatten)
    case ParseTreeAlt(children) => children.flatMap(_.flatten)
  }

}
case class ParseTreeLeaf[A](value: A) extends ParseTree[A]
case class ParseTreeAp[A](children: List[ParseTree[A]]) extends ParseTree[A]
case class ParseTreeAlt[A](children: List[ParseTree[A]]) extends ParseTree[A]

sealed trait PPrefs
case object NullPPrefs extends PPrefs
sealed trait PError
case object PErrorNoMessage extends PError
case class PErrorMessage(s: String) extends PError

case class P[+A](run: PPrefs => PError \/ A) {
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

/* A universally qualified function for handling the existantial parsers in the tree */
trait OptionRunner[F[+_]] {
  def run[A](options: PirateParser[A]): NondetT[F, A]
}

object ParseTools {
  def zeroP[A]: P[A] =
    errorP(PErrorNoMessage)

  def errorP[A](e: PError): P[A] =
    P[A](_ => e.left)

  def errorMessageP[A](e: String): P[A] =
    errorP(PErrorMessage(e))

  def exitP[A, B](p: Parse[B], o: Option[A]): P[A] =
    hoistMaybe(o)

  def hoistMaybe[A](o: Option[A]): P[A] =
    o.map(a => P[A](_ => a.right)).getOrElse(errorP(PErrorNoMessage))

  def tryP[A](p: P[A]): P[PError \/ A] =
    P(prefs => p.run(prefs).right)

  type NondetP[+A] = NondetT[P, A]

  def search[F[+_]: Monad, A](f: OptionRunner[F], parser: Parse[A]): NondetT[F, Parse[A]] =  {
    type NondetF[A] = NondetT[F, A]
    parser match {
      case ValueParse(_) =>
        NondetT.nil[F, Parse[A]]
      case PiratedParse(p, m) =>
        f.run(p).map(_.pure[Parse])
      case ApParse(k, a) =>
        search(f, k).flatMap(x => (a <*> x).pure[NondetF]) <|>
      search(f, a).flatMap(x => (x <*> k).pure[NondetF])
      case AltParse(p1, p2) =>
        search(f, p1) ++ search(f, p2)
      case BindParse(k, a) =>
        search(f, a).flatMap(p => p.eval match {
          case None => NondetT.nil[F, Parse[A]]
          case Some(aa) => NondetT.singleton[F, Parse[A]](k(aa))
        })
    }
  }

  type StateArg[+A] = StateT[P, List[String], A]
  type NondetArg[+A] = NondetT[StateArg, A]

  def searchOpt[A](w: OptWord, p: Parse[A]): NondetArg[Parse[A]] =
    search(new OptionRunner[StateArg] {
      def run[A](options: PirateParser[A]): NondetT[StateArg, A] =
        optMatches(options, w) match {
          case None => NondetT.nil[StateArg, A]
          case Some(a) => NondetT.lift[StateArg, A](a)
        }
    }, p)

  def searchArg[A](arg: String, p: Parse[A]): NondetArg[Parse[A]] =
    search(new OptionRunner[StateArg] {
      def run[A](options: PirateParser[A]): NondetT[StateArg, A] =
        argMatches(options, arg) match {
          case None => NondetT.nil[StateArg, A]
          case Some(a) => NondetT.lift[StateArg, A](a)
        }
    }, p)

  def update[A](run: List[String] => P[(List[String], A)]): StateArg[A] =
    StateT[P, List[String], A](run)

  def optMatches[A](p: PirateParser[A], w: OptWord): Option[StateArg[A]] = p match {
    case FlagParser(flag, a) =>
      w.name match {
        case ShortName(c) => if (flag.a.exists(_ == c)) Some(a.pure[StateArg]) else None
        case LongName(s) => if (flag.b.exists(_ == s)) Some(a.pure[StateArg]) else None
      }
    case OptionParser(flag, metas, p) =>
      w.name match {
        case ShortName(c) =>
          if (flag.a.exists(_ == c))
            Some(update[A](args => p.read(args) match {
              case -\/(e) => errorP(PErrorMessage(e.toString))
              case \/-(r) => r.pure[P]
            }))
          else
            None
        case LongName(s) =>
          if (flag.b.exists(_ == s))
            Some(update[A](args => p.read(args) match {
              case -\/(e) => errorP(PErrorMessage(e.toString))
              case \/-(r) => r.pure[P]
            }))
        else None
      }
    case _ =>
      None
  }


  def argMatches[A](p: PirateParser[A], arg: String): Option[StateArg[A]] = p match {
    case ArgumentParser(p) =>
      p.read(arg :: Nil) match {
        case -\/(e) => None
        case \/-((Nil, a)) => Some(a.pure[StateArg])
        case \/-((_ :: _, _)) => None
      }
    case SubCommandParser(name, p) =>
      if (name == arg)
        Some(
          StateT[P, List[String], A](args =>
            runParserFully(SkipOpts, p, args).map(Nil -> _)
          )
        )
      else
        None
    case _ =>
      None
  }

  // FIX add support for -x=val syntax
  def parseWord(arg: String): Option[OptWord] = arg.toList match {
    case '-' :: '-' :: w =>
      Some(OptWord(LongName(w.mkString), None))
    case '-' :: w :: Nil =>
      Some(OptWord(ShortName(w), None))
    case _ =>
      None
  }

  def stepParser[A](s: ParseState, arg: String, p: Parse[A]): NondetT[StateArg, Parse[A]] =
    s match {
      case SkipOpts => parseWord(arg) match {
        case None => searchArg(arg, p)
        case Some(w) => searchOpt(w, p)
      }
      case AllowOpts =>
        searchArg(arg, p) ++ NondetT.hoistMaybe[StateArg, OptWord](parseWord(arg)).flatMap(searchOpt(_, p))
    }

  def runParser[A](s: ParseState, p: Parse[A], args: List[String]): P[(A, List[String])] =
    args match {
      case Nil => exitP(p, p.eval.map(_ -> args))
      case "--" :: rest => runParser(s, p, rest)
      case arg :: rest =>
        stepParser(s, arg, p).disamb.run(rest).flatMap({
          case (rest, None) =>
            p.eval match {
              case None =>
                zeroP
              case Some(a) =>
                (a, rest).pure[P]
            }
          case (rest, Some(pp)) =>
            runParser(s, pp, rest)
        })
    }

  def runParserFully[A](s: ParseState, p: Parse[A], args: List[String]): P[A] =
    runParser(s, p, args).flatMap({
      case (a, Nil) => a.pure[P]
      case (_, xs)  => errorMessageP("left over arguments: " + xs)
    })
}


sealed trait OptName
case class LongName(s: String) extends OptName
case class ShortName(c: Char) extends OptName

case class OptWord(name: OptName, value: Option[String])

sealed trait Parse[A] {
  def ~(name: String): Command[A] =
    Command(name, None, this)

  def map[B](f: A => B): Parse[B] = this match {
    case ValueParse(o) =>
      ValueParse(o.map(f))
    case PiratedParse(p, m) =>
      PiratedParse(p.map(f), m)
    case ApParse(k, a) =>
      ApParse(k.map(ff => ff.map(f)), a)
    case AltParse(a, b) =>
      AltParse(a.map(f), b.map(f))
    case BindParse(k, a) =>
      BindParse(k.map(_.map(f)), a)
  }

  def flatMap[B](f: A => Parse[B]): Parse[B] =
    BindParse(f, this)

  def |||(other: Parse[A]): Parse[A] =
    AltParse(this, other)

  def eval: Option[A] = this match {
    case ValueParse(o) =>
      o
    case PiratedParse(p, m) =>
      None
    case ApParse(k, a) =>
      a.eval <*> k.eval
    case AltParse(a, b) =>
      a.eval <+> b.eval
    case BindParse(k, a) =>
      a.eval >>= k.map(_.eval)
  }

  def mapTraverse[B](f: TreeTraverseF[B]): List[B] =
    treeTraverse(f).flatten

  def treeTraverse[B](f: TreeTraverseF[B]): ParseTree[B] = {
    def hasDefault[X](p: Parse[X]): Boolean =
      p.eval.isDefined

    def go[C](multi: Boolean, dfault: Boolean, f: TreeTraverseF[B], p: Parse[C]): ParseTree[B] = p match {
      case ValueParse(_) =>
        ParseTreeAp(Nil)
      case PiratedParse(p, m) =>
        ParseTreeLeaf(f.run(OptHelpInfo(multi, dfault), p, m))
      case ApParse(p1, p2) =>
        ParseTreeAp(List(go(multi, dfault, f, p1), go(multi, dfault, f, p2)))
      case AltParse(p1, p2) =>
        val dfaultx = dfault || hasDefault(p1) || hasDefault(p2)
        ParseTreeAlt(List(go(multi, dfaultx, f, p1), go(multi, dfaultx, f, p2)))
      case BindParse(k, p) =>
        go(true, dfault, f, p)
    }

    def simplify[X](x: ParseTree[X]): ParseTree[X] = x match {
      case ParseTreeLeaf(a) => ParseTreeLeaf(a)
      case ParseTreeAp(xs) => ParseTreeAp(xs.map(simplify).flatMap({
        case ParseTreeAp(ys) => ys
        case ParseTreeAlt(Nil) => Nil
        case x => List(x)
      }))
      case ParseTreeAlt(xs) => ParseTreeAlt(xs.map(simplify).flatMap({
        case ParseTreeAlt(ys) => ys
        case ParseTreeAp(Nil) => Nil
        case x => List(x)
      }))
    }

    simplify(go(false, false, f, this))
  }

}

trait TreeTraverseF[A] {
  def run[X](info: OptHelpInfo, p: PirateParser[X], m: PirateMeta): A
}


case class OptHelpInfo(multi: Boolean, dfault: Boolean)

case class ValueParse[A](m: Option[A]) extends Parse[A]
case class PiratedParse[A](p: PirateParser[A], m: PirateMeta) extends Parse[A]
case class ApParse[A, B](f: Parse[A => B], a: Parse[A]) extends Parse[B]
case class AltParse[A](a: Parse[A], b: Parse[A]) extends Parse[A]
case class BindParse[A, B](f: A => Parse[B], a: Parse[A]) extends Parse[B]

object Parse {
  implicit def ParseMonad: Monad[Parse] with Plus[Parse] = new Monad[Parse] with Plus[Parse] {
    def point[A](a: => A) = ValueParse(Some(a))
    override def map[A, B](a: Parse[A])(f: A => B) = a map f
    def bind[A, B](a: Parse[A])(f: A => Parse[B]) = a flatMap f
    override def ap[A, B](a: => Parse[A])(f: => Parse[A => B]) = ApParse(f, a)
    def plus[A](a: Parse[A], b: => Parse[A]) = AltParse(a, b)
  }
}


case class PirateMeta(description: Option[String], visible: Boolean)

sealed trait PirateParser[A] {
  def map[B](f: A => B): PirateParser[B] = this match {
    case FlagParser(flag, a) =>
      FlagParser(flag, f(a))
    case OptionParser(flag, metas, p) =>
      OptionParser(flag, metas, p.map(f))
    case ArgumentParser(p) =>
      ArgumentParser(p.map(f))
    case SubCommandParser(name, p) =>
      SubCommandParser(name, p.map(f))
  }
}

case class FlagParser[A](flag: These[Char, String], a: A)
  extends PirateParser[A]
case class OptionParser[A](flag: These[Char, String], metas: List[String], p: Read[A])
  extends PirateParser[A]
case class ArgumentParser[A](p: Read[A])
  extends PirateParser[A]
case class SubCommandParser[A](name: String, p: Parse[A])
  extends PirateParser[A]

sealed trait ParseState
case object SkipOpts extends ParseState
case object AllowOpts extends ParseState
