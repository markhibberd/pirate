package io.mth.pirate.internal

import scalaz._, Scalaz._, \&/._
import io.mth.pirate._

object Interpretter {
  sealed trait OptName
  case class LongName(s: String) extends OptName
  case class ShortName(c: Char) extends OptName
  case class OptWord(name: OptName, value: Option[String])

  def run[A](p: Parse[A], args: List[String]): ParseError \/ A =
    runParserFully(SkipOpts, p, args).run(NullPrefs)

  def zeroP[A]: P[A] =
    errorP(ParseErrorNoMessage)

  def errorP[A](e: ParseError): P[A] =
    P[A](_ => e.left)

  def errorMessageP[A](e: String): P[A] =
    errorP(ParseErrorMessage(e))

  def exitP[A, B](p: Parse[B], o: Option[A]): P[A] =
    hoistMaybe(o)

  def hoistMaybe[A](o: Option[A]): P[A] =
    o.map(a => P[A](_ => a.right)).getOrElse(errorP(ParseErrorNoMessage))

  def tryP[A](p: P[A]): P[ParseError \/ A] =
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
      def run[A](options: Parser[A]): NondetT[StateArg, A] =
        optMatches(options, w) match {
          case None => NondetT.nil[StateArg, A]
          case Some(a) => NondetT.lift[StateArg, A](a)
        }
    }, p)

  def searchArg[A](arg: String, p: Parse[A]): NondetArg[Parse[A]] =
    search(new OptionRunner[StateArg] {
      def run[A](options: Parser[A]): NondetT[StateArg, A] =
        argMatches(options, arg) match {
          case None => NondetT.nil[StateArg, A]
          case Some(a) => NondetT.lift[StateArg, A](a)
        }
    }, p)

  def update[A](run: List[String] => P[(List[String], A)]): StateArg[A] =
    StateT[P, List[String], A](run)

  def optMatches[A](p: Parser[A], w: OptWord): Option[StateArg[A]] = p match {
    case FlagParser(flag, a) =>
      w.name match {
        case ShortName(c) =>
          flag.hasShort(c).option(a.pure[StateArg])
        case LongName(s) =>
          flag.hasLong(s).option(a.pure[StateArg])
      }
    case OptionParser(flag, metas, p) =>
      w.name match {
        case ShortName(c) =>
          flag.hasShort(c).option(
            update[A](args => p.read(args) match {
              case -\/(e) => errorP(ParseErrorMessage(e.toString))
              case \/-(r) => r.pure[P]
            }))
        case LongName(s) =>
          flag.hasLong(s).option(
            update[A](args => p.read(args) match {
              case -\/(e) => errorP(ParseErrorMessage(e.toString))
              case \/-(r) => r.pure[P]
            }))
      }
    case _ =>
      None
  }


  def argMatches[A](p: Parser[A], arg: String): Option[StateArg[A]] = p match {
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
