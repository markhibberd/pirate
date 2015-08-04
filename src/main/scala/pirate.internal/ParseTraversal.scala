package pirate.internal

import pirate._
import scalaz.{Name => _, _}, Scalaz._

// FIX current dumping ground for all of the dodgy code, need to work through and clean this up.

object ParseTraversal {
  sealed trait ParsedName
  case class LongParsedName(s: String) extends ParsedName
  case class ShortParsedName(c: Char) extends ParsedName
  case class ParsedWord(name: ParsedName, value: Option[String])

  type P[A] = EitherT[PWR, ParseError, A]
  type PWR[A] = WriterT[PR, List[String], A]
  type PR[A] = Reader[Prefs, A]

  def runParserFully[A](s: ParseState, p: Parse[A], args: List[String]): P[A] =
    runParser(s, p, args).flatMap({
      case (a, Nil) => a.pure[P]
      case (_, xs)  => errorP(ParseErrorLeftOver(xs))
    })

  def errorP[A](e: ParseError): P[A] =
    EitherT.left(e.pure[PWR])

  def hoistDisjunction[A](o: ParseTree[Info] \/ A): P[A] =
    o.fold(es => errorP(ParseErrorMissing(es)), a => a.pure[P])

  type NondetP[A] = NondetT[P, A]

  def search[F[_]: Monad, A](f: OptionRunner[F], parser: Parse[A]): NondetT[F, Parse[A]] =  {
    type NondetF[B] = NondetT[F, B]
    parser match {
      case ValueParse(_) =>
        NondetT.nil[F, Parse[A]]
      case ParserParse(p) =>
        f.run(p).map(_.pure[Parse])
      case ApParse(k, a) =>
        search(f, k).flatMap(x => (a <*> x).pure[NondetF]) <|>
          search(f, a).flatMap(x => (x <*> k).pure[NondetF])
      case AltParse(p1, p2) =>
        search(f, p1) ++ search(f, p2)
      case BindParse(k, a) =>
        search(f, a).flatMap(p => ParseTraversal.eval(true,p) match {
          case -\/(_)  => NondetT.nil[F, Parse[A]]
          case \/-(aa) => NondetT.singleton[F, Parse[A]](k(aa))
        })
    }
  }

  type StateArg[A] = StateT[P, List[String], A]
  type NondetArg[A] = NondetT[StateArg, A]

  def searchOpt[A](w: ParsedWord, p: Parse[A]): NondetArg[Parse[A]] =
    search(new OptionRunner[StateArg] {
      def run[B](options: Parser[B]): NondetT[StateArg, B] =
        optMatches(options, w) match {
          case None => NondetT.nil[StateArg, B]
          case Some(a) => NondetT.lift[StateArg, B](a)
        }
    }, p)

  def searchArg[A](arg: String, p: Parse[A]): NondetArg[Parse[A]] =
    search(new OptionRunner[StateArg] {
      def run[B](options: Parser[B]): NondetT[StateArg, B] =
         (if (options.isArg) NondetT.cut[StateArg] else ().pure[NondetArg]).flatMap(_ =>
            argMatches(options, arg) match {
              case None => NondetT.nil[StateArg, B]
              case Some(a) => NondetT.lift[StateArg, B](a)
         })
    }, p)

  def toParseError[A](r: ReadError, p: Parser[A]): ParseError =
    ParseErrorReadError(r, Usage.flags(p, OptHelpInfo(false, false)))

  def update[A](run: List[String] => P[(List[String], A)]): StateArg[A] =
    StateT[P, List[String], A](run)

  def optMatches[A](parser: Parser[A], w: ParsedWord): Option[StateArg[A]] = parser match {
    case SwitchParser(flag, _, a) =>
      w.name match {
        case ShortParsedName(c) =>
          flag.hasShort(c).option(
            update[A](args =>
              ((w.value.map(s => "-" ++ s).toList ++ args) -> a).pure[P]
            ))
        case LongParsedName(s) =>
          flag.hasLong(s).option(a.pure[StateArg])
      }
    case FlagParser(flag, meta, p) =>
      w.name match {
        case ShortParsedName(c) =>
          flag.hasShort(c).option(
            update[A](args => p.read(w.value.toList ++ args) match {
              case -\/(e) => errorP(toParseError(e, parser))
              case \/-(r) => r.pure[P]
            }))
        case LongParsedName(s) =>
          flag.hasLong(s).option(
            update[A](args => p.read(w.value.toList ++ args) match {
              case -\/(e) => errorP(toParseError(e, parser))
              case \/-(r) => r.pure[P]
            }))
      }
    case _ =>
      None
  }

  def argMatches[A](parser: Parser[A], arg: String): Option[StateArg[A]] = parser match {
    case ArgumentParser(_, p) =>
      update[A](args => p.read(arg :: args) match {
        case -\/(e) => errorP(toParseError(e, parser))
        case \/-(r) => r.pure[P]
      }).some
    case CommandParser(sub) =>
      if (sub.name === arg)
        StateT[P, List[String], A](args => for {
          _ <- EitherT.right[PWR,ParseError,Unit](().pure[PWR].<++:(arg :: Nil))
          prefs <- EitherT.right[PWR,ParseError,Prefs](WriterT[PR, List[String], Prefs](Reader(p => (nil -> p))))
          x <- if (prefs.backtrack)
            runParser(SkipOpts, sub.parse, args).map(_.swap)
          else
            runParserFully(SkipOpts, sub.parse, args).map(nil[String] -> _)
        } yield x).pure[Option]
      else
        None
    case _ =>
      None
  }

  def parseWord(arg: String): Option[ParsedWord] = arg.toList match {
    case '-' :: '-' :: w => w.indexOf('=') match {
      case -1 => Some(ParsedWord(LongParsedName(w.mkString), None))
      case i  =>
        val (cmd, value) = w.splitAt(i)
        Some(ParsedWord(LongParsedName(cmd.mkString), Some(value.tail.mkString)))
    }
    case '-' :: w :: rest =>
      Some(ParsedWord(ShortParsedName(w), if (rest.isEmpty) None else Some(rest.mkString)))
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
        searchArg(arg, p) ++ NondetT.hoistMaybe[StateArg, ParsedWord](parseWord(arg)).flatMap(searchOpt(_, p))
    }

  def runParser[A](s: ParseState, p: Parse[A], args: List[String]): P[(A, List[String])] =
    args match {
      case Nil => hoistDisjunction(ParseTraversal.eval(false,p).map(_ -> args))
      case "--" :: rest => runParser(AllowOpts, p, rest)
      case arg :: restArgs =>
        stepParser(s, arg, p).disamb.run(restArgs).flatMap({
          case (_, None) =>
            ParseTraversal.eval(false,p) match {
              case -\/(_) =>
                parseError(arg)
              case \/-(a) =>
                (a, args).pure[P]
            }
          case (rest, Some(pp)) =>
            runParser(s, pp, rest)
        })
    }

  def parseError[A](arg: String): P[A] =
    arg.toList match {
      case '-' :: _ => errorP(ParseErrorInvalidOption(arg))
      case _        => errorP(ParseErrorInvalidArgument(arg))
    }

  def eval[A](multi: Boolean, p: Parse[A]): ParseTree[Info] \/ A = (p match {
    case ValueParse(o) =>
      o.right
    case ParserParse(p) =>
      if (p.isVisible) ParseTreeLeaf(Usage.flags(p, OptHelpInfo(multi, false))).left
      else ParseTreeAp(nil[ParseTree[Info]]).left
    case ApParse(k, a) =>
      evalAp(multi,k, a)
    case AltParse(a, b) =>
      evalAlt[A](multi,a, b)
    case BindParse(k, a) =>
      eval(true,a).flatMap(k.map(eval(true,_)))
  }).leftMap(_.simplify)

  def evalAlt[A](multi: Boolean, a: Parse[A], b: Parse[A]): ParseTree[Info] \/ A =
    eval(multi,a) -> eval(multi,b) match {
      case (\/-(a),_) => a.right
      case (_,\/-(b)) => b.right
      case (-\/(a), -\/(b)) => ParseTreeAlt(a :: b :: Nil).left
    }

  def evalAp[A,B](multi: Boolean, f: Parse[B => A], k: Parse[B]): ParseTree[Info] \/ A =
    eval(multi,k) <*> eval(multi,f) match {
      case \/-(a)  => a.right
      case -\/(xs) => eval(multi,f) -> eval(multi,k) match {
        case (-\/(a),-\/(b)) => ParseTreeAp(List(a, b)).left
        case (_,-\/(b)) => ParseTreeAp(List(b)).left
        case (-\/(a),_) => ParseTreeAp(List(a)).left
        case _ => ParseTreeAp(nil[ParseTree[Info]]).left
      }
    }

  def mapTraverse[A, B](self: Parse[A], f: TreeTraverseF[B]): List[B] =
    treeTraverse(self, f).flatten

  def treeTraverse[A, B](self: Parse[A], f: TreeTraverseF[B]): ParseTree[B] = {
    def hasDefault[X](p: Parse[X]): Boolean =
      eval(false,p).isRight

    def go[C](multi: Boolean, dfault: Boolean, f: TreeTraverseF[B], parse: Parse[C]): ParseTree[B] = parse match {
      case ValueParse(_) =>
        ParseTreeAp(Nil)
      case ParserParse(p) =>
        if (p.isVisible) ParseTreeLeaf(f.run(OptHelpInfo(multi, dfault), p))
        else ParseTreeAp(Nil)
      case ApParse(p1, p2) =>
        ParseTreeAp(List(go(multi, dfault, f, p1), go(multi, dfault, f, p2)))
      case AltParse(p1, p2) =>
        val dfaultx = dfault || hasDefault(p1) || hasDefault(p2)
        ParseTreeAlt(List(go(multi, dfaultx, f, p1), go(multi, dfaultx, f, p2)))
      case BindParse(k, p) =>
        go(true, dfault, f, p)
    }

    go(false, false, f, self).simplify
  }
}

case class OptHelpInfo(multi: Boolean, dfault: Boolean)

sealed trait ParseTree[A] {
  def flatten: List[A] = this match {
    case ParseTreeLeaf(value) => List(value)
    case ParseTreeAp(children) => children.flatMap(_.flatten)
    case ParseTreeAlt(children) => children.flatMap(_.flatten)
  }

  def simplify: ParseTree[A] = this match {
    case ParseTreeLeaf(a) => ParseTreeLeaf(a)
    case ParseTreeAp(x :: Nil) => x.simplify
    case ParseTreeAp(xs) => ParseTreeAp(xs.map(_.simplify).flatMap({
      case ParseTreeAp(ys) => ys
      case ParseTreeAlt(Nil) => Nil
      case x => List(x)
    }))
    case ParseTreeAlt(x :: Nil) => x.simplify
    case ParseTreeAlt(xs) => ParseTreeAlt(xs.map(_.simplify).flatMap({
      case ParseTreeAlt(ys) => ys
      case ParseTreeAp(Nil) => Nil
      case x => List(x)
    }))
  }
}
case class ParseTreeLeaf[A](value: A) extends ParseTree[A]
case class ParseTreeAp[A](children: List[ParseTree[A]]) extends ParseTree[A]
case class ParseTreeAlt[A](children: List[ParseTree[A]]) extends ParseTree[A]

/* A universally qualified function for handling the existantial parsers in the tree */
trait OptionRunner[F[_]] {
  def run[A](options: Parser[A]): NondetT[F, A]
}

trait TreeTraverseF[A] {
  def run[X](info: OptHelpInfo, p: Parser[X]): A
}
