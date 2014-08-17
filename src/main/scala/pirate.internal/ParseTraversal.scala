package pirate.internal

import pirate._
import scalaz._, Scalaz._

// FIX current dumping ground for all of the dodgy code, need to work through and clean this up.

object ParseTraversal {
  sealed trait ParsedName
  case class LongParsedName(s: String) extends ParsedName
  case class ShortParsedName(c: Char) extends ParsedName
  case class ParsedWord(name: ParsedName, value: Option[String])

  def runParserFully[A](s: ParseState, p: Parse[A], args: List[String]): P[A] =
    runParser(s, p, args).flatMap({
      case (a, Nil) => a.pure[P]
      case (_, xs)  => errorMessageP("left over arguments: " + xs)
    })


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
    type NondetF[B] = NondetT[F, B]
    parser match {
      case ValueParse(_) =>
        NondetT.nil[F, Parse[A]]
      case ParserParse(p, m) =>
        f.run(p).map(_.pure[Parse])
      case ApParse(k, a) =>
        search(f, k).flatMap(x => (a <*> x).pure[NondetF]) <|>
      search(f, a).flatMap(x => (x <*> k).pure[NondetF])
      case AltParse(p1, p2) =>
        search(f, p1) ++ search(f, p2)
      case BindParse(k, a) =>
        search(f, a).flatMap(p => ParseTraversal.eval(p) match {
          case None => NondetT.nil[F, Parse[A]]
          case Some(aa) => NondetT.singleton[F, Parse[A]](k(aa))
        })
    }
  }

  type StateArg[+A] = StateT[P, List[String], A]
  type NondetArg[+A] = NondetT[StateArg, A]

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
        argMatches(options, arg) match {
          case None => NondetT.nil[StateArg, B]
          case Some(a) => NondetT.lift[StateArg, B](a)
        }
    }, p)

  def update[A](run: List[String] => P[(List[String], A)]): StateArg[A] =
    StateT[P, List[String], A](run)

  def optMatches[A](parser: Parser[A], w: ParsedWord): Option[StateArg[A]] = parser match {
    case SwitchParser(flag, a) =>
      w.name match {
        case ShortParsedName(c) =>
          flag.hasShort(c).option(a.pure[StateArg])
        case LongParsedName(s) =>
          flag.hasLong(s).option(a.pure[StateArg])
      }
    case FlagParser(flag, metas, p) =>
      w.name match {
        case ShortParsedName(c) =>
          flag.hasShort(c).option(
            update[A](args => p.read(w.value.toList ++ args) match {
              case -\/(e) => errorP(ParseErrorMessage(e.toString))
              case \/-(r) => r.pure[P]
            }))
        case LongParsedName(s) =>
          flag.hasLong(s).option(
            update[A](args => p.read(w.value.toList ++ args) match {
              case -\/(e) => errorP(ParseErrorMessage(e.toString))
              case \/-(r) => r.pure[P]
            }))
      }
    case _ =>
      None
  }


  def argMatches[A](parser: Parser[A], arg: String): Option[StateArg[A]] = parser match {
    case ArgumentParser(p) =>
      p.read(arg :: Nil) match {
        case -\/(e) => None
        case \/-((Nil, a)) => Some(a.pure[StateArg])
        case \/-((_ :: _, _)) => None
      }
    case CommandParser(name, p) =>
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

  def parseWord(arg: String): Option[ParsedWord] = arg.toList match {
    case '-' :: '-' :: w => w.indexOf('=') match {
      case -1 => Some(ParsedWord(LongParsedName(w.mkString), None))
      case i  =>
        val (cmd, value) = w.splitAt(i)
        Some(ParsedWord(LongParsedName(cmd.mkString), Some(value.tail.mkString)))
    }
    case '-' :: w :: Nil =>
      Some(ParsedWord(ShortParsedName(w), None))
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
      case Nil => exitP(p, ParseTraversal.eval(p).map(_ -> args))
      case "--" :: rest => runParser(s, p, rest)
      case arg :: restArgs =>
        stepParser(s, arg, p).disamb.run(restArgs).flatMap({
          case (r, None) =>
            ParseTraversal.eval(p) match {
              case None =>
                zeroP
              case Some(a) =>
                (a, r).pure[P]
            }
          case (rest, Some(pp)) =>
            runParser(s, pp, rest)
        })
    }

  def eval[A](p: Parse[A]): Option[A] = p match {
    case ValueParse(o) =>
      o
    case ParserParse(p, m) =>
      None
    case ApParse(k, a) =>
      eval(a) <*> eval(k)
    case AltParse(a, b) =>
      eval(a) <+> eval(b)
    case BindParse(k, a) =>
      eval(a) >>= k.map(eval)
  }

  def mapTraverse[A, B](self: Parse[A], f: TreeTraverseF[B]): List[B] =
    treeTraverse(self, f).flatten

  def treeTraverse[A, B](self: Parse[A], f: TreeTraverseF[B]): ParseTree[B] = {
    def hasDefault[X](p: Parse[X]): Boolean =
      eval(p).isDefined

    def go[C](multi: Boolean, dfault: Boolean, f: TreeTraverseF[B], parse: Parse[C]): ParseTree[B] = parse match {
      case ValueParse(_) =>
        ParseTreeAp(Nil)
      case ParserParse(p, m) =>
        ParseTreeLeaf(f.run(OptHelpInfo(multi, dfault), p, m))
      case ApParse(p1, p2) =>
        ParseTreeAp(List(go(multi, dfault, f, p1), go(multi, dfault, f, p2)))
      case AltParse(p1, p2) =>
        val dfaultx = dfault || hasDefault(p1) || hasDefault(p2)
        ParseTreeAlt(List(go(multi, dfaultx, f, p1), go(multi, dfaultx, f, p2)))
      case BindParse(k, p) =>
        go(true, dfault, f, p)
    }

    def simplify[X](pt: ParseTree[X]): ParseTree[X] = pt match {
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

    simplify(go(false, false, f, self))
  }

}

case class OptHelpInfo(multi: Boolean, dfault: Boolean)

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

/* A universally qualified function for handling the existantial parsers in the tree */
trait OptionRunner[F[+_]] {
  def run[A](options: Parser[A]): NondetT[F, A]
}

trait TreeTraverseF[A] {
  def run[X](info: OptHelpInfo, p: Parser[X], m: Metadata): A
}
