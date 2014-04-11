 package io.mth.piratex

import io.mth.pirate.Parser
import scalaz._, Scalaz._, \&/._

object NewApi {
  object switch {
    def short(c: Char): ParserX[Boolean] =
      PiratedParser(
        FlagParser(This(c), true),
        PirateMeta(None, true)) ||| false.pure[ParserX]
  }

  object option {
    def short(c: Char, meta: String): ParserX[String] =
      PiratedParser(
        OptionParser(This(c), List(meta), Parser.string),
        PirateMeta(None, true)) ||| ValueParser(None)
  }

  object positional {
    def one(meta: String): ParserX[String] =
      PiratedParser(
        ArgumentParser(Parser.string),
        PirateMeta(None, true)) ||| ValueParser(None)

  }

  object command {
    def of[A](name: String, p: ParserX[A]): ParserX[A] =
      PiratedParser(SubCommandParser(name, p), PirateMeta(None, true)) ||| ValueParser(None)


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
case class OptionParser[A](flag: These[Char, String], metas: List[String], p: Parser[A])
  extends PirateParser[A]
case class ArgumentParser[A](p: Parser[A])
  extends PirateParser[A]
case class SubCommandParser[A](name: String, p: ParserX[A])
  extends PirateParser[A]

case class DocumentedPirateParser[A](p: PirateParser[A], m: PirateMeta) {
  def map[B](f: A => B): DocumentedPirateParser[B] =
    DocumentedPirateParser(p.map(f), m)
}

sealed trait ParserX[A] {
  def map[B](f: A => B): ParserX[B] = this match {
    case ValueParser(o) =>
      ValueParser(o.map(f))
    case PiratedParser(p, m) =>
      PiratedParser(p.map(f), m)
    case ApParser(k, a) =>
      ApParser(k.map(ff => ff.map(f)), a)
    case AltParser(a, b) =>
      AltParser(a.map(f), b.map(f))
    case BindParser(k, a) =>
      BindParser(k.map(_.map(f)), a)
  }

  def flatMap[B](f: A => ParserX[B]): ParserX[B] =
    BindParser(f, this)

  def |||(other: ParserX[A]): ParserX[A] =
    AltParser(this, other)

  def eval: Option[A] = this match {
    case ValueParser(o) =>
      o
    case PiratedParser(p, m) =>
      None
    case ApParser(k, a) =>
      a.eval <*> k.eval
    case AltParser(a, b) =>
      a.eval <+> b.eval
    case BindParser(k, a) =>
      a.eval >>= k.map(_.eval)
  }


/*
  def run(args: List[String]): ParserError \/ (A, List[String]) =
    runState(args, AcceptOptions).map({ case (a, s, rest) => (a, rest) })

  def runState(args: List[String], s: ParseState): ParserError \/ (A, ParseState, List[String]) = args match {
    case Nil =>
      eval match {
        case None => Bomb("eval").left
        case Some(a) => (a, s, Nil).right
      }
    case "--" :: rest if s == AcceptOptions =>
      runState(rest, ExcludeOptions) // FIX stop option parsing
    case _ =>
      this match {
        case ValueParser(o) =>
          Bomb("value").left
        case PiratedParser(p) => p.p match {
          case FlagParser(flag, a) =>
            if (s == ExcludeOptions)
              Bomb("flag exclude").left
            else {
              val (beforea, rgs.span(x => !(flag.a.exists(x => s"-$x" == arg) || flag.b.exists(x => s"--$x" == arg)))

 if ()
              (a, s, rest).right
            else
              Bomb("flag not found").left
          case OptionParser(flag, meta, o) =>
            if (s == ExcludeOptions)
              Bomb("option exclude").left
            else if (flag.a.exists(x => s"-$x" == arg) || flag.b.exists(x => s"--$x" == arg))
              o.parse(rest).disjunction match {
                case -\/(s) => Bomb("option parse fail: " + s).left
                case \/-((rest, a)) => (a, s, rest).right
              }
            else
              Bomb("option not found").left
          case ArgumentParser(o) =>
            o.parse(args).disjunction match {
              case -\/(_) => Bomb("arg parse fail").left
              case \/-((rest, a)) => (a, s, rest).right
            }
          case SubCommandParser(name, p) =>
            ???
        }
        case ApParser(k, a) =>
          for { aa <- a.runState(args, s); (aaa, ss, rest) = aa; kk <- k.runState(rest, ss) } yield { val (kkk, sss, restx) = kk; (kkk(aaa), sss, restx) }
        case AltParser(a, b) =>
          a.runState(args, s) <+> b.runState(args, s)
        case BindParser(k, a) =>
          for { aa <- a.runState(args, s); (aaa, ss, rest) = aa; kk <- k(aaa).runState(rest, ss) } yield kk
      }
  } */
}

case class ValueParser[A](m: Option[A]) extends ParserX[A]
case class PiratedParser[A](p: PirateParser[A], m: PirateMeta) extends ParserX[A]
case class ApParser[A, B](f: ParserX[A => B], a: ParserX[A]) extends ParserX[B]
case class AltParser[A](a: ParserX[A], b: ParserX[A]) extends ParserX[A]
case class BindParser[A, B](f: A => ParserX[B], a: ParserX[A]) extends ParserX[B]

object ParserX {
  implicit def ParserXMonad: Monad[ParserX] with Plus[ParserX] = new Monad[ParserX] with Plus[ParserX] {
    def point[A](a: => A) = ValueParser(Some(a))
    override def map[A, B](a: ParserX[A])(f: A => B) = a map f
    def bind[A, B](a: ParserX[A])(f: A => ParserX[B]) = a flatMap f
    override def ap[A, B](a: => ParserX[A])(f: => ParserX[A => B]) = ApParser(f, a)
    def plus[A](a: ParserX[A], b: => ParserX[A]) =
      AltParser(a, b)
  }
}


sealed trait ParseError
case class Bomb(s: String) extends ParseError



sealed trait ParseResult[A] {
  def map[B](f: A => B): ParseResult[B] = this match {
    case ParseFail(error) => ParseFail(error)
    case ParseOk(args, state, value) => ParseOk(args, state, f(value))
  }

  def flatMap[B](f: A => ParseResult[B]): ParseResult[B] = this match {
    case ParseFail(error) => ParseFail(error)
    case ParseOk(args, state, value) => f(value)
  }

  def orElse(other: => ParseResult[A]): ParseResult[A] =
    this match {
      case ParseFail(_) => other
      case ParseOk(_, _, _) => this
    }
}

case class ParseFail[A](error: ParseError) extends ParseResult[A]
case class ParseOk[A](args: List[String], state: ParseState, value: A) extends ParseResult[A]

object ParseResult {
  implicit def ParseResultMonad: Monad[ParseResult] with Plus[ParseResult] = new Monad[ParseResult] with Plus[ParseResult] {
    def point[A](v: => A) = ParseOk(Nil, AcceptOptions, v)
    def bind[A, B](m: ParseResult[A])(f: A => ParseResult[B]) = m flatMap f
    def plus[A](a: ParseResult[A], b: => ParseResult[A]) =
      a orElse b

  }
}
object Interpret {

  def run[A](parser: ParserX[A], args: List[String], state: ParseState): ParseResult[A] =
    parser match {
      case ValueParser(None) =>
        println("aaaaaaa")
        ParseFail(Bomb("Mandatory argument not specified"))
      case ValueParser(Some(a)) =>
        println("bbbbbb")
        ParseOk(args, state, a)
      case p => args match {
        case Nil =>
          println("ccccc")
          p.eval match {
            case None =>
              println("ccccccc1111111")
              ParseFail(Bomb("Not enough arguments"))
            case Some(a) =>
              println("ccccccc2222222")
              ParseOk(Nil, state, a)
          }
        case h :: t => step(p, h, t, state) match {
          case ParseFail(e) =>
            println("dddddd")
            ParseFail(e)
          case ParseOk(argsx, statex, px) => px match {
            case ValueParser(None) if args != Nil =>
              println(s"eeeee: $px")
              stepArg(parser, args, state) match {
                case ParseFail(e) =>
                  ParseFail(e)
                case ParseOk(argsy, statey, py) =>
                  run(py, argsy, statey)
              }
            case _ =>
              println("fffffff: " +  px)
              run(px, argsx, statex)
          }

        }
      }
    }

  def stepArg[A](parser: ParserX[A], args: List[String], state: ParseState): ParseResult[ParserX[A]] = {
    println(s"stepArgs($parser)")
    parser match {
      case ValueParser(_) =>
        println("`-- aaaa")
        ParseOk(args, state, parser)
      case PiratedParser(p, m) => p match {
        case FlagParser(flag, a) =>
          println("`-- bbbb")
          ParseOk(args, state, ValueParser(None))
        case OptionParser(flag, meta, o) =>
          println("`-- cccc")
          ParseOk(args, state, ValueParser(None))
        case ArgumentParser(o) =>
          println("`-- dddd")
          o.parse(args).disjunction match {
            case -\/(e) => ParseFail(Bomb(e))
            case \/-((rest, a)) => ParseOk(args, state, ValueParser(Some(a)))
          }
        case SubCommandParser(name, p) =>
          println("`-- eeee")
          ParseOk(args, state, ValueParser(None))
      }
      case ApParser(f, a) =>
         println("`-- xfxfffff (ap)")
        ({ println("left"); stepArg(f, args, state).map(r => a <*> r) }) <+> ({ println("right"); stepArg(a, args, state).map(r => r <*> f) })
      case AltParser(a, b) =>
         println("`-- ggggg (alt)")
        stepArg(a, args, state) <+> stepArg(b, args, state)
      case BindParser(f, a) =>
         println("`-- hhhhh (bind)")
        stepArg(a, args, state).map(aa => aa.eval match {
          case None => ValueParser(None)
          case Some(aaa) => f(aaa)
        })
    }
  }

  def step[A](parser: ParserX[A], arg: String, args: List[String], state: ParseState): ParseResult[ParserX[A]] = {
    println("running step on : " + parser)
    parser match {
      case ValueParser(_) =>
          println("+-- aaaa")
        ParseOk(arg :: args, state, parser)
      case PiratedParser(p, m) => p match {
        case FlagParser(flag, a) =>
          println("+-- bbbbb")
          if (flag.a.exists(x => s"-$x" == arg) || flag.b.exists(x => s"--$x" == arg))
            ParseOk(args, state, ValueParser(Some(a)))
          else
            ParseOk(args, state, ValueParser(None))
        case OptionParser(flag, meta, o) =>
          println("+-- cccccc")
          if (flag.a.exists(x => s"-$x" == arg) || flag.b.exists(x => s"--$x" == arg))
              o.parse(args).disjunction match {
                case -\/(s) => ParseFail(Bomb(s))
                case \/-((rest, a)) => ParseOk(rest, state, a.pure[ParserX])
              }
          else
            ParseOk(args, state, ValueParser(None))
        case ArgumentParser(o) =>
          println("+-- ddddd")
          ParseOk(args, state, ValueParser(None))
        case SubCommandParser(name, p) =>
          println("+-- eeeee")
          if (arg == name)
            run(p, args, state) match {
              case ParseFail(e) =>
                ParseFail(e)
              case ParseOk(_ :: _, _, _) =>
                ParseFail(Bomb("too many args for sub command " + name))
              case ParseOk(Nil, s, a) =>
                ParseOk(Nil, s, ValueParser(Some(a)))
            }
          else
            ParseOk(args, state, ValueParser(None))
      }
      case ApParser(f, a) =>
        step(f, arg, args, state).map(a <*> _)
      case AltParser(a, b) =>
        println("+-- ggggg (alt)")
        step(a, arg, args, state) <+> step(b, arg, args, state)
      case BindParser(f, a) =>
        println("+-- hhhhhh (bind)")
        step(a, arg, args, state).map(aa => aa.eval match {
          case None => ValueParser(None)
          case Some(aaa) => f(aaa)
        })

    }
  }


  def complete[A](parser: ParserX[A], args: List[String], state: ParseState): ParseResult[A] =
    search(parser, args, state) match {
      case ParseFail(e) =>
        ParseFail(e)
      case zz @ ParseOk(argsx, statex, value) => { println("`--" + zz); value match {
        case ValueParser(None) => ParseFail(Bomb("noneasfasf"))
        case ValueParser(Some(a)) => ParseOk(argsx, statex, a)
        case _ => complete(value, argsx, statex)
      } }
    }


  def search[A](parser: ParserX[A], args: List[String], state: ParseState): ParseResult[ParserX[A]] =
    parser match {
      case ValueParser(m) =>
        println("search hit a value parser")
          ParseOk(args, state, ValueParser(None))
      case PiratedParser(p, m) =>  p match {
        case FlagParser(flag, a) =>
          val (before, after) = args.span(arg => !(flag.a.exists(x => s"-$x" == arg) || flag.b.exists(x => s"--$x" == arg) || arg == "--"))
          after.headOption match {
            case Some("--") =>
              ParseOk(args, state, ValueParser(None))
            case Some(f) if flag.a.exists(x => f == s"-$x") || flag.b.exists(x => f == s"--$x") =>
              ParseOk(before ::: after.tail, state, a.pure[ParserX])
            case None =>
              ParseOk(args, state, ValueParser(None))
          }
        case OptionParser(flag, meta, o) =>
          val (before, after) = args.span(arg => !(flag.a.exists(x => s"-$x" == arg) || flag.b.exists(x => s"--$x" == arg) || arg == "--"))
          after.headOption match {
            case Some("--") =>
              ParseOk(args, state, ValueParser(None))
            case Some(f) if flag.a.exists(x => f == s"-$x") || flag.b.exists(x => f == s"--$x") =>
              o.parse(after.tail).disjunction match {
                case -\/(s) => ParseOk(args, state, ValueParser(None))
                case \/-((rest, a)) => ParseOk(before ::: rest, state, a.pure[ParserX])
              }
            case None =>
              ParseOk(args, state, ValueParser(None))
          }
        case ArgumentParser(o) =>
          o.parse(args).disjunction match {
            case -\/(s) => ParseOk(args, state, ValueParser(None))
            case \/-((rest, a)) => ParseOk(rest, state, a.pure[ParserX])
          }
        case SubCommandParser(name, p) =>
          ???
      }
      case ApParser(f, a) =>
        println("ApParser")
        search(f, args, state).map(r => a <*> r) <+> search(a, args, state).map(r => r <*> f)
      case AltParser(a, b) =>
        println("AltParser: ")
        search(a, args, state) <+> search(b, args, state)
      case BindParser(f, a) =>
        println("BindParser")
        search(a, args, state).map(aa => aa.eval match {
          case None => ValueParser(None)
          case Some(aaa) => f(aaa)
        })
    }

/*
  def run[A](parser: ParserX[A], args: List[String], state: ParseState): ParseError \/ ParseOk[A] =
    parser match {
      case ValueParser(m) => m match {
        case None =>
          Bomb("No value").left
        case Some(a) =>
          ParseOk(args, state, a).right
      }
      case PiratedParser(p, m) =>  p match {
        case FlagParser(flag, a) =>
          ???
        case OptionParser(flag, meta, o) =>
          ???
        case ArgumentParser(o) =>
          ???
        case SubCommandParser(name, p) =>
          ???
      }
      case ApParser(f, a) =>
        for { aa <- run(a, args, state); ff <- run(f, aa.args, aa.state) } yield ParseOk(ff.args, ff.state, ff.value(aa.value))
      case AltParser(a, b) =>
        run(a, args, state) <+> run(parser, args, state)
      case BindParser(f, a) =>
        for { aa <- run(a, args, state); bb <- run(f(aa.value), aa.args, aa.state) } yield bb
    }
*/
}



sealed trait ParseState
case object AcceptOptions extends ParseState
case object ExcludeOptions extends ParseState
