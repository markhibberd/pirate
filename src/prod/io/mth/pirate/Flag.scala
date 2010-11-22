package io.mth.pirate

/**
 * A data type representing argument values.
 */
trait Flag[A] {
  /**
   * The catamorphism for the Flag data type.
   */
  def fold[B](
    flag0: Option[Char] => Option[String] => String => (A => A) => B,
    flag1: Option[Char] => Option[String] => String => String => (String => A => A) => B,
    positional1: String => (String => A => A) => B,
    positionalN: String => (List[String] => A => A) => B,
    flags: List[Flag[A]] => B
  ): B

  def isFlag0 = fold(
      s => l => d => f => true,
      s => l => d => m => f => false,
      m => f => false,
      m => f => false,
      f => false
    )

  def isFlag1 = fold(
      s => l => d => f => false,
      s => l => d => m => f => true,
      m => f => false,
      m => f => false,
      f => false
    )
  
  def isPositional1 = fold(
      s => l => d => f => false,
      s => l => d => m => f => false,
      m => f => true,
      m => f => false,
      f => false
    )

  def isPositionalN = fold(
      s => l => d => f => false,
      s => l => d => m => f => false,
      m => f => false,
      m => f => true,
      f => false
    )


  def isFlags = fold(
      s => l => d => f => false,
      s => l => d => m => f => false,
      m => f => false,
      m => f => false,
      f => true
    )

  def allFlags: List[Flag[A]] =
    fold(
      s => l => d => f => List(this),
      s => l => d => m => f => List(this),
      m => f => List(),
      m => f => List(),
      f => f.flatMap(_.allFlags)
    )

  def allPositional: List[Flag[A]] =
    fold(
      s => l => d => f => List(),
      s => l => d => m => f => List(),
      m => f => List(this),
      m => f => List(this),
      f => f.flatMap(_.allPositional)
    )

  def description: String =
    fold(
      s => l => d => f => d,
      s => l => d => m => f => d,
      m => f => "",
      m => f => "",
      f => ""
    )

  def flaguse: String =
    fold(
      s => l => d => f => s.map("-" + _).getOrElse("") + (if (s.isDefined && l.isDefined) "," else "") + l.getOrElse(""),
      s => l => d => m => f => s.map("-" + _).getOrElse("") + (if (s.isDefined && l.isDefined) "," else "") + l.getOrElse("") + "=" + m ,
      m => f => "",
      m => f => "",
      f => ""
    )

  def flagsynopsis: String =
    fold(
      s => l => d => f => "[" + s.map("-" + _).getOrElse("") + (if (s.isDefined && l.isDefined) "|" else "") + l.getOrElse("") + "]",
      s => l => d => m => f => "[" + s.map("-" + _).getOrElse("") + (if (s.isDefined && l.isDefined) "|" else "") + l.getOrElse("") + " " + m + "]",
      m => f => m,
      m => f => "[" + m + " ...]",
      f => ""
    )

  def all = allFlags ++ allPositional

  def bind(f: Flag[A]) = Flag.flags(List(this, f))

  def >>=(f: Flag[A]) = bind(f)

  def toParser: Parser[A => A] = fold(
        s => l => d => f => FlagParsers.flag0(s, l).lift(_ => f),
        s => l => d => m => f =>  FlagParsers.flag1(s, l).lift(v => f(v)(_)),
        m => f => FlagParsers.positional1.lift(v => f(v)(_)),
        m => f => FlagParsers.positionalN.lift(v => f(v)(_)),
        fs => FlagParsers.flatCommandline(allFlags.map(_.toParser), allPositional.map(_.toParser))
      )


  // FIX Sort out how the union types do usage/bind.
  def usage: String = fold(
      s => l => d => f => s.map("-" + _).getOrElse("") + (if (s.isDefined && l.isDefined) "," else "") + l.getOrElse("") + "\t\t" + d,
      s => l => d => m => f => s.map("-" + _).getOrElse("") + (if (s.isDefined && l.isDefined) "," else "") + l.getOrElse("") + "=" + m +"\t\t" + d,
      m => f => m,
      m => f => "[" + m + " ...]",
      f => "[not implemented]"
  )
}

object Flag {
  def short[A](short: Char, description: String)(f: A => A) = 
    flag0(Some(short), None, description, f)

  def long[A](long: String, description: String)(f: A => A) =
    flag0(None, Some(long), description, f)

  def full[A](short: Char, long: String, description: String)(f: A => A) = 
    flag0(Some(short), Some(long), description, f)

  def short1[A](short: Char, description: String, meta: String)(f: String => A => A) = 
    flag1(Some(short), None, description, meta, f)

  def long1[A](long: String, description: String, meta: String)(f: String => A => A) =
    flag1(None, Some(long), description, meta, f)

  def full1[A](short: Char, long: String, description: String, meta: String)(f: String => A => A) = 
    flag1(Some(short), Some(long), description, meta, f)

  def positional1[A](meta: String)(f: String => A => A) =
    positional1x(meta, f)

  def positionalN[A](meta: String)(f: List[String] => A => A) =
    positionalNx(meta, f)

  private def flag0[A](s: Option[Char], l: Option[String], d: String, f: A => A) = new Flag[A] {
    def fold[B](
       flag0: Option[Char] => Option[String] => String => (A => A) => B,
       flag1: Option[Char] => Option[String] => String => String => (String => A => A) => B,
       positional1: String => (String => A => A) => B,
       positionalN: String => (List[String] => A => A) => B,
       flags: List[Flag[A]] => B
    ): B = flag0(s)(l)(d)(f)
  }

  private def flag1[A](s: Option[Char], l: Option[String], d: String, m: String, f: String => A => A) = new Flag[A] {
    def fold[B](
       flag0: Option[Char] => Option[String] => String => (A => A) => B,
       flag1: Option[Char] => Option[String] => String => String => (String => A => A) => B,
       positional1: String => (String => A => A) => B,
       positionalN: String => (List[String] => A => A) => B,
       flags: List[Flag[A]] => B
    ): B = flag1(s)(l)(d)(m)(f)
  }

  private def positional1x[A](m: String, f: String => A => A) = new Flag[A] {
    def fold[B](
       flag0: Option[Char] => Option[String] => String => (A => A) => B,
       flag1: Option[Char] => Option[String] => String => String => (String => A => A) => B,
       positional1: String => (String => A => A) => B,
       positionalN: String => (List[String] => A => A) => B,
       flags: List[Flag[A]] => B
    ): B = positional1(m)(f)
  }

  private def positionalNx[A](m: String, f: List[String] => A => A) = new Flag[A] {
    def fold[B](
       flag0: Option[Char] => Option[String] => String => (A => A) => B,
       flag1: Option[Char] => Option[String] => String => String => (String => A => A) => B,
       positional1: String => (String => A => A) => B,
       positionalN: String => (List[String] => A => A) => B,
       flags: List[Flag[A]] => B
    ): B = positionalN(m)(f)
  }


  private def flags[A](fs: List[Flag[A]]) = new Flag[A] {
    def fold[B](
       flag0: Option[Char] => Option[String] => String => (A => A) => B,
       flag1: Option[Char] => Option[String] => String => String => (String => A => A) => B,
       positional1: String => (String => A => A) => B,
       positionalN: String => (List[String] => A => A) => B,
       flags: List[Flag[A]] => B
    ): B = flags(fs)
  }
}

