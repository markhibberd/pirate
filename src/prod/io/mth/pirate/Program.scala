package io.mth.pirate

import scalaz._

sealed trait Program[A] {
  def fold[X](
      command: Command[A] => X,
      description: Command[A] => String => X,
      multiforms: List[Command[A]] => String => X,
      multimodes: Command[A] => List[Command[A]] => X
  ): X

  def usage = Usage.usage(this)

  def parse(args: List[String], a: A) = fold(
      _.parse(args, a),
      c => d => c.parse(args, a),
      cs => error("todo"),
      p => s => error("todo")
    )

  def parseArgs(args: Array[String], a: A) = parse(args.toList, a)
}

object Program {
  def program[A](c: Command[A]): Program[A] = new Program[A] {
    def fold[X](
      command: Command[A] => X,
      description: Command[A] => String => X,
      multiforms: List[Command[A]] => String => X,
      multimodes: Command[A] => List[Command[A]] => X
    ): X = command(c)
  }

  def programWithDescription[A](c: Command[A], d: String): Program[A] = new Program[A] {
    def fold[X](
      command: Command[A] => X,
      description: Command[A] => String => X,
      multiforms: List[Command[A]] => String => X,
      multimodes: Command[A] => List[Command[A]] => X
    ): X = description(c)(d)
  }
}