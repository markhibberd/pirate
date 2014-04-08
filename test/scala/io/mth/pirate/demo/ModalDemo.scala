package io.mth.pirate.demo

import io.mth.pirate._

object ModalDemo {

  sealed trait Mode
  case class WinMode(flag: String) extends Mode
  case class LoseMode(flag: String) extends Mode
  case class HelpMode(mode: String) extends Mode

  val win =
    command[WinMode]("win") <|>
      flag1('f', "flag", "set value.", "VALUE")((x, s) => x.copy(flag = s))

  val lose =
    command[LoseMode]("lose") <|>
      flag1('f', "flag", "set value.", "VALUE")((x, s) => x.copy(flag = s))

  val help =
    command[HelpMode]("help") >|
      positional[HelpMode]("MODE")((x, s) => x.copy(mode = s))

  val demo =
    commands[Mode]("demo")

  def main(ignored: Array[String]) {


  }
}
