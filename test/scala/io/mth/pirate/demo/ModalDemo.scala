package io.mth.pirate.demo

import io.mth.pirate._

object ModalDemo {

  sealed trait Mode
  case class WinMode(flag: Option[String]) extends Mode
  case class LoseMode(flag: Option[String]) extends Mode
  case class HelpMode(mode: Option[String]) extends Mode

  def win =
    command[WinMode]("win") <|>
      flag1('f', "flag", "set value.", "VALUE")((x, s) => x.copy(flag = Some(s)))

  def lose =
    command[LoseMode]("lose") <|>
      flag1('f', "flag", "set value.", "VALUE")((x, s) => x.copy(flag = Some(s)))

  def help =
    command[HelpMode]("help") >|
      positional[HelpMode]("MODE")((x, s) => x.copy(mode = Some(s)))

  def demo =
    commands[Mode]("demo")
      .withSubtypeCommand(WinMode(None), win)
      .withSubtypeCommand(LoseMode(None), lose)
      .withSubtypeCommand(HelpMode(None), help)

  def main(ignored: Array[String]) {
    val args = List("lose", "-f", "not")

    demo.dispatchOrDie(args) {
      case WinMode(flag) => println("win: " + flag)
      case LoseMode(flag) => println("lose: " + flag)
      case HelpMode(flag) => println("help: " + flag)
    }
  }
}
