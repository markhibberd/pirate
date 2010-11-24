package io.mth

import pirate.{Command, Flag, Positional}

package object pirate {
  def command[A] = Command.command[A] _

  def short[A] = Flag.short[A] _
  def long[A] = Flag.long[A] _
  def full[A] = Flag.full[A] _
  def short1[A] = Flag.short1[A] _
  def long1[A] = Flag.long1[A] _
  def full1[A] = Flag.full1[A] _

  def positional[A] = Positional.positional[A] _
  def positionalN[A] = Positional.positionalN[A] _
  def positional0plus[A] = Positional.positional0plus[A] _
  def positional1plus[A] = Positional.positional1plus[A] _
}