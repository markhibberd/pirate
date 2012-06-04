package io.mth

import pirate.{Flags, Command, Flag, Positional}

/**
 * Pirate argument parser.
 *
 * This package object exports everything required for normal usage.
 */
package object pirate {
  def command[A] = Command.command[A] _

  def short[A] = Flag.short[A] _
  def long[A] = Flag.long[A] _
  def flag[A] = Flag.flag[A] _
  def short1[A] = Flag.short1[A] _
  def long1[A] = Flag.long1[A] _
  def flag1[A] = Flag.flag1[A] _

  def positional[A] = Positional.positional[A] _
  def positionalN[A] = Positional.positionalN[A] _
  def positional0plus[A] = Positional.positional0plus[A] _
  def positional1plus[A] = Positional.positional1plus[A] _
}