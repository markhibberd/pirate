package pirate.internal

sealed trait ParseState
case object SkipOpts extends ParseState
case object AllowOpts extends ParseState
