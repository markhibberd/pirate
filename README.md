Pirate
======

> command line 'arrrrg processor

[![Build Status](https://travis-ci.org/markhibberd/pirate.png)](https://travis-ci.org/markhibberd/pirate)

Description
-----------

Pirate is a functional Scala library that provides a mechanism for
parsing command line arguments and producing usage strings.

Pirate defines a set of applicative combinators for constructing
commands from a set of flags and positional parameters in a natural
manner. Each component has an attached function that is executed
as arguments are processed, to transform it into its final form.
Flags, switches, positionals, and subcommands are all easy to create
and combine for simple and complex programs alike.

Pirate also provides a number of utilities such as a standard
dispatch mechanism for parsing arguments and executing a program
based upon the result.

Usage
-----

To get started, import the pirate package:

    import pirate._

This will expose the required type constructors, extra helper functions
are available with

    import Pirate._

Define an argument object, which could be anything from a simple map,
tuple, or list through to a case class (which is recommended).

```scala
case class MyArgs(flag: Boolean, author: Option[String], delim: String, dryRun: Boolean, path: String)
```

Construct a command line, combining in flags and positional parameters.

```scala
val cmd = (MyArgs |*| (
    switch(both('f',"flag"), description("enable flag."))
  , flag[String](long("author"), metavar("<pattern>")).option
  , flag[String](long("delim"), metavar("[|]")).default("|")
  , switch(long("dry-run"), empty)
  , argument[String](metavar("<path>"))
  )) ~ "myprogram" ~~ "My description"
```

Extend `PirateMain` or `PirateMainIO` to use:

```scala
object MyApp extends PirateMain[MyArgs] {

  def command = cmd

  def run(args: MyArgs): Unit = ???
}
```

Or run directly:

```scala
Runners.runOrFail(args.toList, cmd).map {
  args => ???
}
````

When run with incorrect parameters, a custom help text will be generated, e.g.,

```
Usage:
  myprogram [(-f|--flag)] [--author <pattern>] [--delim [|]] [--dry-run] <path>

My description

Available options:
  -f|--flag               enable flag.
  --dry-run
  --author <pattern>
  --delim [|]

Positional arguments:
  <path>
```

Consult the api and demos for more advanced/complete documentation.

State
-----

The `pirate` library is currently very usable and in use by the
engineering team at Ambiata, who provide builds in an Ivy repo via
https://ambiata-oss.s3.amazonaws.com

The API however, is not yet frozen and is still being refined, and
may therefore be subject to breaking changes future. In particular,
we would like to allow user definable parser configurations for how
and when the usage text is displayed, and other parser properties
such as backtracking are used.  Further refinement of the usage
texts is also expected.

Authors
-------

- Mark Hibberd <mark@hibberd.id.au>
- Karl Roberts <karl.roberts@owtelse.com>
- Charles O'Farrell <@charlesofarrell>
- Huw Campbell

Much of Pirate is inspired by optparse-applicative by Paolo Capriotti.

Notes
-----

1. Official repository
   https://github.com/markhibberd/pirate
2. Site and documentation
   http://pirate.mth.io
3. License (3 point BSD style)
   https://github.com/markhibberd/pirate/blob/master/LICENSE
