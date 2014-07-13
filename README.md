Pirate
======

> command line 'arrrrg processor

[![Build Status](https://travis-ci.org/markhibberd/pirate.png)](https://travis-ci.org/markhibberd/pirate)

Description
-----------

Pirate is a functional scala library that provides a mechanism for
parsing command line arguments and producing usage strings.

Pirate defines a set of combinators for constructing commands from
a set of flags and positional parameters. Each component has an
attached function that is executed when an argument is parsed to
transform a default argument object into its final form.

Pirate also provides a number of utilities such as a standard
dispatch mechanism for parsing arguments and executing a program
based upon the result.

Usage
-----

To get started, import the pirate package:

    import io.mth.pirate._

This will expose the required type constructors.

Define an argument object, could be anything from
a simple map or list through to a case class (which
is recommended).

```scala
case class MyArgs(flag: Boolean, author: Option[String], delim: String, dryRun: Boolean, path: String)
```

Construct a command line, combining in flags and positional parameters.

```scala
val cmd = (MyArgs |*| (
    option[Boolean]('f' -> "flag", "enable flag."),
    [String]("author", "<pattern>").option,
    option[String]("delim", "[|]").default("|"),
    switch("dry-run"),
    positional.one[String]("<path>")
  )) ~ "myprogram" ~~ "My description"
```

Extend `PirateMain` or `PirateMainIO` to use:

```scala
class MyApp extends PirateMain[MyArgs] {

  def command = cmd

  def run(args: MyArgs): Unit = ???
}
```

Or run directly:

```scala
Runners.unsafeRunOrFail(args.toList, cmd, args => ???)
````

Consult the api and demos for more advanced/complete documentation.

State
-----

The `pirate` library is currently in an incubation stage and is
considered experimental and subject to breaking change -- the
plan is to evolve the api until the following limitations have
been addressed.

Limitations
-----------

The current version is very usable, however it has some limitations
on more advanced command line options:

1. multi-modal commands (allowing things like help only or ...)
2. git style sub commands using different argument types
3. more (configurable) flag syntax e.g. --flag=value
4. better error detection / constraints for flag combinations


Author
------

- Mark Hibberd <mark@hibberd.id.au>
- Karl Roberts <karl.roberts@owtelse.com>

Notes
-----

1. Official repository
   https://github.com/markhibberd/pirate
2. Site and documentation
   http://pirate.mth.io
3. License (3 point BSD style)
   https://github.com/markhibberd/pirate/blob/master/LICENSE
