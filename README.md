NAME
        pirate - command line 'arrrrg processor

DESCRIPTION
        Pirate is a functional scala library that provides a mechanism for
        parsing command line arguments and producing usage strings.

        Pirate defines a set of combinators for constructing commands from
        a set of flags and positional parameters. Each component has an
        attached function that is executed when an argument is parsed to
        transform a default argument object into its final form.

        Pirate also provides a number of utilities such as a standard
        dispatch mechanism for parsing arguments and executing a program
        based upon the result.

USAGE
        To get started, import the pirate package:

            import io.mth.pirate._

        This will expose the required type constructors.

        Define an argument object, could be anything from
        a simple map or list through to a case class (which
        is recommended).

          case class MyArgs(flag: Boolean, params: List[String])

        Construct a command line, combining in flags and
        positional parameters.

          val cmd = command[MyArgs]("myprogram") <|>
              flag('f', "flag", "enable flag.")(_.copy(flag = true)) >|
              positional0plus("THINGS")((myargs, list) => myargs.copy(params = list))

        Dispatch on that command line for default behaviour.

          val exitcode = cmd.dispatch(args, MyArgs(false, List()) { myargs =>
            // run program with parsed myargs.
          }
          exit(exitcode)

        Consult the api and demos for more advanced/complete
        documentation.

STATE
        The `pirate` library is currently in an incubation stage and is
        considered experimental and subject to breaking change -- the
        plan is to evolve the api until the following limitations have
        been addressed.

LIMITATIONS
        The current version is very usable, however it has some limitations
        on more advanced command line options:
          1. multi-modal commands (allowing things like help only or ...)
          2. git style sub commands using different argument types
          3. more (configurable) flag syntax e.g. --flag=value
          4. better error detection / constraints for flag combinations


AUTHOR
        Mark Hibberd <mark@hibberd.id.au>
        Karl Roberts <karl.roberts@owtelse.com>

NOTES
        1. official repository
           https://github.com/markhibberd/pirate
        2. site and documentation
           http://pirate.mth.io
        3. license (3 point BSD style)
           https://github.com/markhibberd/pirate/blob/master/LICENSE

