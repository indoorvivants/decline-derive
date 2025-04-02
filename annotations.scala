package decline_derive

import scala.annotation.ConstantAnnotation

private trait DeclineDeriveAnnotation

private[decline_derive] class Debug()
    extends ConstantAnnotation,
      DeclineDeriveAnnotation

/** Set name of command, subcommand, or argument */
class Name(val value: String)
    extends ConstantAnnotation,
      DeclineDeriveAnnotation

/** Set short name of argument */
class Short(val value: String)
    extends ConstantAnnotation,
      DeclineDeriveAnnotation

/** Set help message of command, subcommand, or argument */
class Help(val value: String)
    extends ConstantAnnotation,
      DeclineDeriveAnnotation

/** Make the argument positional instead of an option passed with -- or - */
class Positional(val metavar: String)
    extends ConstantAnnotation,
      DeclineDeriveAnnotation

/** Environment variable used as fallback if this argument is not provided */
class Env(val name: String, val help: String)
    extends ConstantAnnotation,
      DeclineDeriveAnnotation

/** Explicitly set the default value of a flag argument. Note that boolean
  * parameters are automatically turned into flags, so this annotations is only
  * necessary if you want to flip the default value and the action of the flag.
  */
class Flag(val default: Boolean)
    extends ConstantAnnotation,
      DeclineDeriveAnnotation
