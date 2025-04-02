package decline_derive

import scala.annotation.ConstantAnnotation

private trait DeclineDeriveAnnotation

private[decline_derive] class Debug()
    extends ConstantAnnotation,
      DeclineDeriveAnnotation

class Name(val value: String)
    extends ConstantAnnotation,
      DeclineDeriveAnnotation

class Short(val value: String)
    extends ConstantAnnotation,
      DeclineDeriveAnnotation

class Help(val value: String)
    extends ConstantAnnotation,
      DeclineDeriveAnnotation

class Positional(val metavar: String)
    extends ConstantAnnotation,
      DeclineDeriveAnnotation

class Env(val name: String, val help: String)
    extends ConstantAnnotation,
      DeclineDeriveAnnotation

class Flag(val default: Boolean)
    extends ConstantAnnotation,
      DeclineDeriveAnnotation
