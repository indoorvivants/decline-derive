package decline_derive

import quoted.*

private[decline_derive] case class ArgHintProvider(e: Expr[Seq[ArgHint]]):
  inline def getHint[T: Type](
      inline f: PartialFunction[ArgHint, T]
  )(using Quotes): Expr[Option[T]] =
    '{ $e.collectFirst(f) }

  end getHint

  def name(using Quotes) =
    getHint:
      case ArgHint.Name(value) => value

  def flag(using Quotes) =
    getHint:
      case ArgHint.FlagDefault(value) => value

  def short(using Quotes) =
    getHint:
      case ArgHint.Short(value) => value

  def help(using Quotes) =
    getHint:
      case ArgHint.Help(value) => value

  def isArgument(using Quotes) =
    getHint:
      case ArgHint.Argument(metavar: String) => Some(metavar)
      case ArgHint.Argument(None)            => None
      case ArgHint.Argument(Some(metavar))   => Some(metavar)

end ArgHintProvider
