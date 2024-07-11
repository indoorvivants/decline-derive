package decline_derive

import quoted.*

private[decline_derive] case class CmdHintProvider(e: Expr[Seq[CmdHint]]):
  inline def getHint[T: Type](
      inline f: PartialFunction[CmdHint, T]
  )(using Quotes): Expr[Option[T]] =
    '{ $e.collectFirst(f) }

  end getHint

  def name(using Quotes) =
    getHint:
      case CmdHint.Name(value) => value

  def help(using Quotes) =
    getHint:
      case CmdHint.Help(value) => value

end CmdHintProvider
