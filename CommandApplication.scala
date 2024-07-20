package decline_derive

import com.monovore.decline.*
import scala.deriving.*
import scala.quoted.*

trait CommandApplication[T]:
  val opt: Command[T]
  private[decline_derive] val subcommands: List[Command[T]]

object CommandApplication:
  inline def derived[T](using Mirror.Of[T]): CommandApplication[T] =
    ${ Macros.derivedMacro[T] }

  inline def parse[T: CommandApplication](
      args: Seq[String],
      env: Map[String, String] = Map.empty
  ): Either[Help, T] =
    summon[CommandApplication[T]].opt.parse(args, env)
end CommandApplication
