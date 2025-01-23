package decline_derive

import com.monovore.decline.*
import scala.deriving.*
import scala.quoted.*

trait CommandApplication[T]:
  def command: Command[T]
  def subcommands: List[Command[T]]

object CommandApplication:

  inline def derived[T](using Mirror.Of[T]): CommandApplication[T] =
    ${ Macros.derivedMacro[T] }

  inline def parse[T: CommandApplication](
      args: Seq[String],
      env: Map[String, String] = sys.env
  ): Either[Help, T] =
    summon[CommandApplication[T]].command.parse(args, env)

  inline def parseOrExit[T: CommandApplication](
      args: Seq[String],
      env: Map[String, String] = sys.env,
      printHelp: Boolean = true
  ): T =
    summon[CommandApplication[T]].command.parse(args, env) match
      case Left(value) =>
        if printHelp then println(value)
        if value.errors.nonEmpty then sys.exit(-1) else sys.exit(0)
      case Right(value) =>
        value
    end match
  end parseOrExit

  class Impl[T](
      val opt: Command[T],
      val sub: List[Command[T]]
  ) extends CommandApplication[T]:
    override def command: Command[T] = opt
    override def subcommands: List[Command[T]] = this.sub
  end Impl
end CommandApplication
