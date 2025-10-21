package decline_derive

import com.monovore.decline.*
import scala.deriving.*
import scala.quoted.*

trait CommandApplication[T]:
  def command: Command[T]
  def subcommands: List[Command[T]]

object CommandApplication:

  trait Main[T: CommandApplication]:
    def run(args: T): Unit

    final def main(args: Array[String]): Unit  = 
      run(CommandApplication.parseOrExit[T](args, sys.env))


  inline def derived[T](using Mirror.Of[T]): CommandApplication[T] =
    ${ Macros.derivedMacro[T] }

  /** Parse the command line arguments and the environment variables. Note that
    * by default `sys.env` is used as default value for @env parameter
    */
  inline def parse[T: CommandApplication](
      args: Seq[String],
      env: Map[String, String] = sys.env
  ): Either[Help, T] =
    summon[CommandApplication[T]].command.parse(args, env)

  /** Parse the command line arguments and the environment variables, but exit
    * the program if either `--help` flag is passed (exit code 0, help printed
    * to stderr), or an error was encountered (exit code -1). Note that by
    * default `sys.env` is used as default value for @env parameter
    */
  inline def parseOrExit[T: CommandApplication](
      args: Seq[String],
      env: Map[String, String] = sys.env,
      printHelp: Boolean = true
  ): T =
    summon[CommandApplication[T]].command.parse(args, env) match
      case Left(value) =>
        if printHelp then System.err.println(value)
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
