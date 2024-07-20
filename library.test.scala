import munit.FunSuite

import decline_derive.*
import util.chaining.*

class Tests extends FunSuite:
  test("simple parameters"):
    case class Cmd(location: String, test: Boolean, y: Int)
        derives CommandApplication

    assertArgs[Cmd](Cmd("howdy", true, 25))(
      "--location",
      "howdy",
      "--test",
      "--y",
      "25"
    )

    assertArgs[Cmd](Cmd("yass", false, -150))(
      "--location",
      "yass",
      "--y",
      "-150"
    )

  test("optional parameters"):
    case class Cmd(location: Option[String], y: Option[Int])
        derives CommandApplication

    assertArgs[Cmd](Cmd(None, None))()

    assertArgs[Cmd](Cmd(Some("yess"), Some(150)))(
      "--location",
      "yess",
      "--y",
      "150"
    )

  test("repeated parameters"):
    case class Cmd(locations: List[String]) derives CommandApplication

    assertArgs[Cmd](Cmd(List("yess", "yo")))(
      "--locations",
      "yess",
      "--locations",
      "yo"
    )

  test("optional repeated parameters"):
    case class Cmd(locations: Option[List[String]]) derives CommandApplication

    assertArgs[Cmd](Cmd(Some(List("yess", "yo"))))(
      "--locations",
      "yess",
      "--locations",
      "yo"
    )

    assertArgs[Cmd](Cmd(None))()

  test("argument hints: name"):
    case class Cmd(
        @arg(_.Name("yepp")) location: Option[String],
        @arg(_.Name("flag2")) flag: Boolean
    ) derives CommandApplication

    assertArgs[Cmd](Cmd(Some("shroom"), false))("--yepp", "shroom")
    assertArgs[Cmd](Cmd(Some("shroom"), true))("--yepp", "shroom", "--flag2")

  test("argument hints: short"):
    case class Cmd(@arg(_.Short("y")) location: Option[String])
        derives CommandApplication

    assertArgs[Cmd](Cmd(Some("shroom")))("-y", "shroom")

  test("argument hints: flag default"):
    case class Cmd(@arg(_.FlagDefault(true)) isLit: Boolean)
        derives CommandApplication

    assertArgs[Cmd](Cmd(false))("--isLit")

  test("argument hints: positional"):
    case class Cmd(
        location: String,
        @arg(_.Positional("metavar")) isLit: String
    ) derives CommandApplication

    assertArgs[Cmd](Cmd("hello", "yes"))("--location", "hello", "yes")

  test("argument hints: positional (repeated)"):
    case class Cmd(
        location: String,
        @arg(_.Positional("metavar"))
        isLit: List[String]
    ) derives CommandApplication

    assertArgs[Cmd](Cmd("hello", List("yes", "bla", "test")))(
      "--location",
      "hello",
      "yes",
      "bla",
      "test"
    )

  test("subcommands: basic"):
    enum Cmd derives CommandApplication:
      case Index(location: String)
      case Evaluate(file: String, strict: Boolean)

    assertArgs(Cmd.Index("hello.trig"))("index", "--location", "hello.trig")
    assertArgs(Cmd.Evaluate("hello.trig", true))(
      "evaluate",
      "--file",
      "hello.trig",
      "--strict"
    )

    assertErr[Cmd]("unknown")
    assertErr[Cmd]()

  test("subcommands: nested"):
    enum IndexCommand derives CommandApplication:
      case Workspace(location: String)
      case Files(
          @arg(_.Positional())
          locations: List[String]
      )
    end IndexCommand

    enum EvaluateCommand derives CommandApplication:
      case Simple(file: String, strict: Boolean)
      case More(
          @arg(_.FlagDefault(false))
          yes: Boolean
      )
    end EvaluateCommand

    enum Cmd derives CommandApplication:
      case Index(commands: IndexCommand)
      case Evaluate(commands: EvaluateCommand, test: Option[String])

    assertArgs(Cmd.Index(IndexCommand.Workspace("hello.trig")))(
      "index",
      "workspace",
      "--location",
      "hello.trig"
    )
    assertArgs(
      Cmd.Index(IndexCommand.Files(List("hello.trig1", "hello.trig2")))
    )(
      "index",
      "files",
      "hello.trig1",
      "hello.trig2"
    )

    assertArgs(Cmd.Evaluate(EvaluateCommand.Simple("hello.trig", true), None))(
      "evaluate",
      "simple",
      "--file",
      "hello.trig",
      "--strict"
    )
    assertArgs(Cmd.Evaluate(EvaluateCommand.More(false), Some("25")))(
      "evaluate",
      "--test",
      "25",
      "more",
      "--yes"
    )

    assertErr[Cmd]("unknown")
    assertErr[Cmd]()

  test("subcommands: name hints"):
    enum Cmd derives CommandApplication:
      @cmd(_.Name("index-file")) case Index(location: String)
      @cmd(_.Name("evaluate-all")) case Evaluate(file: String, strict: Boolean)

    assertArgs(Cmd.Index("hello.trig"))(
      "index-file",
      "--location",
      "hello.trig"
    )
    assertArgs(Cmd.Evaluate("hello.trig", true))(
      "evaluate-all",
      "--file",
      "hello.trig",
      "--strict"
    )

  private def assertArgs[T: CommandApplication](res: T)(args: String*) =
    assertEquals(CommandApplication.parse[T](args), Right(res))

  private def assertErr[T: CommandApplication](args: String*) =
    val newValue = CommandApplication.parse[T](args)
    assert(newValue.isLeft, newValue)

  private def printHelp[T: CommandApplication](args: String*) =
    val Left(help) = CommandApplication.parse[T](args): @unchecked

    println(help)
  end printHelp
end Tests
