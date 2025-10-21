import munit.FunSuite

import decline_derive.*
import util.chaining.*
import com.monovore.decline.Argument

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
        @Name("yepp") location: Option[String],
        @Name("flag2") flag: Boolean
    ) derives CommandApplication

    assertArgs[Cmd](Cmd(Some("shroom"), false))("--yepp", "shroom")
    assertArgs[Cmd](Cmd(Some("shroom"), true))("--yepp", "shroom", "--flag2")

  test("argument hints: env"):
    case class Cmd(
        @Env("TEST_ME", "") location: Option[String],
        @Env("HELLO", "") count: Int
    ) derives CommandApplication

    assertArgs[Cmd](
      Cmd(Some("shroom"), 25),
      Map("TEST_ME" -> "shroom", "HELLO" -> "25")
    )()

    // arguments take priority over env
    assertArgs[Cmd](
      Cmd(Some("yes"), 11),
      Map("TEST_ME" -> "shroom", "HELLO" -> "25")
    )("--location", "yes", "--count", "11")

  test("argument hints: short"):
    case class Cmd(
        @Short("y") location: Option[String],
        @Short("X") flag: Boolean
    ) derives CommandApplication

    assertArgs[Cmd](Cmd(Some("shroom"), true))("-y", "shroom", "-X")

  test("argument hints: flag default"):
    case class Cmd(@Flag(true) isLit: Boolean) derives CommandApplication

    assertArgs[Cmd](Cmd(false))("--isLit")

  test("argument hints: positional"):
    case class Cmd(
        location: String,
        @Positional("metavar") isLit: String
    ) derives CommandApplication

    assertArgs[Cmd](Cmd("hello", "yes"))("--location", "hello", "yes")

  test("argument hints: positional (repeated)"):
    case class Cmd(
        location: String,
        @Positional("metavar")
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
          @Positional("locations")
          locations: List[String]
      )
    end IndexCommand

    enum EvaluateCommand derives CommandApplication:
      case Simple(file: String, strict: Boolean)
      case More(
          @Flag(true)
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
      @Name("index-file") case Index(location: String)
      @Name("evaluate-all") case Evaluate(file: String, strict: Boolean)

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

  test("issue #8: Name label on enum shouldn't propagate to cases"):
    @Name("wut")
    enum Cmd derives CommandApplication:
      case Open
      case Close()

    @Name("wut")
    enum Cmd1 derives CommandApplication:
      @Name("yass") case Open
      @Name("nope") case Close()

    assertArgs(Cmd.Open)(
      "open"
    )

    assertArgs(Cmd.Close())(
      "close"
    )

    assertArgs(Cmd1.Open)(
      "yass"
    )

    assertArgs(Cmd1.Close())(
      "nope"
    )

  test("default values: basic types"):
    case class Cmd(location: String = "default-location", count: Int = 42)
        derives CommandApplication

    // Test with no arguments - should use defaults
    assertArgs(Cmd("default-location", 42))()

    // Test with partial arguments
    assertArgs(Cmd("custom", 42))("--location", "custom")
    assertArgs(Cmd("default-location", 100))("--count", "100")

    // Test with all arguments
    assertArgs(Cmd("custom", 100))(
      "--location",
      "custom",
      "--count",
      "100"
    )

  test("default values: boolean flags"):
    case class Cmd(verbose: Boolean = true, quiet: Boolean = false)
        derives CommandApplication

    // Test with no arguments - should use defaults
    assertArgs(Cmd(true, false))()

    // Test with flags provided - flags toggle the default value
    assertArgs(Cmd(true, true))(
      "--quiet"
    ) // quiet defaults to false, --quiet toggles to true
    assertArgs(Cmd(false, false))(
      "--verbose"
    ) // verbose defaults to true, --verbose toggles to false

  test(
    "default values: warns on boolean flags with annotation and default (and favours default)"
  ):
    case class Cmd(@Flag(false) verbose: Boolean = true)
        derives CommandApplication

    // Test with no arguments - should prefer default parameter value
    assertArgs(Cmd(true))()

    // Test with flag set - should prefer default parameter value
    assertArgs(Cmd(false))("--verbose")

  test("default values: custom Argument types"):
    case class Port(i: Int)
    given Argument[Port] =
      Argument.readInt.map(Port(_))

    case class Cmd(port: Port = Port(25)) derives CommandApplication

    assertNoArgs(Cmd(Port(25)))
    assertArgs(Cmd(Port(500)))("--port", "500")

  test("default values: optional with defaults"):
    case class Cmd(location: Option[String] = Some("default"))
        derives CommandApplication

    // Test with no arguments - should use default
    assertArgs[Cmd](Cmd(Some("default")))()

    // Test with explicit value
    assertArgs[Cmd](Cmd(Some("custom")))("--location", "custom")

  test("default values: mixed with and without defaults"):
    case class Cmd(
        required: String,
        optional: String = "default",
        count: Int = 10
    ) derives CommandApplication

    // Test with only required argument
    assertArgs(Cmd("req", "default", 10))("--required", "req")

    // Test with required and one optional
    assertArgs(Cmd("req", "custom", 10))(
      "--required",
      "req",
      "--optional",
      "custom"
    )

    // Test with all arguments
    assertArgs(Cmd("req", "custom", 25))(
      "--required",
      "req",
      "--optional",
      "custom",
      "--count",
      "25"
    )

  test("case class composition"):
    case class HttpConfig(host: String = "localhost", port: Int = 80)
        derives CommandApplication
    case class Cmd(http: HttpConfig, y: Int = 15) derives CommandApplication

    assertNoArgs(Cmd(HttpConfig()))
    assertArgs(Cmd(HttpConfig("0.0.0.0", 8080), 500))(
      "--host",
      "0.0.0.0",
      "--port",
      "8080",
      "--y",
      "500"
    )

  private def assertArgs[T: CommandApplication](
      res: T,
      env: Map[String, String] = Map.empty
  )(args: String*) =
    assertEquals(CommandApplication.parse[T](args, env), Right(res))

  private def assertNoArgs[T: CommandApplication](
      res: T,
      env: Map[String, String] = Map.empty
  ) =
    assertEquals(CommandApplication.parse[T](Seq.empty, env), Right(res))

  private def assertErr[T: CommandApplication](args: String*) =
    val newValue = CommandApplication.parse[T](args)
    assert(newValue.isLeft, newValue)

end Tests
