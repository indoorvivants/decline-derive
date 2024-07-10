package decline_derive

import ArgHint.*

@cmd(CmdHint.Name("my-cli"))
enum MyCLI derives CommandApplication:
  case Index(
      @arg(
        Name("index"),
        Short("l"),
        Help("location of index file")
      ) location: String,
      language: Option[String]
  )

  @cmd(CmdHint.Name("search")) case Search(location: String, repl: Boolean)

  @cmd(CmdHint.Name("test-me"), CmdHint.Help("this tests stuff")) //
  case Test(
      bla: Int
  )

case class Stuff(flag: Int, location: Option[String]) derives CommandApplication

@main def hello =
  println(
    summon[CommandApplication[MyCLI]].opt
      .parse(
        // Seq("index", "--index-location", "index.trig", "--language", "scala")
        Seq("--help")
      )
  )

  println(
    summon[CommandApplication[Stuff]].opt
      .parse(
        Seq("--help")
      )
  )
