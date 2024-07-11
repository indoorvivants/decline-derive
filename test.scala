package decline_derive

@cmd(_.Name("my-cli"))
enum MyCLI derives CommandApplication:
  case Index(
      @arg(
        _.Name("index"),
        _.Short("l"),
        _.Help("location of index file")
      ) location: String,
      language: Option[String],
      things: Option[List[String]]
  )

  @cmd(_.Name("search")) case Search(location: String, repl: Boolean)

  @cmd(_.Name("test-me"), _.Help("this tests stuff")) //
  case Test(
      bla: Int
  )

case class Stuff(flag: Int, location: Option[String]) derives CommandApplication

@main def hello =
  println(
    summon[CommandApplication[MyCLI]].opt
      .parse(
        Seq("index", "--help")
        // Seq("--help")
      )
  )
