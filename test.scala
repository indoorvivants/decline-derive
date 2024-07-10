package decline_derive

enum MyCLI derives CommandApplication:
  case Index(
      @arg(
        ArgHint.Name("other-location"),
        ArgHint.Help("location of index file")
      ) location: String
  )
  case Search(location: String, repl: Boolean)
  case Test(bla: Int)

@main def hello =
  println(
    summon[CommandApplication[MyCLI]].opt
      .parse(Seq("test", "--bla", "150"))
  )
