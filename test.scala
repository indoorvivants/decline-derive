package decline_derive

enum MyCLI derives CommandApplication:
  case Index(location: String)
  case Search(location: String, repl: Boolean)

@main def hello =
  println(
    summon[CommandApplication[MyCLI]].opt
      .parse(Seq("search", "--location", "./index.trig", "--repl"))
  )
