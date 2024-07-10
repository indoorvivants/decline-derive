package decline_derive

import ArgHint.*
enum MyCLI derives CommandApplication:
  case Index(
      @arg(
        Name("index-location"),
        Short("l"),
        Help("location of index file")
      ) location: String,
      language: Option[String]
  )
  case Search(location: String, repl: Boolean)
  case Test(bla: Int)

@main def hello =
  println(
    summon[CommandApplication[MyCLI]].opt
      .parse(Seq("index", "--language", "java"))
  )
