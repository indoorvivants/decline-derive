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

case class Stuff(flag: Int, location: Option[String]) derives CommandApplication

@main def hello =
  println(
    summon[CommandApplication[MyCLI]].opt
      .parse(
        Seq("index", "--index-location", "index.trig", "--language", "scala")
      )
  )

  println(
    summon[CommandApplication[Stuff]].opt
      .parse(
        Seq("--help")
      )
  )
