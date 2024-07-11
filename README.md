# decline-derive

<!--toc:start-->
- [decline-derive](#decline-derive)
  - [Installation](#installation)
  - [Getting started](#getting-started)
  - [Contributing](#contributing)
<!--toc:end-->

Experimental library to quickly derive [Decline](https://ben.kirw.in/decline/) CLI 
interfaces, to quickly prototype CLI applications. 

It is driven by types (with `enum`s and `case classes` representing groups of subcommands and groups of parameters respectively, see example below), and can handle anything Decline has built-in support for, and some extra handling of `Option[..]` and `List[..]` types. The library is inspired by [clap from Rust](https://docs.rs/clap/latest/clap/index.html), but mainly because it's the most recent example I came across - deriving CLI parsers with macros is a long standing tradition in Scala.

This library does not aim to cover every possible case representable with Decline - in fact
in only covers the most basic of cases.
For complicated CLIs I would still recommend using the approach that Decline library recommends.

Acknowledgements:
- [August Nagro's post on how to read annotations from macros](https://august.nagro.us/read-annotations-from-macro.html)
- [Scala 3 documentation page on macro-driven derivation](https://dotty.epfl.ch/docs/reference/contextual/derivation-macro.htm:)

## Installation

Latest version: [![decline-derive Scala version support](https://index.scala-lang.org/indoorvivants/decline-derive/decline-derive/latest.svg)](https://index.scala-lang.org/indoorvivants/decline-derive/decline-derive)

- **SBT**: `libraryDependencies += "com.indoorvivants" %%% "decline-derive" % "<latest version>"`
- **Scala CLI**: `//> using dep com.indoorvivants::decline-derive::<latest version>`
- **Mill**: `ivy"com.indoorvivants::decline-derive::<latest version>"`

## Getting started

```scala
import decline_derive.*

enum CLI derives CommandApplication:
  case Index(location: String, @arg(_.Name("lit")) isLit: Boolean)
  case Run(@arg(_.Positional()) files: List[String])

@main def helloDecline(args: String*) = 
    println(CommandApplication.parse[CLI](args))
```

Notice how we're using `@arg(_.Name("lit"))` to customise certain aspects of 
generated Decline parser.

For more configuration options, see [tests]("./library.test.scala"), [ArgHint]("./ArgHint.scala"), [CmdHint]("./CmdHint.scala") and [annotations]("./annotations.scala").

## Contributing

If you see something that can be improved in this library – please contribute! 
Turning users into contributors and maintainers is one of the purest joys of OSS.

This library was largely put together on board of a plane during a very short flight,
so it cuts a lot of corners when it comes to performance of generated code - mainly 
because I didn't use 

This is a standard Scala CLI project, with a Makefile for some useful commands.

Here are some useful commands:

- `make test` – run tests. Note that this command runs tests for all three platforms - 
    which might be unnecessarily slow for development purposes. Quickest feedback loop 
    is achieved by just running `scala-cli test *.scala`
- `make check-docs` – verify that snippets in `README.md` (this file) compile
- `make pre-ci` – format the code so that it passes CI check
- `make run-example` – run example from README
