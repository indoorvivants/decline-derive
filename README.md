# decline-derive

<!--toc:start-->
- [decline-derive](#decline-derive)
  - [Installation](#installation)
  - [Getting started](#getting-started)
  - [Contributing](#contributing)
  <!--toc:end-->

An _experimental_ [Smithy4s](https://disneystreaming.github.io/smithy4s/) client backend for [Scala.js](https://www.scala-js.org/), utilising [Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch) directly, without introducing a http4s/cats dependency.

The purpose of this library is to provide users of Smithy4s backend services a more lightweight client for the frontend – if your Scala.js frontend is not using Cats/Cats-Effect based libraries, you can communicate with your Smithy4s backend directly using Fetch, **reducing bundle size by as much as 50% in some instances**.

The library is currently only available for Scala 3, but we will welcome contributions cross-compiling it to 2.13 – it should be very easy. API surface is very minimal and designed for binary compatible evolution, so after initial round of testing and gathering community feedback, we plan to release 1.0.0 and start checking binary/Tasty compatibility for each release.

## Installation

Latest version: [![decline-derive Scala version support](https://index.scala-lang.org/neandertech/decline-derive/decline-derive/latest.svg)](https://index.scala-lang.org/neandertech/decline-derive/decline-derive)

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

## Contributing

If you see something that can be improved in this library – please contribute!

This is a relatively standard Scala CLI project, even though the tests run a 
Scala version newer than the library itself (library is published for 3.3, tests are 
in 3.4, to make use of smithy4s-deriving).

Here are some useful commands:

- `make test` – run tests
- `make check-docs` – verify that snippets in `README.md` (this file) compile
- `make pre-ci` – format the code so that it passes CI check
- `make run-example` – run example from README against real https://httpbin.org
