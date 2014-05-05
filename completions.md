The Completions plugin
======================

To use tab-completion, your sbt version must support the `completions`
command. The command is available in sbt 0.13.2, but for earlier
versions you need to use a plugin.

Add the CompletionPlugin to your sbt environment as a global
plugin available to all projects. The directory depends on the sbt
version:

- sbt 0.12: `~/.sbt/plugins/`
- sbt 0.13.1: `~/.sbt/0.13/plugins/`

You need these two files in the plugins directory:

`CompletionsPlugin.scala`:

```scala
import sbt._
import Keys._
import sbt.complete._
import Parsers._

object CompletionsPlugin extends Plugin {
  override lazy val settings = Seq(commands += completions)

  lazy val completions = Command.make("completions") { state =>
    val notQuoted = (NotQuoted ~ Parsers.any.*) map {case (nq, s) => (nq +: s).mkString}
    val quotedOrUnquotedSingleArgument = Space ~> (StringVerbatim | StringEscapable | notQuoted)

    Parser.token(quotedOrUnquotedSingleArgument ?? "" examples("", " ")) map { input =>
      () => {
        Parser.completions(state.combinedParser, input, 1).get map {
          c => if (c.isEmpty) input else input + c.append
        } foreach { c =>
          println("[completions] " + c.replaceAll("\n", " "))
        }
        state
      }
    }
  }
}
```

`build.sbt`:


```
sbtPlugin := true
```

