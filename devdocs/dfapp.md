# DFApp

The application engine behind every runnable DFHDL design: what the compiler plugin generates around
it, where a run's options come from, and what actually reads them.

`DFApp` (`lib/src/main/scala/dfhdl/app/DFApp.scala`) deliberately has no `main` of its own. The
plugin injects one into the design's companion object, so the companion (`Foo`) is the single,
cleanly named runnable entry point, and `DFApp` stays a plain class that the generated code drives.

## The generated entry point

`TopAnnotPhase.mkMainDef` builds this for a `@top` design `Foo(width: Int <> CONST = 8)`:

```scala
object Foo:
  def main(args: Array[String]): Unit =
    val app = new DFApp()
    app.setInitials(classOf[Foo], "Foo", "Foo", <@top annotation>, List("width"), List(<8>),
                    List(<doc>), <scalac -Werror>, <hasResourceOwner>, <hasPorts>)
    def mkDsn(dfc: DFC): Foo = new Foo(app.getDsnArg("width"))
    app.setDsn(mkDsn(app.getDsnDFC))
    app.run(args)
```

The plugin resolves five `DFApp` members BY NAME, so renaming any of them silently breaks entry-point
generation (the plugin's `select("...".toTermName)` fails at the generated call site, not here):

| Member | Purpose |
|---|---|
| `setInitials` | hands over everything known at compile time: the design's identity, its `@top` options, and its parameters |
| `getDsnArg` | reads one design argument, after the command line has had its say |
| `getDsnDFC` | the DFHDL context the top design elaborates in |
| `setDsn` | receives the design BY NAME, so nothing is elaborated until a step demands it |
| `run` | takes the raw argv |

Users rarely write `@top` themselves: `PreTyperPhase` injects it onto every concrete class that looks
like a design (a `Design` parent, a `<> CONST` parameter, or `<>` in the body), skipping traits, case
and enum classes, interfaces, and anything with more than one parameter block. It injects the
explicit `@top(true)` form on purpose, since that is the lenient variant: `TopAnnotPhase` silently
skips entry-point generation when such a class turns out not to be a `Design`, whereas a bare `@top`
written by hand is strict and reports a compile error. `@top(false)` opts out entirely.

The same phase also ensures the companion object exists BEFORE the typer runs, which is what lets
`TopAnnotPhase` inject `main` into a real companion later (a same-named module created post-typer is
not recognized as one, and the backend would emit a clashing mirror class). A design nested inside an
object cannot expose a `main` through its nested companion at all, so `mkPackageEntryObject` hosts
one in a fresh top-level object named by the nesting path (`object inside { class Foo }` gives
`inside_Foo`).

### Laziness is load-bearing

`setDsn` takes its argument by name and stores it as a thunk. The design is therefore constructed,
and its body elaborated, only when the `elaborate` step pulls on it, which is after `run` has parsed
the command line. Everything the design sees, its arguments and its context, is resolved at that
moment. `mkDsn` is a local method rather than a direct `new Foo(...)` for a related reason, described
under [The design's context](#the-designs-context).

## Two ways in

| Path | Entered by | Command line | Notes |
|---|---|---|---|
| App | the plugin-generated `main`, calling `run(argv)` | yes | the only path with design arguments |
| Manual | `dsn.lint` / `dsn.simulate` / `dsn.build` / `dsn.program` (`lib/src/main/scala/dfhdl/default.scala`), calling `runManual(mode)` | no | `ManualDFApp` wraps the user's design expression |

The manual path never calls `setInitials`: `runManual` takes the option sets as `using` parameters
from the call site and forces `onError = Exception` on the ones that can abort. It also constructs
the design in the USER's scope, not through `mkDsn`, so a manually run design keeps the context of
its own declaration site. Both paths share everything from `execute` down.

## Where a run's options come from

The `@top` annotation carries the eight option sets in its third parameter list, resolved at the
design's DECLARATION site:

```scala
final case class top(genMain: Boolean = true)(using annot: AnnotatedWith[top, Any])(using
    val elaborationOptions: ElaborationOptions.Defaults[annot.Out], ...)
```

That is how a user's `given options.CompilerOptions.Backend = _.vhdl` reaches the app. `setInitials`
copies each set into a mutable field, applies the `-Werror` scalac flag to the tool option sets, and
picks the default `AppMode`:

| The design | Default mode |
|---|---|
| runs in Scastie | `compile` |
| has no ports | `simulate` |
| declares board resources (`platforms.resources.ResourceOwner`) | `program` |
| anything else | `commit` |

`run` then applies the command line on top of those fields. So the layering is
**declaration-site `given`s, then the command line**, and the help screen's `(default = ...)` is the
first layer showing through.

### An option is only effective where someone reads it

This is the part that bites. There are two distinct readers, and which one an option has determines
whether a command-line override does anything at all.

| Reader | Options | Reached by |
|---|---|---|
| `DFApp` itself | all the `printDFHDLCode` / `printBackendCode` flags, every `CompilerOptions`, `LinterOptions`, `SimulatorOptions`, `BuilderOptions`, `ProgrammerOptions`, `AppOptions` field | the app's own fields and its `inline given`s |
| the design's `DFC` | `ElaborationOptions.cacheEnable`, `Werror`, `onError`, `trapErrors`, `defaultClkCfg` / `defaultRstCfg` | `dfc.elaborationOptions`, during elaboration |

Historically only the first reader saw the command line, because a design's context was built from
the annotation's options directly. `--nocache` therefore gated the app's step cache while the
sub-design cache kept serving cached bodies, and `--Werror` was inert: it parsed, it appeared in
`help`, it updated a field, and nothing ever read that field. Both are fixed by
[`getDsnDFC`](#the-designs-context).

One field currently has NO reader: `ElaborationOptions.logLevel`. `--log` works entirely through
`CompilerOptions.logLevel` (which `StageRunner` reads) and the app's own logger. The elaboration
field is set for symmetry and does nothing.

## The design's context

`MetaContextPlacerPhase` gives every design instantiation a context by overriding `__dfc` on a
synthetic anonymous subclass. Its rule is: use the DFHDL context in scope at the instantiation site
(`ContextArg.at` scans the enclosing method's parameters for a meta-context type), and only when
there is none, fall back to `DFC.empty(<the @top annotation's elaborationOptions>)`.

`mkDsn(dfc: DFC)` exists to put a context in scope at the top design's instantiation site, so the
normal rule applies instead of the fallback. The app supplies that context:

```scala
private def newDFC: core.DFC = core.DFC.empty(elaborationOptions)
final def getDsnDFC: core.DFC = newDFC
```

Since `setInitials` seeds `elaborationOptions` from the same annotation the fallback would have used,
this changes nothing on a run with no command line, and carries every override on a run with one.

### Two contexts, shared options, separate DBs

`DFApp` builds two contexts from `newDFC`, and the separation is deliberate. A `DFC` owns a
`MutableDB`; rebuilding a design argument's constant (`--mask h"5a"` becomes a fresh `DFBits` const)
allocates IR members into it, and those must never land in the design's own DB.

| Context | Used for |
|---|---|
| `given dfc: DFCG` | reading and rebuilding design arguments |
| `getDsnDFC` | the top design's elaboration |

What they DO share is the options, so that a user's `OnError` governs a failure while parsing a
command-line argument exactly as it governs an elaboration error. `exitWithError`
(`core/.../DFError.scala`) reads `dfc.elaborationOptions.onError`, and an ownerless context like the
design-argument one reaches it directly from `trydf`.

`given dfc: DFCG` has a second, unrelated job: `DFCG <: DFC`, so its presence in scope is what makes
`println` in `DFApp.scala` resolve to Scala's rather than to DFHDL's text output (with the
`dfhdl.hw.flag.scalaPrints` import). It compiles to a lazy val, which is what keeps it from being
forced before `setInitials` has set the options.

## The step pipeline

The modes are `DiskCache.Step` nodes (`internals/.../DiskCache.scala`), each cached under
`sandbox/<TopName>/cache`:

```
elaborate -> compile -> commit -> lint
                              -> simPrep -> simRun
                              -> build   -> program
```

| Step | Extra cache-key parts |
|---|---|
| `elaborate` | `designCodeDigest`, DFHDL version, default RT domain config, design arguments |
| `compile` | default RT domain config, `dropUserOpaques`, printer `align`, backend |
| `commit` | none (inherits its input's key), declares generated files |
| `simPrep` | tool, tool version, tools location, backend; declares generated files |
| `simRun` | tools location |
| `build` | `flash`, tool; declares generated files |
| `lint`, `program` | none |

Three invariants worth knowing before touching this:

- **A step that cannot serialize its result must be invoked with `uncached = true`.** `lint`,
  `simRun`, and `program` define `valueToCacheStr` as `???`, and `execute` calls exactly those three
  with `uncached = true`. They are terminal actions whose value is the side effect.
- **`hasGenFiles` steps declare their outputs** through `genFiles`, so a cache hit restores files to
  disk rather than merely returning a value.
- **`AppMode.help` never reaches `execute`.** `run` handles it before dispatching, which is why the
  `case AppMode.help => ???` there is unreachable rather than wrong.

`designCodeDigest` is what makes the elaborate step safe across rebuilds: it folds in the entry-point
class, everything it transitively references, the DFHDL version, and the plugin that compiled it. See
[elaboration-caching.md](elaboration-caching.md) for how it relates to the two finer-grained caches
inside elaboration.

## The command line

`ParsedCommandLine` (scallop) turns argv into the mode and its options. Its structure is
`[design-args] [app-options] <mode> [mode-options]`, and its defaults are read from the option sets
`setInitials` resolved, which is why the help screen always reflects the user's source. The
user-facing reference is [docs/user-guide/command-line](../docs/user-guide/command-line/index.md);
what matters here is only that `run` copies the parsed values back into the app's fields before
`execute`.

Design arguments are exposed for every parameter DFHDL can represent, and CLI values are parsed into
fresh DFHDL constants by `DesignArgs.updateScalaValue` under the design-argument context.

## Gotchas

- **`wvlet.log.Logger` is a process-global singleton keyed by name.** `Logger("DFHDL App")` returns
  the same instance for every `DFApp` in the JVM, so the constructor pins its level explicitly.
  Without that, a `--log off` run leaked its level into every later run in the same JVM, which is
  routine under sbt's unforked `runMain` and in tests.
- **Under sbt, the app does not exit.** `run` calls `sys.exit` only when neither `sbtShellIsRunning`
  nor `sbtnIsRunning`, so a CLI parse error returns normally inside a build tool.
- **A cancelled tool run is not a failure.** `execute` swallows `ToolInterruptedException`, since
  `Tool.exec` has already reported the interruption and killed the process tree.
- **The caching flag covers two different caches.** `--cache`/`--nocache` sets both
  `AppOptions.cacheEnable` (the step pipeline) and `ElaborationOptions.cacheEnable` (the sub-design
  cache consulted during elaboration).

## Tests

`lib/src/test/scala/app/DesignArgsCLISpec.scala` drives the plugin-generated `main` reflectively
through `DesignArgsCLIHelper.java` (a Java file, so the plugin does not rewrite the reflection calls)
and captures its output. It covers the help screen, design-argument round trips through elaboration,
malformed literals, the nested entry object, `--log`, and the elaboration options reaching the
design's context.

That last one needs a fixture that reports what its own context saw (`TestCLIElabFlags`), because the
values are otherwise invisible from outside. Its `tag` argument exists to defeat the app's own
elaborate-step cache: a fresh value changes the step key, forcing the body to actually run.
