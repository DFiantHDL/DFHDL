# sbt 2.0 Migration — Issues Encountered

> Record of the problems hit while attempting to move this build from **sbt 1.12.13** to
> **sbt 2.0.0** (released 2026-06-14). The attempt was **reverted**; this doc exists so a future
> migration starts from the known set of fixes rather than rediscovering them one reload at a time.
>
> Each problem below was surfaced by `sbt reload` failing the metabuild (build.sbt) compile. sbt 2's
> metabuild is compiled with **Scala 3.8.x**, so every issue here is fundamentally "the build
> definition now goes through the Scala 3 typer and the sbt 2 API."

## Bottom line

The migration is mechanically achievable — none of the issues were blockers — but it touches the
build definition in several non-obvious ways. The full set of edits that got `reload` to pass is
listed in [Fixes that worked](#fixes-that-worked). It was reverted by choice, not because of a wall.

### Things that did *not* need changing

- **Plugins.** `sbt-ci-release` 1.11.2 and `sbt-bloop` 2.0.19 already publish `_sbt2_3` artifacts.
  sbt picks the right suffix from `sbt.version`, so `project/plugins.sbt` and the auto-generated
  `project/metals.sbt` need no edits.
- **CI workflows.** `.github/workflows/*` only run single sbt commands (`sbt test`, `sbt ci-release`,
  `sbt testApps`), never chained ones, so no command-quoting changes are required.
- **`DFHDLCommands.scala`.** We never got far enough to confirm it compiles under the Scala 3.8
  metabuild (the build.sbt errors below came first), but its APIs — `Command.command`,
  `Project.extract`, `appendWithSession`, `runInputTask`, `Command.process`, `LocalProject` — are all
  retained in sbt 2.0. **This is the main unverified area** if the migration is retried.

## The reference points

- Official guide: <https://www.scala-sbt.org/2.x/docs/en/changes/migrating-from-sbt-1.x.html>
- Change summary: <https://www.scala-sbt.org/2.x/docs/en/changes/sbt-2.0-change-summary.html>
- Plugin migration helper: <https://github.com/sbt/sbt2-compat>

## Problems, in the order `reload` surfaced them

Each error came from `project/build.properties` set to `sbt.version = 2.0.0`, then repeatedly running
`sbt reload` and fixing the first error reported.

### 1. Mixed tabs/spaces in indentation

```
[error] build.sbt:67: Incompatible combinations of tabs and spaces in indentation prefixes.
[error] Previous indent : 4 spaces
[error] Latest indent   : 1 tab, 2 spaces
```

One line in the `.aggregate(...)` list (`compiler_stages,`) used a tab + 2 spaces instead of 4 spaces.
Scala 2 tolerated it; the Scala 3 metabuild parser rejects mixed tab/space indentation.
**Fix:** replace the tab with spaces.

### 2. Ambiguous `Append` given for `libraryDependencies ++=`

```
[error] Ambiguous given instances: both given instance appendSeq in object Append and given
[error] instance appendOption in object Append match type sbt.Append.Values[Seq[ModuleID], A2]
[error] of parameter x$2 of method ++= in class SettingKey
```

sbt 2's `SettingKey.++=` is `inline def ++=[A2](inline vs: A2)(using Append.Values[A1, A2])`. `A2` is
inferred from the argument's static type. The argument `commonDependencies` was built from an
anonymous `new { … }` object (see #3) whose member types weren't pinning down to `ModuleID`, so `A2`
stayed open and the given search matched both `appendSeq` (`Seq[V]`) and `appendOption` (`Option[T]`).

**Fix (partial):** annotate `commonDependencies: Seq[ModuleID]`. This is a symptom; the real cause was
#3. Keeping the annotation is still good hygiene.

### 3. `new { … }` widens to `Object` in the Scala 3 metabuild

```
[error] build.sbt:122: value oslib is not a member of Object
```
…then after that was worked around at the definition site:
```
[error] build.sbt:111: Not found: dependencies
```

The build grouped dependency `ModuleID`s in an anonymous structural object:

```scala
lazy val dependencies = new {
  val scodec = "org.scodec" %% "scodec-bits" % scodecVersion
  // …
}
```

Two distinct Scala-3-metabuild facts bite here:

1. **Scala 3 widens the public type of `lazy val dependencies = new { … }` to `Object`**, dropping the
   refinement members — so `dependencies.oslib` is "not a member of Object". (Scala 2 retained the
   structural type. Annotating the member types as `: ModuleID` does not help; the widening is on the
   *enclosing* val.)
2. **Replacing it with `object dependencies { … }` fails differently** — "Not found: dependencies." In
   build.sbt only `val` / `lazy val` / `def` definitions are hoisted into scope for the settings DSL
   expressions; a top-level `object`/`class`/`trait` is not visible to them.

**Fix:** flatten to top-level `lazy val`s, one per dependency (`scodecDep`, `munitDep`, …), each
annotated `: ModuleID`, and update the ~7 reference sites. (Alternatively move the group into a real
`project/Dependencies.scala` — the idiomatic sbt home for an `object`.)

### 4. `packageBin` returns `xsbti.HashedVirtualFileRef`, not `File`

```
[error] build.sbt:151: value lastModified is not a member of xsbti.HashedVirtualFileRef
```

sbt 2 changed classpath/artifact types from `File` to `xsbti.HashedVirtualFileRef`. The plugin-wiring
settings used `(plugin / Compile / packageBin).value.getAbsolutePath` / `.lastModified` (to build the
`-Xplugin:` path and a `-Jdummy=<mtime>` cache-buster).

**Fix:** resolve the ref to a real path through the build's `fileConverter`:

```scala
val jar = fileConverter.value.toPath((plugin / Compile / packageBin).value).toFile
```

`fileConverter` is a `SettingKey[xsbti.FileConverter]`; `toPath(ref): java.nio.file.Path`.

### 5. Overlapping output directories (root vs lib)

```
[error] Overlapping output directories:
[error]   …/target/out/jvm/scala-3.8.4/dfhdl:
[error]     ProjectRef(…,root)
[error]     ProjectRef(…,lib)
```

sbt 2 derives each project's output dir from its **name** under a shared `target/out/<platform>/<scala>/<name>`
tree. In sbt 1.x projects were separated by their base directory (`./target`, `lib/target`), so two
projects sharing a name was fine. The root project was named `dfhdl` (via the old bare
`name := projectName`) and so is `lib` (the published artifact) — now a collision.

**Fix:** let the root project keep its default name (`root`); only `lib` should be `dfhdl`. Related: in
sbt 2 **bare settings in build.sbt are injected into *all* subprojects**, not just root — so
`name := projectName` would have renamed *every* module to `dfhdl` anyway. Root-only settings
(`commands += …`, any root `name`) must be scoped to `LocalRootProject`, or the `name` simply dropped.

## Fixes that worked (summary)

| # | Problem | Fix |
|---|---------|-----|
| 1 | Tab/space indentation in `.aggregate` | Use spaces |
| 2 | Ambiguous `Append` given on `++=` | Annotate `commonDependencies: Seq[ModuleID]` (root cause is #3) |
| 3 | `new { … }` → `Object`; `object` not in DSL scope | Flatten deps to top-level `lazy val: ModuleID` (or `project/Dependencies.scala`) |
| 4 | `packageBin` is `HashedVirtualFileRef` | `fileConverter.value.toPath(ref).toFile` |
| 5 | root/lib output dir collision | Drop root `name` override; scope root-only settings to `LocalRootProject` |

## Not yet reached / open questions for a retry

- **`project/DFHDLCommands.scala` under Scala 3.8.** Confirm the custom commands compile and that
  `appendWithSession` / `runInputTask` / `Command.process` behave the same. APIs exist; semantics
  unverified.
- **Task caching.** sbt 2 caches all tasks by default; result types need an `sjsonnew.JsonFormat` or
  must be wrapped in `Def.uncached(...)`. The `Compile / resourceGenerators` task (writes
  `version.properties`) returns `Seq[File]` and was *not* yet exercised — verify it doesn't trip the
  cache requirement.
- **`Resolver.scalaNightlyRepository` and `localStaging`.** Used in build.sbt; existence under sbt 2.0
  was not independently confirmed (the build failed earlier, so they were never evaluated).
- **`exportJars` now defaults to `true`** in sbt 2 — check it doesn't change how the compiler plugin
  jar is picked up.
