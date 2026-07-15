# DFHDL Compile Time Performance Guide

## CURRENT STATE (updated 2026-07) - read `devdocs/compile-perf-journal.md`

The detailed, dated experiment log now lives in
[`devdocs/compile-perf-journal.md`](../../devdocs/compile-perf-journal.md).
Highlights that supersede parts of this older guide:

- **The bottleneck moved.** The `FlattenInlinedPhase.minimizeCall` fix already
  cut `posttyper` from 20-70 s down to ~4.5 s. On the `compiler_stages` test
  compile (the heaviest workload) the phase split is now roughly:
  **typer 65% (~162 s), inlining 16% (~40 s), CodeDigest ~2.6 s, everything
  else small.** `posttyper` is no longer the problem; **typer is.**
- **Shipped win 1:** `CodeDigestPhase` used to hash `tree.show` (which renders
  the giant inferred types to text) on every top-level class - ~17 s. It now
  folds a structural digest via one traversal (~2.6 s). ~9% off the whole
  compile. Verify cross-build cache soundness with `testApps` before trusting it.
- **Shipped win 2:** `Check1`/`Check2` now have a statically-true fast-path given
  (`ok`, `CondValue` fixed to `true`, mirroring `CheckNUB.ok`) that returns
  `CheckOK` without running `checkMacro`. Most width/sign checks hold statically,
  so this skips their macro splice: **typer 162 s -> 146 s (-16 s)**. Pure
  library code, transfers to future compiler versions. Together with win 1 the
  compile is ~258 s -> ~222 s.
- **Key correction:** the high check-expansion counts are per-operation VOLUME,
  NOT redundant re-expansion (`a+a+...(N)` -> `2N+2` checks, LINEAR). So there is
  nothing to "memoize"; the lever is fewer/cheaper checks per op (win 2 is the
  cheaper-check direction).
- **typer's cost is transparent-inline RE-EXPANSION.** A single `y := a+a` fires
  ~4 distinct `Check` and ~4 distinct `AssertGiven` macros (all legitimate). But
  in a chain `a + a + ... (N)`, an inner width check re-expands once per
  enclosing op (`transparent inline` operators take `inline` operands, so the
  operand's expansion re-runs when it becomes the next op's `inline lhs`).
  `DualSummonTrapError` (behind `==`/`<>`) doubles it again by searching both
  arms. That is why the trace shows 7699 `AssertGiven` / ~6000 `Check`
  expansions from far fewer written checks.
- **Ruled out (do not retry - see journal for why):** replacing `AssertGiven`
  with `summonFrom` (breaks the `DualSummonTrapError` trap); dropping
  `transparent` from `AssertGiven` or `Check` (just moves the macro work from
  typer to `inlining`, net SLOWER: typer -7/-32 s but inlining +24/+42 s);
  sealing the `exactOp2` result behind a val (no change - the re-expansion is
  the compiler re-expanding the transparent inline, not the macro output tree).
- **The remaining big lever is compiler-side:** memoize transparent-inline /
  macro expansion by (symbol, type args, argument-tree identity) so an identical
  nested transparent-inline application is not re-expanded once per enclosing
  level. Prototype in the scala3 fork. NOTE: DFHDL is pinned to Scala 3.8.4, so
  a fork build must be based on the 3.8.4 tag (not the fork's newer nightly HEAD)
  to test against DFHDL.

### Useful environment note

Default sbt heap (1 GB) OOMs / GC-thrashes this workload. Use a local `.jvmopts`
(not committed): `-Xmx10G`, `-Xss8M`, `-XX:+UseG1GC`,
`-XX:ReservedCodeCacheSize=1G`. After touching `internals/` macros, `clean`
(zinc misses macro changes); if the incremental build acts stale, `sbtn
shutdown` then `clean`.

### Diagnosing macro re-expansion

To see how often a macro re-runs per source site: add
`System.err.println(s"[TAG] ${Position.ofMacroExpansion.sourceFile.name}:${Position.ofMacroExpansion.startLine+1}")`
(optionally + the checked type) at the top of the macro impl, rebuild, and
compile a tiny design via `corePlayground`; `grep | sort | uniq -c` the tags.

## Quick Setup for Debugging Compile Times

```bash
sbtn.bat corePlayground   # Only compiles core/src/test/scala/Playground.scala
sbtn.bat libPlayground    # Only compiles lib/src/test/scala/Playground.scala
sbtn.bat compile          # Compile dependencies (run after clean or code changes)
sbtn.bat "core/Test/compile"  # Compile just the Playground test file
```

Enable profiling in `build.sbt` (in `pluginTestUseSettings`):
```scala
"-Yprofile-enabled",
// "-Yprofile-trace:compiler.trace"
```

Always use `sbtn.bat` (sbt client), never `sbt`.

## Test Methodology

### Measuring Compile Time

1. Run `corePlayground` or `libPlayground` once per session (persists until `shutdown`)
2. Run `clean` then `compile` to build dependencies
3. Edit `Playground.scala` with the test code
4. Run `core/Test/compile` — this compiles ONLY the Playground file
5. To force recompile without changing code, add/change a comment (e.g., `// v2`)
6. Compare `Total time` or `posttyper` phase timing with `-Yprofile-enabled`

When running the full test suite, always separate compilation from execution:
```bash
sbtn.bat Test/compile    # compile all tests first — measure compile workload
sbtn.bat test            # then run tests — measure runtime workload
```
This avoids conflating compile time with test runtime in measurements.

### When to Run `clean`

- **After modifying `internals/` or `plugin/`** — the incremental compiler doesn't always detect changes in macro code or plugin classes. A stale compiled macro will produce the OLD tree even though the source changed. Always `clean` after touching:
  - `internals/src/main/scala/dfhdl/internals/Exact.scala`
  - `internals/src/main/scala/dfhdl/internals/Checked.scala`
  - `plugin/src/main/scala/plugin/*.scala`
  - Any file that defines macros or compiler plugin phases
- **After modifying `core/`** — changes to `DFVal.scala`, `DFDecimal.scala`, etc. affect inline definitions that downstream code depends on
- **NOT needed** when only editing `Playground.scala` — incremental compilation handles this correctly

### OOM and JVM Issues

The sbt JVM can run out of memory when compiling large test suites or after many incremental compilations. Symptoms:
- `java.lang.OutOfMemoryError: Java heap space`
- `GC overhead limit exceeded`
- Compile times suddenly spike to 5+ minutes
- sbt becomes unresponsive

Recovery:
```bash
taskkill //F //IM java.exe    # Kill all Java processes (Windows)
sleep 2                        # Wait for cleanup
sbtn.bat clean                 # Start fresh
sbtn.bat compile               # Rebuild
```

The `sbtn` client will auto-start a new JVM server. Always kill java BEFORE starting sbtn again, otherwise the old server may still hold ports/locks.

When running the full test suite (`sbtn.bat test`), watch for OOM — the test compilation of all spec files plus Playground can exhaust 1GB heap. If the `lib/src/test/scala/Playground.scala` or `core/src/test/scala/Playground.scala` contain heavy test code (many operands, design hierarchies), comment them out before running the full suite.

### Printing Trees for Debugging

In the `FlattenInlinedPhase` plugin, use `prepareForUnit` / `transformUnit` overrides to print trees before/after transformation:
```scala
override def prepareForUnit(tree: Tree)(using Context): Context =
  if tree.source.path.toString.contains("Playground.scala") then
    println(tree.show)
  ctx

override def transformUnit(tree: Tree)(using Context): Tree =
  val result = treeMap.transform(tree)
  if tree.source.path.toString.contains("Playground.scala") then
    println(result.show)
  result
```

Use `System.err.println` for debug output that needs to bypass sbt's output buffering.

### Upstream Scala 3 Bug

The root cause is a performance bug in `PostTyper.scala` (line ~568 in current nightly):
```scala
case tree @ Inlined(call, bindings, expansion) if !tree.inlinedFromOuterScope =>
  ...
  withMode(Mode.NoInline)(transform(call))  // expensive, result DISCARDED
  val callTrace = Inlines.inlineCallTrace(call.symbol, pos)(...)
  cpy.Inlined(tree)(callTrace, ...)          // callTrace replaces call entirely
```
`transform(call)` re-transforms the full call tree (running `checkBounds`, `normalizeTypeArgs`, etc. on any TypeApply inside it), but the result is thrown away — the next line computes `callTrace` (a minimal `Ident`) and uses that instead. With N chained transparent inlines producing O(N) Inlined nodes with increasingly complex call trees, this causes O(N²) or worse behavior.

**Fix**: Either remove `withMode(Mode.NoInline)(transform(call))` entirely, or replace it with just the side-effecting checks needed (which `CrossVersionChecks.checkRef` already covers on the line above). See https://github.com/scala/scala3/blob/main/CONTRIBUTING.md for contribution guide.

A Scala 3 clone is available at `C:\Users\OronPort\IdeaProjects\forclaude\scala3` for building a minimal reproducer and preparing the upstream PR.

## Root Cause: Transparent Inline Expansion

The primary compile time bottleneck comes from Scala 3's `transparent inline` mechanism used extensively in DFHDL for type-level computation. The key file is `internals/src/main/scala/dfhdl/internals/Exact.scala`.

### How It Works

Operations like `+`, `-`, `<>`, `apply` are defined as:
```scala
transparent inline def +(inline rhs: SupportedValue)(using DFCG): DFValAny =
  exactOp2[FuncOp.+.type, DFC, DFValAny](lhs, rhs)
```

`exactOp2` is a `transparent inline` macro that:
1. Extracts the exact types of `lhs` and `rhs` via `exactInfo`
2. Summons an `ExactOp2` typeclass instance for those exact types
3. Returns the result with the precise output type

### The Exponential Problem

When chaining operations like `a + b + c + d`, each `+` is `transparent inline`, and:
- The `inline lhs` parameter causes the entire previous expansion to be substituted
- The compiler's `posttyper` phase re-processes the growing tree
- Tree sizes grow linearly but `posttyper` time grows super-linearly

### What We Fixed

1. **`cleanTypeHack` removal** — Was an extra `transparent inline` + `inline match` layer around every `exactOp*` call. Replaced with `ascribeWidenedType` using `.dealias` type ascription. See `Exact.scala`.

2. **`flattenInlined` in macros** — The macros now flatten nested `Inlined` nodes from subexpressions by extracting val bindings into a flat `Block`.

3. **`ascribeWidenedType`** — Adds a `Typed` node with `.dealias` type to prevent unreduced type projections (`ExactOp2Aux[...].Out`) from leaking into error messages.

4. **Non-transparent Check/CTName/DualSummonTrapError givens** — Removed unnecessary `transparent` from `inline given` instances that don't need type narrowing.

5. **`FlattenInlinedPhase` compiler plugin** — Runs between `typer` and `posttyper`. Flattens nested `Inlined` trees and pre-minimizes the `call` field of `Inlined` nodes to a simple `Ident` pointing to the top-level class. This prevents PostTyper from re-transforming complex call trees (which contain TypeApply with heavy type arguments) for every `Inlined` node. This is the single biggest compile time fix — it reduced 8-addition chains from 33s to 2s and eliminated the exponential growth entirely (20 additions: previously impossible, now 7s).

## Key Files

| File | Role |
|---|---|
| `internals/src/main/scala/dfhdl/internals/Exact.scala` | ExactOp1/2/3 macros, cleanTypeHack, flattenInlined |
| `internals/src/main/scala/dfhdl/internals/Checked.scala` | Check1/Check2 constraint macros |
| `plugin/src/main/scala/plugin/FlattenInlinedPhase.scala` | Compiler plugin phase for tree optimization |
| `plugin/src/main/scala/plugin/Plugin.scala` | Plugin registration |

## Profiling Results

The `posttyper` phase is the main bottleneck. Use `-Yprofile-enabled` to measure.

### Compiler Phase Breakdown (10-operand `a + b + c + ... + j`)
- `typer`: ~1-2s (macros run here, very fast)
- `posttyper`: 20-70s depending on optimizations (the bottleneck)
- `inlining`: <1s
- All other phases: <1s each

### What PostTyper Does

PostTyper performs post-typing checks on inline-expanded code:
- Overriding checks
- Type bound verification
- Import resolution
- Re-traverses the expanded tree multiple times

### What Drives PostTyper Time

**The `call` field of `Inlined` nodes** — PostTyper's `Inlined` handler calls `withMode(Mode.NoInline)(transform(call))` on every Inlined node. The result is then immediately discarded and replaced by `inlineCallTrace(call.symbol, pos)`, which is just an `Ident` to the top-level class. With chained inlines producing 100+ Inlined nodes, each with a complex call tree containing TypeApply, PostTyper re-transforms all of these call trees for nothing. Pre-minimizing the call to the same Ident that PostTyper would produce eliminates this entirely.

**NOT type lambda complexity** — replacing complex `[LS, RS] =>> LS || !RS` with trivial `[LS, RS] =>> Boolean` had zero impact on posttyper time.

**NOT node count alone** — removing 200 nodes from a 2300-node tree gave marginal improvement.

**NOT InferredTypeTree spans** — PostTyper's `checkInferredWellFormed` gates on `span.isZeroExtent` for TypeTree nodes. Marking all 457 zero-extent TypeTrees with non-zero spans had zero impact — this check is not the bottleneck.

**NOT TypeApply type arg dealiasing** — dealiasing all TypeApply type arguments had negligible impact. The types are already small individually; the issue was the call-tree re-transformation, not type complexity.

### What We Tried That Didn't Work

1. **Replacing TypeLambda args with `Any`** — Violates HK upper bounds (`[T <: Int] =>> Boolean` can't accept `Any`).

2. **Replacing TypeLambda args with `Nothing`** — Crashes Check macro when it tries to interpret `Nothing` as a lambda body for runtime checks.

3. **Replacing TypeLambda args with upper-bound lambdas** — Works for compile-time checks but breaks `CheckNUBLP` runtime fallback in `lib`.

4. **Simplifying `asInstanceOf` type args** — The `CheckOK.asInstanceOf[Check[...]]` pattern doesn't appear in trees because Check givens are now non-transparent (the `asInstanceOf` is inside the macro, not in expanded code).

5. **Replacing `CheckNUB.ok[...](...)(...)`  with `CheckNUBOK`** — Pattern matching failed because these calls are inside nested Inlined nodes that the TreeMap doesn't visit in the right order.

6. **Removing Check type signatures from trees** — User confirmed no performance impact. The Check types are not the bottleneck.

### Resolved

The compile time issue is resolved. The `minimizeCall` optimization in `FlattenInlinedPhase` eliminates the exponential growth in PostTyper. This is also a general Scala 3 compiler bug — PostTyper's `Inlined` handler re-transforms call trees whose results are immediately discarded. A minimal reproducer and upstream patch should be filed against the Scala 3 compiler.

## Checked.scala Architecture

`Check1` / `Check2` are type-level constraint systems:
- **Check**: Trait with `Cond` and `Msg` type lambdas, macro-generated
- **CheckNUB**: "Not Upper Bounded" variant, handles unknown types at compile time
- **CheckNUB.ok**: Fast path when condition is statically true (returns `CheckNUBOK`)
- **CheckNUBLP**: Fallback when condition needs runtime evaluation (summons `Check` which triggers the macro)
- **CheckOK / CheckNUBOK**: Singleton no-op objects

The macro (`checkMacro`) checks `condOpt`:
- `Some(true)` → returns `CheckOK.asInstanceOf[Check[...]]` (no runtime check needed)
- `Some(false)` → compile error
- `None` → generates runtime check code (`if (!cond) throw ...`)
