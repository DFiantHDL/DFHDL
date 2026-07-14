# Compile-Time Performance Journal (compiler_stages tests)

> Running log of experiments to reduce the time to compile `compiler_stages`'
> test sources, which exercise the DFHDL compiler plugin and compile-time
> checks the hardest. Append-only; newest section at the bottom.

## Environment

- Host: remote Claude Code container, 4 vCPU, 15 GB RAM, JDK 17.0.19.
- sbt 1.12.13 via `sbtn` (client/server). **Default sbt heap is 1 GB**, which
  OOMs and GC-thrashes on this workload (a first warmup run spent >90% of wall
  time in GC and then threw `OutOfMemoryError` after 407 s).
- Fix applied for all measurements: local `.jvmopts` (NOT committed) with
  ```
  -Xmx10G
  -Xss8M
  -XX:+UseG1GC
  -XX:ReservedCodeCacheSize=1G
  ```
  Heap is held constant across every measurement so comparisons are valid.

## Methodology

Per the task and the `/compile-perf` skill:

1. Warmup: `compiler_stages/Test/compile` (also builds all upstream deps).
2. Clean only the tests: `compiler_stages/Test/clean`.
3. Time `compiler_stages/Test/compile` (compiles only the 72 test sources;
   upstream classes stay cached). Repeat.

Wall time is measured with `date`; sbt also prints its own `elapsed`. The two
agree within ~1 s, so sbt's `elapsed` is used as the authoritative figure.

The 72 `compiler_stages` test sources are the target because they lean hardest
on the plugin and on DFHDL's transparent-inline / compile-time-check machinery.

## Baseline (branch `performance`, plugin enabled, no profiler)

Warmup (deps + tests, cold): 296 s.

Clean-tests-then-compile (72 sources only):

| Iter | sbt elapsed | wall |
|------|-------------|------|
| 1    | 271 s       | 272 s |
| 2    | 256 s       | 256 s |
| 3    | (cut off by tool time budget) | - |

**Baseline ≈ 260 s (~4.3 min)** to compile the 72 compiler_stages test sources.

This is the number to beat.

## Phase breakdown with `-Yprofile-enabled` (258 s run)

Enabled `Test / scalacOptions += "-Yprofile-enabled"` in `pluginTestUseSettings`.
Profiler overhead is negligible (258 s vs 256-271 s baseline). Per-phase `run ns`
aggregated across all 72 files:

| Phase | Time (s) | % of total | Notes |
|-------|---------:|-----------:|-------|
| **typer** | **166.6** | **65%** | transparent-inline expansion + macros (exactOp/Check) + given search + inference |
| **inlining** | **41.7** | **16%** | expansion of regular (non-transparent) `inline def`s |
| **CodeDigest** | **15.6** | **6.0%** | DFHDL plugin phase (ours) |
| posttyper | 4.5 | 1.8% | already fixed by prior `FlattenInlinedPhase.minimizeCall` work |
| genBCode | 2.4 | | |
| MegaPhase{crossVersionChecks..} | 2.5 | | |
| erasure | 2.2 | | |
| OnCreateEvents | 1.8 | | DFHDL plugin |
| MetaContextGen | 1.6 | | DFHDL plugin |
| CodeDigest+other DFHDL plugin phases | ~10 total | | LoopFSM, CustomControl, DesignDefs, etc. each <1.1s |
| PureCheck | 1.5 | | DFHDL plugin |
| ~15 other standard phases | <1 each | | |

**Key takeaway: the bottleneck has shifted.** The `/compile-perf` skill notes
(written earlier) say posttyper was the 20-70 s bottleneck and typer was
"1-2 s". After the `FlattenInlinedPhase.minimizeCall` fix, **posttyper is down to
4.5 s** and the cost has moved into:

1. **typer (166.6 s, 65%)** - the dominant cost. This is transparent-inline
   expansion, macro execution (`exactOp*`, `Check*`), given/implicit search, and
   dependent-type inference. Inherent to how DFHDL encodes types; hard to strip
   with a plugin because it happens *during* typing, before any plugin phase runs.
2. **inlining (41.7 s, 16%)** - expansion of the remaining non-transparent
   `inline def`s (the actual runtime ops). Runs *after* posttyper.
3. **CodeDigest (15.6 s, 6%)** - one of our own plugin phases; worth auditing
   since we fully control it.

### Implication for the "strip post-typer type trees" hypothesis

The user's hypothesis (strip huge dependent-type trees after typer via a plugin)
can only affect phases that run *after* typer: `inlining` (41.7 s), the DFHDL
plugin phases, `posttyper` (4.5 s), pickling, erasure. It **cannot** reduce the
166 s spent in typer, because those type trees are *built and consumed* during
typer itself. So the realistic ceiling for that specific idea is the ~55 s of
post-typer work, and only the fraction of it that is actually type-tree-size
bound. The elephant (typer, 166 s) needs a different lever (macro/inline
architecture, given-search caching, or an upstream compiler fix).

## Method-level breakdown with `-Yprofile-trace` (chrome trace, 67 MB)

Enabled `-Yprofile-trace:.../compiler.trace` alongside `-Yprofile-enabled`,
compiled the 72 test sources (280 s with tracing), and aggregated the 606 k
begin/end events by self-time (exclusive) and bucketed each event into the
compiler phase it fired in.

### Self-time by (phase, category)

```
PHASE typer     : 174 s self   -> inline 67.1s | implicit 65.9s | typecheck 41.0s
PHASE inlining  :  41 s self   -> "inliner machinery" 28.5s | inline-def expansion 10.3s
PHASE CodeDigest:  17 s self   -> all in the phase body (our plugin)
PHASE posttyper :   4 s self
PHASE erasure   :   2 s self
PHASE genBCode  :   2 s self
```

### Hottest named events (whole run, by self-time)

| self | total | count | event |
|-----:|------:|------:|-------|
| 27.8s | 109.5s | 30939 | typecheck `method $anonfun` (typing the many anon blocks/lambdas in designs) |
| 14.0s | 54.6s | 2623 | inline `exactOp2` (the core Exact operation macro) |
| 10.5s | 19.3s | 14336 | implicit `?[type Aux]` (searching `...Aux` type members) |
| 10.2s | 22.4s | 5340 | inline `given_Check_...` (Check constraint given) |
| 9.8s | **144.0s** | 6573 | inline `method <>` (the connect operator; its closure drives 144 s = 55% of the run) |
| 8.4s | 34.8s | 2623 | implicit `?[trait ExactOp2]` (summoning the ExactOp2 typeclass) |
| 7.7s | 44.5s | 3002 | implicit `?[trait TC]` |
| 7.2s | 13.2s | 7699 | inline `given_AssertGiven_G_M` (compile-time assertion given) |
| 5.4s | 25.8s | 5197 | implicit `?[type Check]` |
| 4.9s | 24.5s | 1687 | inline `method conv` |

### What this tells us about the hypothesis

**1. typer (166 s) is unreachable by any post-typer plugin.** Its cost is
implicit/given search (66 s) + transparent-inline & macro expansion (67 s) +
typechecking of anonymous blocks (41 s). All of it happens *during* typing.
No plugin phase (they all run after typer) can remove it. The only levers are:

- reduce the number / cost of transparent-inline given expansions
  (`AssertGiven` 7699x, `Check` 5340x) - e.g. by not expanding them during the
  hot nested implicit-search path;
- reduce `Aux`-member implicit search (14336 searches, the classic slow HK/Aux
  pattern);
- an upstream compiler improvement to implicit search or transparent-inline
  expansion.

**2. The "huge type tree" cost is real but lives in specific post-typer places,
not in passive traversal.** The addressable post-typer surface is:

- **inlining phase, 28.5 s of "inliner machinery"** - the inliner copies each
  inline-def body and *re-types* it; bodies/surrounding trees carrying large
  inferred types make this re-typing and tree-copying slower.
- **CodeDigest, 16.8 s (our own plugin)** - `transformTypeDef` calls
  `tree.show` on every top-level class to hash its code identity. `tree.show`
  pretty-prints the whole typed tree *including its giant inferred type
  annotations* to a string. This is a direct, self-contained instance of "huge
  type trees are expensive", and it is entirely under our control.
- posttyper 4.3 s (already minimized by `FlattenInlinedPhase`).

So the user's instinct is correct in shape: large post-typer type trees do cost
real time - but concentrated in (a) the inliner re-typing bodies and (b) our own
`tree.show` in CodeDigest, rather than in generic tree traversal that a
"strip types" plugin pass would speed up. Blindly stripping/​widening type trees
before `inlining` is unsafe: the inliner needs those types to substitute and
re-type correctly.

## Experiment 1 (SHIPPED): stop pretty-printing types in CodeDigest

**Hypothesis under test:** the huge post-typer inferred types are expensive
specifically because `CodeDigestPhase.transformTypeDef` hashed
`sha256(pluginStamp + "\n" + tree.show)`, and `tree.show` renders the whole
typed tree - *including every inferred dependent type* - to a string, once per
top-level class.

**Change** (`plugin/src/main/scala/plugin/CodeDigestPhase.scala`): replaced the
`tree.show` hash with a single `TreeTraverser` that folds the tree's code
identity straight into the SHA-256 digest - node kind + referenced symbol
full-names + literal constants + the identity-bearing parts of each carried
type (named-type symbols, constants, refinement names, and each type part's
structural kind so `A & B` / `A | B` / `(A, B)` cannot collide). No giant type
strings are ever materialized. It stays position-insensitive (no spans read)
and source-path-free, exactly as the rendering was.

**Result (compiler_stages test compile, 10 G heap, `-Yprofile-enabled`):**

| phase | before | after |
|-------|-------:|------:|
| CodeDigest | 16.8 s | **2.6 s** |
| typer | 166.6 s | 162 s (noise) |
| inlining | 41.7 s | 39.4 s (noise) |
| **total wall** | **258 s** | **~236 s** |

**~14 s off CodeDigest, ~9% off the whole compile**, from a self-contained
change to our own plugin. This directly confirms the user's intuition:
pretty-printing the giant inferred types was pure, avoidable cost.

**Verification done here:** `compiler_stages/Test/compile` clean; all runnable
suites green - `StagesSpec.*` 526/526 (incl. `ClassDesignCacheSpec`,
`ClassDesignKeySpec`, `SubDesignCacheSpec`), `CoreSpec.*` 104/104.

**Verification still owed (maintainer):** the `.dfdigest` `own` hash is the
cross-build *code identity* that keys the elaboration disk cache. The suites
above exercise the design-load gate and sub-design cache, but true cross-build
staleness soundness (change a helper body -> dependents' composed digests must
change) is what `testApps` and multi-run cache scenarios cover, and those need
the external toolchain / `dftools` download that is blocked in this sandbox.
The new hash captures at least as much identifying information as the old
rendering, but please run `testApps` before relying on it in production caching.

## Where the real time is, and what is worth trying next

The 166 s in **typer** is the prize, and no post-typer plugin can touch it.
Ranked by expected value:

1. **Transparent-inline givens expanded during typer.** `AssertGiven` (7699
   expansions, 13 s) and `Check` (5340, 22 s) are `transparent inline given`s
   whose macros run inside the hot, deeply-nested implicit-search path. If any
   of them do not actually need type narrowing (their declared result type is
   fixed, e.g. `AssertGiven[G, M]`), dropping `transparent` defers expansion to
   the flat `inlining` phase. CAUTION: `ControlledMacroError` +
   `DualSummonTrapError` suggest some of these errors are *trapped during
   summon*, which would make their `transparent`-ness load-bearing - each given
   must be checked individually. Not attempted here to stay within the safe,
   verifiable envelope.
2. **`Aux`-member implicit search** (14336 searches, 19 s). The classic slow
   HK/`Aux` pattern. Restructuring `ExactOp*Aux` to avoid the `Aux`-member
   summon, or caching, could help - but it is an architectural change to
   `Exact.scala`.
3. **`inlining` phase, 28.5 s of inliner machinery** re-typing inline-def
   bodies. A safe reduction would come from fewer / smaller non-transparent
   inline bodies on the hot ops (`fromValue`, `conv`, `assertCodeString`).
4. **Upstream Scala 3 change.** The implicit-search and transparent-inline
   expansion costs are partly compiler-side. A minimal reproducer (a chain of
   `Aux`-summoning transparent-inline givens) could motivate an upstream
   improvement, mirroring the earlier PostTyper `minimizeCall` finding. The
   scala3 fork is available for prototyping if pursued.



