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

