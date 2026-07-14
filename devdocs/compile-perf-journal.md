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
