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

## Experiment 2 (REVERTED): `summonFrom`-based AssertGiven

**Hypothesis under test (maintainer suggestion):** `AssertGiven` is one of the
hottest typer expansions (7699 expansions, 13.2 s total, 7.2 s self). It is a
`transparent inline given` backed by a quotes macro. Replacing the macro with
`compiletime.summonFrom` would resolve it as a compiler intrinsic during typing
and skip the per-expansion quotes/reflection overhead.

**Semantics probe (cheap, via `corePlayground`).** Before touching `internals`,
a standalone probe confirmed `summonFrom { case _: G => ... ; case _ => error }`
reproduces AssertGiven's meaning for every real usage shape:

- single type `G`: matches when a given for `G` exists;
- union `G = A | B | ...`: matches when ANY arm's given exists - implicit search
  for a union already accepts a given conforming to any arm (`A <: A | B`), and
  this held for `=:=`, `<:<` and `util.NotGiven` arms (the arms are checked
  separately, just as the macro's `recur` does);
- no arm holds: fails at the summon site with the exact `M` message.

So in isolation the replacement is behavior-equivalent.

**Why it was REVERTED.** Under the full build it breaks `==` (and by the same
mechanism `<>`):

```
PrintCodeStringSpec.scala:840  if (cnt == HALF_PERIOD - 1)
    Cannot implicitly convert to DFHDL Int type.
```

`==` lowers to `specialCompare`, which summons
`DualSummonTrapError[Compare[...], Compare[...]]`. The trap **activates
`ControlledMacroError`**, runs `Implicits.search` on each arm, and reads back
`getLastMacroAbortError`. `AssertGiven` is summoned *inside* that trapped
search, and its behaviour must be **context-sensitive**:

- **not trapped** (normal use): return `compiletime.error(M)` - the given
  materializes carrying a deferred error, so the user sees the custom `M`
  message at the use site;
- **trapped**: `errorAndAbort` - *fail the implicit search* so the trap detects
  it (as a `PriorityError` carrying `M`) and can fall back to the other arm.

`ControlledMacroError.report` chooses between these by inspecting whether error
control is active. `summonFrom` cannot: a `summonFrom` given ALWAYS
materializes, so inside the trap it looked like a spurious success, the trap
picked a wrong arm, and the comparison failed to resolve. A `using G` variant
(fail the search unconditionally) fixes the trap case but throws away the custom
`M` message everywhere, which is the whole point of `AssertGiven`. Only a
context-sensitive macro can satisfy both, so the macro stays.

Conclusion: like dropping `transparent`, this lever is blocked by the
`DualSummonTrapError` trap that `AssertGiven` participates in. Recorded in the
`AssertGiven` source as a "do not retry" note. (A theoretical partial win
remains - a fast `summonFrom` variant for the AssertGiven usages that are
*provably never* reached inside a trap, e.g. the scope/domain/init checks on
user-facing methods, keeping the macro only on the `Compare`/`TC_Connect`
closure - but classifying usages by trap-reachability is fragile and was not
pursued.)

### Experiment 2b (measurement only): flip `transparent` off, keep the macro

To answer "is it even faster if we ignore the failures?", the given was changed
from `transparent inline given` to plain `inline given` (macro unchanged),
profiler on, full clean rebuild. It still breaks the same 3 `==` sites (the trap
again), but the phase profile is the point:

| phase | `transparent` (baseline) | plain `inline` |
|-------|-------------------------:|---------------:|
| typer | 162 s | **155 s** (-7 s) |
| inlining | 39 s | **63.5 s** (+24.5 s) |

**It is NOT faster - it is slower.** Making the given non-transparent only
*moves* the 7699 macro expansions out of typer and into the `inlining` phase,
where they cost ~3x more (and the +24.5 s is understated: the 3 failing units
error out in typer and never reach inlining, so inlining did more work on fewer
files). Net regression of ~17 s across the two phases, on top of the broken
trap. Reverted.

Takeaway: the AssertGiven expansion cost is not removable by relocating it
between phases; it is intrinsic to running the check at all. The only real wins
would reduce how OFTEN it runs (fewer expansions) or make each check cheaper
without a macro - and the trap forbids the non-macro route.

## Experiment 3: WHY are there so many AssertGiven / Check expansions?

7699 AssertGiven and ~5980 Check expansions is far more than the number of
checks written in the 72 test files. To find out why, `AssertGiven.macroImpl`,
`Check1.checkMacro` and `Check2.checkMacro` were instrumented to print, on every
invocation, the macro-expansion source position **and** the type actually being
checked (`System.err.println`, reverted after). Then a minimal design was
compiled via `corePlayground` (recompiles only `Playground.scala`, ~1 s).

### Finding 1: many DISTINCT checks per operation (not re-runs)

For a single `y := a + a`:

```
[TRACE-AG] line5 G=DFC.Scope.HasAssign | NotGiven[...]        (assignment scope)
[TRACE-AG] line5 G=scala.<:<[Modifier.PortOUT & ...]          (port direction)
[TRACE-AG] line5 G=scala.=:=[DFXInt.Val.Candidate...]         (operand kind)
[TRACE-AG] line5 G=NotGiven[<:<[Modifier.PortO...]]           (modifier)
[TRACE-CK2] line5 cond=[BaS.. WcS..]   [BaW.. WcW..]   [LS.. RS..]   [LW.. RW..]
```

The 4 AssertGivens and 4 Check2s are all **different** checks, each firing
**once**. So a large part of the count is simply that every DFHDL operation
carries a fixed handful of independent type-level checks (domain/scope/port +
several width/sign conditions). That part is not waste, it is the checks the
language performs.

### Finding 2: chained operations RE-EXPAND inner checks (the waste)

Compiling `y := a + a + ... (N additions)` and counting the same check's repeats:

| N additions | total Check2 expansions | max repeats of one check |
|------------:|------------------------:|-------------------------:|
| 2 | 6 | 2 |
| 4 | 10 | 4 |
| 6 | 14 | 6 |

- AssertGiven on the `:=` stays at 4 total for ANY N: top-level checks fire once.
- But an inner width check re-expands **once per enclosing operation** (max
  repeats == N). Total Check2 grows ~`2N + 2`.

**Mechanism.** `+` (and every op) is `transparent inline` with an `inline lhs`.
`Check1.apply` is `inline def apply(arg) = compiletime.summonInline[Check[..]]`,
so the expanded body of `(a + a)` still CONTAINS an unresolved
`summonInline[Check[..]]`. When `(a + a)` is spliced in as the `inline lhs` of
the next `+` and that outer transparent-inline is expanded/re-typed, the inner
`summonInline` is resolved AGAIN, re-running `checkMacro`. Every extra level of
nesting re-resolves the checks beneath it. The growth here is linear per check
(the total stays ~`2N+2`, not `N^2` - the post-typer `N^2` blowup was already
killed by `FlattenInlinedPhase`), but it is still a 2x-plus multiplier over the
minimum, applied to every chained expression in the suite. Combined with
`DualSummonTrapError` searching BOTH arms (another 2x for everything reached
through `==`/`<>`), this is what turns a few hundred written checks into
thousands of expansions.

### What would actually reduce it

The cost is re-resolution of `summonInline[Check[..]]` (and the trap's
double search) that sit INSIDE transparent-inline bodies which get
re-expanded. Levers, in order of leverage vs. risk:

1. **Compiler-side memoization of transparent-inline / macro expansion**
   (the "trace the compiler" outcome). The compiler re-expands an identical
   transparent-inline application (same symbol + same type args + same
   argument trees) every time it re-appears as an inline argument, instead of
   caching the first expansion. Memoizing by (symbol, targs, arg-tree identity)
   during typer would collapse the per-level re-runs to one. This is the
   highest-leverage fix and matches the earlier PostTyper `minimizeCall`
   pattern (a compiler doing discardable repeated work). It needs prototyping in
   the scala3 fork and careful correctness review (macros may be effectful; a
   memo must key on everything the expansion depends on).
2. **Resolve the check once, at its own level.** If `Check1.apply` did not leave
   a `summonInline[Check[..]]` in the expanded body but instead forced the check
   to a already-resolved no-op (`CheckOK`) that carries no further summon, the
   outer re-expansions would have nothing to re-resolve. This is a DFHDL-side
   restructure of `Check1`/`Check2` and needs the maintainer (it interacts with
   the runtime-fallback `CheckNUB` path).
3. **Halve the trap cost.** `DualSummonTrapError` searches both arms even when
   the first already succeeded; the inner checks re-run for the second arm.
   Short-circuiting when the first arm fully succeeds (where the two-directional
   fallback is not needed) would remove a 2x on the `==`/`<>` closure.

## Where the real time is, and what is worth trying next

The 166 s in **typer** is the prize, and no post-typer plugin can touch it.
Ranked by expected value:

1. ~~**Transparent-inline givens expanded during typer.**~~ **RULED OUT by the
   maintainer (2026-07): dropping `transparent` will not work.** These givens
   rely on transparent expansion during typing (result-type narrowing and/or
   the `ControlledMacroError`/`DualSummonTrapError` summon-time trap), so the
   modifier is load-bearing. Do not revisit.
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



