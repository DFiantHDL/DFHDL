# Compile-Time Performance Journal (compiler_stages tests)

> Running log of experiments to reduce the time to compile `compiler_stages`'
> test sources, which exercise the DFHDL compiler plugin and compile-time
> checks the hardest. Append-only; newest section at the bottom.

## TL;DR (start here)

**Baseline:** ~258 s to compile the 72 `compiler_stages` test sources (10 GB
heap; the default 1 GB OOMs - see Environment).

**Three shipped, verified wins - together ~258 s -> ~217 s (~16%), all on 3.8.4:**
1. **CodeDigest** (Experiment 1): `CodeDigestPhase` hashed `tree.show` on every
   top-level class, rendering the giant inferred types to text (~17 s). Replaced
   with a structural digest folded in one traversal -> CodeDigest **16.8 s ->
   2.6 s**. **Owed:** run `testApps` to confirm cross-build `.dfdigest` cache
   soundness (needs the external toolchain, not runnable here).
2. **`Check` fast path** (Experiment 7): a statically-true `ok` given returns
   `CheckOK` without running `checkMacro` -> **typer 162 s -> 146 s (-16 s)**.
   Works because the macro's true-branch was a genuine no-op.
3. **`Check ok` made non-inline** (Experiment 9): the fast-path `ok` givens on
   `Check1`/`Check2` were `inline given` but take no inline params, so `inline`
   only forced inline-expansion of `CheckOK.asInstanceOf[Check[...]]` (with its
   full Check type arg) at every static check site during the `inlining` phase
   (2043 expansions in the trace). Plain givens leave one `ok[...]` ref instead
   -> **inlining 45.0 s -> 39.6 s (-5.3 s, -12%; -185 MB allocated)**, typer
   unchanged. `CheckNUB.ok` stays inline (it has inline params).
All verified with `StagesSpec` 526/526 + `CoreSpec` 104/104.

**BIG compiler-side win (Experiment 12): `TyperState.commit` O(n^2) no-op merge.**
A JFR profile of the compile showed **~39% of ALL compiler execution samples** in
one leaf: `SimpleIdentityMap.apply`, under `TyperState.nestingLevel` <-
`TyperState.commit` <- `tryEither`. `commit` merges type-variable nesting levels
by iterating `upLevels` and doing a linear-scan lookup+update per binding = O(n^2)
in level-lowered type vars; DFHDL forks/commits huge numbers of typer states (inline
re-typing, implicit search) each carrying many such vars. But a forked state shares
`upLevels` by reference, so when the speculative typing lowered no level the whole
merge is a **no-op** - which is the common case. A one-line reference-equality guard
(`if upLevels ne targetState.upLevels`) skips it. Measured on the patched compiler:
the hotspot drops **39% -> 1.5%**, total compiler samples **-44%**, and the
`compiler_stages` test compile **242 s -> 164 s (-32%)**, `StagesSpec` 526/526 +
`CoreSpec` 104/104 still passing. Patch is on the scala3 fork
(`soronpo/scala3` @ `claude/dfhdl-compiler-perf-wbtdaf`), one commit,
upstreamable. The fix is identical in 3.8.4 and 3.10 (same code, same 39% hotspot),
so it transfers; validated on 3.10 because that fork was already built (see
Experiment 12 for the `BitNumWrapper` genBCode workaround that unblocked it).

**SECOND compiler-side win (Experiment 13): redundant inline dependency
extraction.** With `TyperState.commit` fixed, the next JFR showed zinc dependency
extraction at **23% of samples**, 16.7% of it in the `Inlining` phase. `Inlining`
runs `inlineFinder` on EVERY transformed node, so each `Inlined` subtree's (huge)
types are re-traversed for dependency recording once per non-`Inlined` ancestor -
O(depth) redundant. Dependency recording is idempotent, so collecting each
`Inlined` once by identity is equivalent: dep-extract **23% -> 5.6%**, total
compiler samples a further **-19%**. Passes `StagesSpec`/`CoreSpec` AND the full
scala3 `testCompilation` corpus (0 failed). Separate branch/PR:
`soronpo/scala3` @ `claude/inlining-dep-dedup`.

**Phase split before the compiler wins:** `typer` ~146 s, `inlining` ~40 s,
everything else small. Both `TyperState.commit` and the inline dependency
extraction were inside those numbers.

**Total, `compiler_stages/Test/compile` wall-clock:** ~258 s (original) -> ~217 s
with the three library/plugin wins (on 3.8.4, live today, -16%). The two compiler
patches cut a further ~38% (242 s -> ~150 s measured on 3.10, transfers to 3.8.4),
landing the compile around ~140-150 s once the upstream PRs merge - roughly half
the original.

**Key correction (Experiment 5):** the high macro-expansion counts (7699
AssertGiven, ~6000 Check) are per-operation **VOLUME**, not redundant
re-expansion (`a+a+...(N)` -> `2N+2` checks, LINEAR not quadratic). So there is
nothing to "memoize"; the only lever is fewer/cheaper checks per op. The
`Check` fast path (win 2) is the cheaper-check direction and is why it worked.

**Dead ends (measured, don't repeat):** `summonFrom` AssertGiven and a
`compiletime.error` fallback (both break the `DualSummonTrapError` trap);
non-transparent AssertGiven/Check (moves cost typer->inlining, net SLOWER);
output-tree seal in `exactOp2` (no effect); `exactOp2` try/catch workaround
(fires 0x here); short-circuiting `DualSummonTrapError` (second arm is the
runtime connect fallback); **`ORGIVEN` + fast-path AssertGiven (Experiment 8):
correct but ~2 s WORSE - the assert's summon work is irreducible and the
`NotGiven` guard needed to prefer the fast path costs a second summon**;
**`ControlledMacroError` TrieMap -> plain `mutable.Map` (Experiment 10): typer
UNCHANGED (within noise) - the map ops are not a hot fraction of typer, and
`TrieMap` is load-bearing for sbt's parallel in-JVM compilation, so reverted.**

**Newer compilers (Experiment 6):** DFHDL `internals`/`plugin`/`compiler_ir`
compile clean on Scala 3.10.0-RC1 (plugin is forward-compatible), but `core`
hits a genBCode `Integer`-vs-`int` backend assertion on BOTH 3.9.0-RC1 and
3.10.0-RC1 - a real incompatibility to bisect. So DFHDL stays on 3.8.4; the wins
above are pure library/plugin code and transfer forward.

## Build reference: trying the latest compiler (Scala 3.10) - CONCLUDED

Outcome is summarized in the TL;DR and Experiment 6 (plugin forward-compatible;
`core` blocked by a genBCode `Integer`-vs-`int` regression on 3.9/3.10). The
build mechanics below are kept for anyone repeating the bump.

Rationale: the fork HEAD carries recent inlining work ("Enhance constant-folding
during inlining"), so bumping DFHDL off 3.8.4 might itself cut compile time,
independent of any custom patch. Also a prerequisite for prototyping compiler
-side expansion memoization.

Build notes:
- The scala3 fork (`soronpo/scala3` @ `claude/dfhdl-compiler-perf-wbtdaf`)
  builds and `publishLocal`s as **`3.10.0-RC1-bin-SNAPSHOT`**. One build blocker
  was fixed and pushed to the fork: the scaladoc step downloads `inkuire.js` from
  GitHub, which 403s offline and stalls `publishLocal`; it is now gated behind
  `SKIP_INKUIRE_FETCH=1` (commit on the fork branch). Publish command that works:
  ```
  cd scala3 && SKIP_INKUIRE_FETCH=1 sbt \
    scala3-interfaces/publishLocal tasty-core-bootstrapped/publishLocal \
    scala-library-bootstrapped/publishLocal scala3-library-bootstrapped/publishLocal \
    scala3-sbt-bridge-bootstrapped/publishLocal scala3-compiler-bootstrapped/publishLocal
  ```
  (Do NOT use `set every .../packageDoc/publishArtifact := false` - it cascades
  and drops the main jars, publishing only ivy.xml.)
- Next: bump DFHDL `compilerVersion` to `3.10.0-RC1-bin-SNAPSHOT` and compile
  `internals -> plugin -> compiler_ir -> core -> compiler_stages`. The plugin
  (~6 k lines on `dotty.tools.dotc` internals) is the migration risk across two
  major versions; expect API breakage to fix. Kept UNCOMMITTED / off `performance`
  until it compiles and `CoreSpec`/`StagesSpec` pass, so the branch stays green on
  the shipped CodeDigest win.

Caveat on expected payoff: the re-expansion thesis behind a memoization patch is
weaker than first stated. `AssertGiven` expansions stay CONSTANT with chain
length (4 per assignment regardless of RHS size), so a large part of the ~13 k
check/assert expansions is per-operation VOLUME, not redundant re-expansion. The
`Cond`-lambda-signature trace cannot distinguish "same check re-expanded" from
"N distinct checks sharing a lambda signature"; a decisive test must print the
APPLIED operand types. So the primary hope from 3.10 is the compiler's own
inlining improvements, not a custom memoization.

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

## Experiment 4 (REVERTED): seal the operand result behind a val

The Exact mechanism was the prime suspect for the re-expansion: `exactOp2Macro`
calls `flattenInlined`, which HOISTS the operand's inner bindings (its resolved
checks) into one flat `Block`, so an enclosing op re-processes them. Since
`ExactInfo.exactTpe` derives the exact type from `term.tpe` (widened), not from
the expanded tree, the plan was: bind the whole op result behind a `val` typed
at its precise exact type and return a reference to it, so an enclosing op reads
the exact type off the val ref instead of re-descending into the checks.

Implemented in `exactOp2Macro.buildFlattened`:
`ValDef.let(spliceOwner, fullResult)(ref => ref)`.

**Result: no change.** The `checkMacro` re-run count on `y := a + a + ... (N)`
stayed exactly `2N + 2` (N=2 -> 6, N=4 -> 10, N=6 -> 14), identical to baseline.

**Interpretation.** Sealing the macro's OUTPUT tree does not reduce the
re-expansion, so the repetition is NOT the enclosing op re-descending into the
operand's output and re-resolving its checks. It happens one level lower: the
COMPILER re-expands the nested `transparent inline` operator calls themselves
(and/or re-runs the `ExactOp2` summon), independent of what tree each expansion
returns. No restructuring of the macro output can change how many times the
compiler expands `a + a`. Reverted.

This narrows the fix to two places, neither of which is the macro's output tree:

- **Compiler-side memoization of transparent-inline expansion / macro results**
  keyed by (symbol, type args, argument-tree identity). This is the direct fix
  for "the compiler re-expands the same nested transparent inline N times" and
  remains the highest-leverage lever. Prototype in the scala3 fork.
- **Fewer nested transparent-inline levels** in the operator encoding (an
  architectural change to how `exactOp*` chains compose) so there is less to
  re-expand. Maintainer-side; interacts with the exact-type threading that the
  `inline` operands exist to provide.

Note also the two constant-factor multipliers found along the way, each cheaper
to attack than the compiler change:
- the `try/catch` "Scala compiler bug" workaround in `exactOp2Macro` re-runs the
  ENTIRE `ExactOp2` summon (and thus its checks) whenever the first summon
  throws - worth measuring how often it fires;
- `DualSummonTrapError` searches both arms, re-running the inner checks for the
  second arm on every `==`/`<>` (a 2x on that closure), removable by
  short-circuiting when the first arm fully succeeds.

## CORRECTION (Experiment 5): it is check VOLUME, not re-expansion

Experiments 3-4 suggested chained ops re-expand inner checks. A sharper
measurement refutes the *redundant* part of that. `Check2.checkMacro` was
instrumented to print the APPLIED operand types (`T1`, `T2`, `CondValue`) - not
the `Cond` lambda signature, which is identical across all arithmetic checks and
was the source of the earlier ambiguity. Compiling `y := a + a + ... (N)`:

| check (by applied types) | N=1 | N=2 | N=3 |
|--------------------------|----:|----:|----:|
| `T1=true T2=true` (sign) | 2 | 3 | 4  (= N+1) |
| `T1=UBound...` (width)   | 1 | 2 | 3  (= N)   |
| `T1=32 T2=16` (assign)   | 1 | 1 | 1          |
| **total**                | 4 | 6 | 8  (= 2N+2)|

The total is **linear** in N. If inner checks truly re-expanded once per
enclosing level (the Experiment-3/4 hypothesis), the innermost check would fire
N times, the next N-1, etc. - a **quadratic** total. It is linear, so each `+`
contributes exactly one sign + one width check and the `:=` adds its two. **The
high macro-expansion counts (7699 AssertGiven, ~6000 Check across the suite) are
inherent per-operation VOLUME, not redundant re-expansion.**

Consequences:
- The compiler-side "memoize identical transparent-inline expansions" idea does
  NOT apply: there are no identical re-expansions to collapse. This retroactively
  explains why the Experiment-4 output-seal changed nothing.
- Reducing typer time requires FEWER or CHEAPER checks/resolutions per operation
  (architectural), not de-duplication. Candidates (all maintainer-side, higher
  risk): combine the per-op sign+width `Check`s into one; reduce the `Aux`-member
  implicit search (14336 searches / 19 s); or halve the `DualSummonTrapError`
  double-arm search that doubles every `==`/`<>` (the dominant `<>` closure is
  144 s of the run) - but its second arm feeds the runtime connect fallback, so
  it is not safe to drop blindly.

## Experiment 6 (build spike): DFHDL on newer compilers (3.9 / 3.10)

The maintainer OK'd bumping off 3.8.4. Built the `soronpo/scala3` fork
(`3.10.0-RC1`) locally: `publishLocal` of the compiler chain. Two build-tooling
notes for anyone repeating this in the sandbox:
- The scaladoc step downloads `inkuire.js` from a GitHub *release* (403 through
  the proxy). `raw.githubusercontent.com` is reachable but the asset is not a
  repo file. Patched `project/DocumentationWebsite.scala` to skip the fetch under
  `SKIP_INKUIRE_FETCH=1` (committed on the fork branch); the empty stub only
  disables doc *search*, the bin jars publish fine.
- The compiler chain needs `scala3-interfaces`, `tasty-core`, `scala-library`,
  `scala3-library`, `scala3-sbt-bridge`, `scala3-compiler`, plus transitively
  `scala3-directives-parser`, `scaladoc`, `scala3-tasty-inspector`. Easiest is
  `SKIP_INKUIRE_FETCH=1 sbt scala3-bootstrapped/publishLocal`. Do NOT compile
  DFHDL while that publish runs - the compiler jar is rewritten mid-flight and
  DFHDL's compile dies with a spurious `NoClassDefFoundError`.

**Migration result (uncommitted, DFHDL kept on 3.8.4):**
- `internals` (macros/quotes), `plugin` (6 k lines on `dotty.tools.dotc`
  internals), and `compiler_ir` all compile CLEAN on 3.10.0-RC1. **The DFHDL
  plugin is forward-compatible** with the newer compiler - a useful, non-obvious
  result.
- `core` does NOT compile: the JVM backend crashes with
  `AssertionError: Cannot emit primitive conversion from Integer to I`
  (3.10.0-RC1) / `Cannot compute maxType: Integer, I` (3.9.0-RC1). Same
  `Integer`-vs-`int` backend assertion on BOTH newer versions - a genBCode
  incompatibility with some boxed-`Integer`-where-`int` pattern in `core` that
  3.8.4 tolerated. No source position (backend crash); pinpointing needs a
  bisect the maintainer is best placed to do. This blocks measuring whether the
  newer inlining helps, so we stay on 3.8.4 (where a fix should transfer forward
  anyway).

## Experiment 7 (SHIPPED): statically-true fast path for `Check`

Since the cost is per-operation check VOLUME (Experiment 5), the lever is making
each check cheaper. `Check1`/`Check2` had a single `transparent inline given`
that always ran `checkMacro`, even when the condition holds statically (the
common case: matching widths/signs). `checkMacro`'s statically-true branch just
returns `CheckOK`, but the macro splice runs regardless.

Added a higher-priority fast-path given (mirroring the existing `CheckNUB.ok`)
with `CondValue` fixed to `true`:

```scala
inline given ok[Wide, T <: Wide, Cond[..], Msg[..], MsgValue <: String, Warn <: Boolean]
    : Check[Wide, T, Cond, Msg, true, MsgValue, Warn] =
  CheckOK.asInstanceOf[Check[Wide, T, Cond, Msg, true, MsgValue, Warn]]
// general macro given renamed `fromMacro`, unchanged
```

Because `CondValue` is fixed to `true`, this given is more specific than the
general `fromMacro` and is preferred when the condition is statically satisfied,
so those checks skip the `checkMacro` splice entirely. When `CondValue` is
`false` or abstract, `ok` does not match and `fromMacro` runs exactly as before
(compile error / runtime check preserved). No trait-priority gymnastics were
needed - the compiler picks `ok` by specificity with zero ambiguity.

**Result (compiler_stages test compile, `-Yprofile-enabled`):**

| phase | before | after |
|-------|-------:|------:|
| typer | 162 s | **146 s** (-16 s) |
| inlining | 40 s | 44 s (+4 s; the `ok` bodies expand there) |
| **net** | | **~-12 s** |

Verified: `StagesSpec` 526/526, `CoreSpec` 104/104 (twice). Safe by
construction - it only shortcuts the provably-satisfied case, which was already a
`CheckOK` no-op. This is transferable to future compiler versions (it is pure
DFHDL library code). Combined with the CodeDigest fix, the whole compile is now
~258 s -> ~222 s.

## Experiment 8 (REVERTED): fast-path `AssertGiven` via `ORGIVEN`

Same idea as Experiment 7, applied to the other hot given (`AssertGiven`, 7699
expansions). Introduced `infix trait ORGIVEN[L, R]` (summonable iff a given for
`L` OR `R` is, decided by two givens `fromL`/`fromR`) to replace the bare `A | B`
inside `AssertGiven` applications, then split `AssertGiven` into a fast-path
given `ok[G, M](using G): AssertGiven[G, M] = Success` plus a fallback for the
failure case. Migrated all 7 `|` unions in `AssertGiven` usages to `ORGIVEN`.

**It works but does NOT help - reverted.** typer **148 s vs the 146 s
Experiment-7 baseline** (~2 s WORSE); `StagesSpec` 526/526, `CoreSpec` 104/104.

Why it cannot win:
- The `AssertGiven` macro's real cost is the `summon[G]` that decides whether the
  assertion holds. The `ok` fast path pays exactly that same summon (`using G`),
  so bypassing the macro splice saves almost nothing. Unlike `Check` (Experiment
  7), where the statically-true branch was a genuine no-op, here there is no
  skippable work.
- The fallback CANNOT be a plain `compiletime.error` given (the maintainer's
  first suggestion): that materializes on failure, so inside a
  `DualSummonTrapError` search the trap sees a spurious success and `==`/`<>`
  break (identical failure to Experiment 2). The fallback must stay a
  `ControlledMacroError` macro.
- To make `ok` reliably beat the macro fallback, they must be mutually exclusive
  via `using NotGiven[G]` on the fallback (Scala's specificity otherwise prefers
  the macro; without the guard the macro is picked for HOLDING assertions and
  falsely reports the error - 322 errors). But `NotGiven[G]` internally re-checks
  `G`, so every assertion now does TWO summons instead of the macro's one - which
  is the +2 s. A low-priority trait for the fallback (no `NotGiven`) is not an
  option: a macro given in a trait fails with "inline accessor not statically
  accessible".

Takeaway: the `Check` fast path was a real win only because its common case was
provably a no-op; the `AssertGiven` check is irreducible summon work, so the same
structural trick regresses. Left reverted.

## Experiment 9 (SHIPPED): `Check ok` fast-path made non-inline

Hypothesis (from profiling the `inlining` phase, not just typer): the huge
`Check` type trees and inline residue that flow past typer make `inlining`
(~44 s) slower than it needs to be. Method: `-Yprofile-trace` on the full
`compiler_stages` test compile, then aggregate self-time of events occurring
INSIDE the `inlining` phase span.

What the trace showed about `inlining` (40.9 s total):
- **28.5 s** generic inliner traversal / re-typing of inline-def bodies (the
  file-level self-time). This is node-count bound; prior experiments showed type
  complexity alone does not move it.
- **12.4 s** named inline expansions. Of these only **1.7 s** is Check-related,
  essentially all of it the **`ok` given inlined 2043x (1.6 s)**.

So Check-type erasure per se was not the lever (Check is a small slice of
inlining, and typer - where the checks are actually resolved - is upstream and
untouched by any post-typer rewrite). But the trace pinpointed a concrete,
free win: the fast-path `ok` givens (Experiment 7) were declared `inline given`
yet take **no inline params**. `inline` bought nothing at the type level
(specificity alone makes them the preferred given) and only forced the compiler
to inline-expand `CheckOK.asInstanceOf[Check[bigtype]]` at each of the thousands
of static check sites during `inlining`. Making them plain `given`s leaves a
single `ok[...]` reference.

Measured on `compiler_stages/Test/compile` (same JVM, clean rebuild each side):

| phase    | `inline given ok` | plain `given ok` | delta        |
|----------|-------------------|------------------|--------------|
| typer    | 148.7 s           | 145.6 s          | ~noise       |
| inlining | **44.95 s**       | **39.6 s**       | **-5.3 s**   |
| inlining allocated | 1.59 GB | 1.41 GB         | -185 MB      |

`StagesSpec` 526/526 + `CoreSpec` 104/104 pass (error/warn paths are unaffected -
those resolve via `fromMacro`, unchanged). `CheckNUB.ok` keeps `inline` because
it genuinely has inline params. Shipped.

Generalizable takeaway: an `inline given`/`inline def` with no inline params and
no need for transparent result narrowing is pure inlining-phase cost. Auditing
the other hot inline defs the trace named (`fromValue`, `conv`, `generate`) for
the same property is the natural follow-up (they may legitimately need `inline`,
but it is worth checking one by one).

## Experiment 10 (REVERTED): `ControlledMacroError` TrieMap -> plain Map

Hypothesis (maintainer): the two `collection.concurrent.TrieMap`s that back the
error-control trap were made concurrent on the assumption that different compiler
threads share the macro context; if they do not, a plain `mutable.Map` would be
cheaper. Changed both to `collection.mutable.Map`, clean rebuild, measured.

Result: **typer 148.9 s vs 145.6-148.7 s across the other runs - unchanged
within noise.** The map operations (a handful per error-controlled given search)
are not a hot fraction of typer; macro tree-building dominates. `StagesSpec`
526/526 still pass (the mechanism works single-threaded).

Reverted, because the change is perf-neutral AND `TrieMap` is load-bearing:
sbt runs compile tasks in parallel within one JVM, and `ControlledMacroError`
is a static singleton whose class can be shared across concurrent compiler runs
on the same classpath. A plain `HashMap` under concurrent structural mutation can
corrupt or throw (a compiler crash); `TrieMap` is safe. No upside, real downside.

## Experiment 11 (REVERTED): generalizing the non-inline rule to the `as*` casts

Following Experiment 9's rule ("an inline def with no inline params and no
transparent narrowing is pure inlining-phase cost"), audited the other hot inline
expansions the trace named inside `inlining`:
- `fromValue`, `conv` (Exact.scala, DFVal.scala): `transparent inline` with an
  `inline value` param - fully load-bearing, NOT candidates.
- `generate`: not a library inline def (test/elaboration side).
- the `as*` cast family (`asValOf`/`asValTP`/`asConstOf`/... on `DFValAny`):
  plain `inline def`, no inline params, body is a bare `asInstanceOf` with an
  explicit return type. Looked exactly like `ok`. Tried making the pure-cast
  block non-inline.

Result: **core + compiler_stages compiled, but StagesSpec FAILED** -
`DropLocalDclsSpec` codegen diffs and `GlobalizePortVectorParamsSpec` threw
`NoSuchElementException: key not found: Const(... DFVal.scala:175 ...)`. The key
is the giveaway: with the cast inlined, the meta-context source position planted
during elaboration is the USER call site; as a plain method it collapses to the
cast's definition site inside `DFVal.scala`. The DFHDL plugin's position/naming
tracking (MetaContextPlacer/Gen) depends on these casts being inlined.

Takeaway (sharpens Experiment 9's rule): `inline` on a value-path def can be
load-bearing for **meta-context position propagation** even when it has no inline
params and no transparent narrowing. `ok` was safe only because its result is a
leaf singleton (`CheckOK`) that carries no position and that no plugin phase
inspects. Any inline def that produces or wraps a `DFVal` is off-limits. Reverted.

## Experiment 12 (SHIPPED to scala3 fork): `TyperState.commit` O(n^2) merge

The biggest single win found so far, and the first on the compiler side. Method:
attach JFR (`jcmd <sbt-pid> JFR.start settings=profile`) to the sbt server during
`compiler_stages/Test/compile`, dump, and aggregate execution-sample leaf frames.

Result: **~39% of all compiler execution samples** sit in one leaf,
`dotty.tools.dotc.util.SimpleIdentityMap$MapMore.apply`. Walking the stacks up:
```
SimpleIdentityMap.apply   (linear scan of a persistent identity map)
  <- TyperState.nestingLevel(tv)
  <- TyperState.commit  (the `upLevels.foreachBinding` loop)
  <- Typer.tryEither
  <- Inliner$InlineTyper.typedApply / Applications.typedApply / Implicits.inferImplicitArg
```

`TyperState.commit` merges the committing state's type-variable nesting levels into
the target state:
```scala
upLevels.foreachBinding { (tv, level) =>
  if level < targetState.nestingLevel(tv) then   // linear-scan lookup, O(|target.upLevels|)
    targetState.setNestingLevel(tv, level)        // linear-scan + array copy, O(|target.upLevels|)
}
```
`upLevels` is a `SimpleIdentityMap[TypeVar, Integer]` whose `MapMore.apply` is a
linear array scan, so the loop is O(n^2) in the number of level-lowered type vars.
DFHDL carries MANY of them (deeply nested inline scopes) and forks/commits enormous
numbers of typer states (every `tryEither` for overload resolution, implicit search
and inline re-typing), so this O(n^2) merge dominates.

Key observation: `fresh` copies `upLevels` BY REFERENCE (`ts.upLevels = upLevels`),
so a forked state that lowered no nesting level commits back with
`upLevels eq targetState.upLevels`. Then every binding already holds the target's
level (`level < nestingLevel(tv)` is always false) and the entire loop is a no-op -
yet it still runs the full O(n^2) rescan. That is the 39%. Fix (one line):
```scala
if upLevels ne targetState.upLevels then
  upLevels.foreachBinding { ... }
```

Measured (patched vs unpatched compiler, same DFHDL sources, full test compile):

| metric                              | baseline | patched | delta        |
|-------------------------------------|----------|---------|--------------|
| `SimpleIdentityMap.apply` samples   | 39.2%    | 1.5%    | gone         |
| total compiler execution samples    | 15428    | 8598    | **-44%**     |
| `compiler_stages/Test/compile` wall | 242 s    | 164 s   | **-78 s (-32%)** |

`StagesSpec` 526/526 + `CoreSpec` 104/104 pass on the patched compiler - the skipped
work is a proven no-op, so output is identical.

How it was validated (and why on 3.10): building a patched 3.8.4 from scratch is
expensive, but the fork was already built/published as `3.10.0-RC1-bin-SNAPSHOT`
(Experiment 6). DFHDL's `core` had been blocked on 3.9/3.10 by a genBCode
`Integer`-vs-`int` assertion; the maintainer's workaround is to **drop `extends
AnyVal` from `BitNumWrapper`** (`core/.../DFBoolOrBit.scala`), after which `core`
compiles on 3.10. With that, DFHDL builds on the fork, and the patch (byte-identical
between 3.8.4 and 3.10, hitting the same 39% hotspot on both) was validated there.
The patch is one commit on `soronpo/scala3` @ `claude/dfhdl-compiler-perf-wbtdaf`,
suitable for an upstream PR; DFHDL adopts it for real once it lands in a release.

Reproduce: in scala3 fork edit `TyperState.commit`, `SKIP_INKUIRE_FETCH=1 sbt
scala3-compiler-bootstrapped/publishLocal`; in DFHDL set `compilerVersion =
"3.10.0-RC1-bin-SNAPSHOT"`, drop the `BitNumWrapper` `AnyVal`, `sbtn shutdown` +
`clean`, then compile.

## Experiment 13 (SHIPPED to scala3 fork): redundant inline dependency extraction

After Experiment 12 removed the `TyperState.commit` hotspot, re-profiling (JFR on
the sbt server, same method) showed the next dominant cluster is the sbt/zinc
**dependency extraction**, grouped by subsystem:

| subsystem            | % of post-`commit`-fix samples |
|----------------------|--------------------------------|
| zinc dep-extract     | **23%** (16.7% via the `Inlining` phase) |
| constraint/subtyping | 16% |
| application-typing   | 13% |
| inliner              | 11% |
| type-substitution    | 10% |
| implicit-search      | 6%  |

The `Inlining` phase records incremental-compilation dependencies from inlined code
by running `inlineFinder` on the result of EVERY `transform` call (`Inlining.scala`,
`inlineFinder.traverse(result)` at the end of `transform`). `inlineFinder` hands each
`Inlined` node to `collector`, which traverses that node's inlined types (huge, for
DFHDL). Because `transform` runs per node and re-runs `inlineFinder` over the whole
result each time, an `Inlined` subtree is re-collected once per non-`Inlined`
ancestor on the path up to it - O(depth) redundant traversals of the same types.
The leaves confirm it: `Arrays.fill` (5.6%, from `scratchSeen.clear` in
`AbstractExtractDependenciesCollector`) and `EqHashSet.add` (2.7%).

Dependency recording is idempotent (it feeds name/dependency sets), so collecting
each `Inlined` exactly once is equivalent. Fix: an identity set of already-collected
`Inlined` nodes guards the `collector` call.
```scala
private val collectedInlined = util.EqHashSet[Tree]()
...
case tree: Inlined =>
  if collectedInlined.add(tree) then collector.traverse(tree)
```

Measured (patched vs `TyperState`-only compiler, same DFHDL sources):

| metric                           | TyperState-only | + dedup | delta      |
|----------------------------------|-----------------|---------|------------|
| zinc dep-extract (% samples)     | 23.2%           | 5.6%    | gone       |
| `Arrays.fill` (% samples)        | 5.6%            | 2.0%    | -          |
| total compiler samples           | 8761            | 7064    | **-19%**   |

Correctness: safe by construction - a distinct `Inlined` is never skipped, only
re-collection of the same node is, so no dependency is ever lost. Verified with
`StagesSpec` 526/526 + `CoreSpec` 104/104 AND the full scala3 `testCompilation`
regression corpus (exit 0, 0 failed, ~26 min). Delivered as its own commit/branch
(`soronpo/scala3` @ `claude/inlining-dep-dedup`), independent of the `TyperState` PR.

What's left after this (from the same profile): constraint/subtyping (16%),
application-typing (13%), type-substitution (10%) - these are genuine typing work
(`TypeMap.mapOver`/`Substituters`, `OrderingConstraint`), not obvious no-op
redundancy, so they are harder than the two wins above. The `Arrays.fill` residue
(scratchSeen growing huge then clearing for small traversals) is a possible small
follow-up (cap the retained capacity / reset-to-initial), lower value.

## Where the real time is, and what is worth trying next

**typer** is the prize, and no post-typer plugin can touch it. The single biggest
lever turned out to be compiler-side (Experiment 12, `TyperState.commit`, -32%);
what remains, ranked by expected value:

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
   bodies. Experiment 9 took the one clean bite here (the `ok` givens did not
   need `inline` at all, -5.3 s). The follow-up audit (Experiment 11) found no
   more free wins: `fromValue`/`conv` need `transparent` + inline params, and
   the trivial-looking `as*` casts need `inline` for meta-context positions.
   Further reduction now means genuinely smaller inline bodies on the hot ops
   (an architectural change), not just dropping the `inline` modifier.
4. **More compiler-side wins (Experiment 12 opened this door).** JFR on the sbt
   server is the tool: `jcmd <pid> JFR.start settings=profile`, compile, dump,
   aggregate leaf frames. After the `TyperState.commit` fix the next leaves are
   `TypeMap.mapOver` (~5%), `Arrays.fill`/`SimpleIdentityMap.updated` (the
   persistent-map copies feeding constraint solving), and `EqHashSet.add`. These
   are the constraint solver / implicit search doing genuine work, so they are
   harder than the no-op `commit` merge, but worth another profiling pass. The
   `TyperState.commit` patch itself should go upstream (one commit on the fork).
5. **`Aux`-member implicit search** (from the earlier trace, ~19 s). The classic
   slow HK/`Aux` pattern. Restructuring `ExactOp*Aux` to avoid the `Aux`-member
   summon is a DFHDL-side (Exact.scala) architectural change.

## Experiment 14 (2026-07): fresh baseline with both compiler wins + `scratchSeen` adaptive-clear (WASH, reverted)

Re-established the full profiling environment on 3.10: both compiler patches
applied together (cherry-picked `TyperState.commit` onto the `inlining-dep-dedup`
branch), the inkuire local build workaround, and the DFHDL 3.10 `BitNumWrapper`
`AnyVal`-drop. Fresh isolated `compiler_stages/Test/compile` (test sources only,
deps cached, `-Yprofile-enabled`): **159 s**, 6914 compile-thread JFR samples.
Both shipped wins confirmed still live: `TyperState.commit` **39% -> 1.0%**, zinc
dep-extract **23% -> 6.1%**.

**The post-wins profile is flat** (inclusive per-sample attribution, categories
overlap because the inliner re-types bodies that drive the rest):

| subsystem            |   % |
|----------------------|-----|
| application-typing   | 39% |
| inliner (transparent-inline expansion) | 35% |
| implicit-search      | 29% |
| type-map / substitution | 28% |
| TypeComparer (subtyping) | 23% |
| denotations          | 20% |
| constraint solving   | 15% |
| uniques / hash-consing | 9% |
| zinc dep-extract     |  6% |

Top *leaf* is `TypeMap.mapOver` (6.3%), entirely `Substituters.substSym`/
`substParams` under `TreeTypeMap.transform` from the **Inliner** - one-time
substitution of DFHDL's enormous inferred types into each expanded inline body.
Legitimate, non-redundant.

**Authoritative per-phase wall-clock** (same run, from the compiler's own
`-Yprofile-enabled`; JFR bottom-frame attribution is unreliable here because
`settings=profile` truncates stacks at 64 frames and the deep typer/inlining
phase-driver frames get cut). Whole compilation, **156.9 s total**:

| phase group                     |   wall |   % | notes |
|---------------------------------|-------:|----:|-------|
| **typer**                       | 113.2 s | 72% | 12.4 GB allocated - the GC driver too |
| DFHDL plugin phases (all 13)    |  13.6 s |  9% | breakdown below |
| other standard phases           |  10.8 s |  7% | genBCode 2.9, erasure 2.3, pickler 1.7, sbt-deps 0.9, sbt-api 0.8, splicing/pickleQuotes 0.6 ea, parser 0.4 |
| inlining                        |   8.2 s |  5% | non-transparent `inline def` expansion + dep extraction |
| MegaPhase transforms (~60)      |   7.0 s |  4% | crossVersionChecks/refchecks/betaReduce cluster biggest at 2.4 s |
| posttyper                       |   4.2 s |  3% | (was 20-70 s before the `minimizeCall` fix) |

DFHDL plugin phases individually: CodeDigest 2.2 (was ~17 s before Exp 1 - win
holding), PureCheck 1.6, OnCreateEvents 1.3, MetaContextGen 1.3, MetaContextPlacer
1.2, LoopFSM 1.1, FlattenInlined 0.9, TopAnnot 0.8, DesignClsSkip 0.8, DesignDefs
0.7, CustomControl 0.7, MetaContextDelegate 0.6, PreTyper 0.5.

**typer is 72% and everything else is small.** The two compiler wins already landed
*inside* typer (and the inlining / sbt-deps slivers); no non-typer phase is a
meaningful lever (biggest is inlining at 8.2 s, no plugin phase exceeds CodeDigest's
2.2 s). What is left in typer is genuine work: transparent-inline re-expansion,
implicit search for the `ExactOp`/`Check` givens, and substitution/subtyping over
DFHDL's large inferred types.

**Attempted next fix (the `Arrays.fill` residue, ~1.7%).** The sbt dependency
collector reuses one `scratchSeen` `EqHashSet` across every type-dependency
traversal and clears it with `clear(resetToInitial = false)`, i.e. `Arrays.fill`
over the backing array. A single giant-DFHDL-type traversal grows that array huge
and it is never shrunk, so every later small traversal pays a full fill of the
retained huge array. Added an adaptive `clearReusing()` to `GenericHashSet` that
drops the table back to initial capacity when the just-finished round left it
sparse (reads `used` before zeroing it), otherwise keeps the fast in-place fill;
pointed `ExtractDependencies.scratchSeen` at it.

Measured end-to-end (rebuilt+republished the bootstrapped compiler, clean DFHDL
rebuild, isolated JFR recompile):

| metric (compile thread) | before | after |
|-------------------------|--------|-------|
| `Arrays.fill` (leaf)    | 117    | **22** (-81%) |
| `GenericHashSet.clear` (any) | 115 | **16** |
| dep-extract cluster (any) | 362  | 357 (flat) |
| total compile samples   | 6914   | 6884 (**-0.4%**) |
| wall time               | 159 s  | 162 s (noise) |

The mechanism works (the fill leaf is gone) but it is a **wash on total CPU**: the
fill saving (~95 samples) reappears (~65) as the reallocation/regrow the shrink
introduces, netting -0.4% (noise). It is inherent - every dependency record builds
a fresh traverser and pays exactly one clear regardless; adaptive only makes a
small-clear-after-big cheaper, and the next big record regrows and eats it back.
A `collection.Vector` cannot help here: `scratchSeen` is a set needing O(1)
identity membership (a hash table's slot array), not a `Seq`; Vector gives O(n)
membership and would make the giant-type traversals O(n^2). **Reverted** - not
shippable as a perf win (valid micro-fix + memory-hygiene only). The fork stays at
the two validated wins.

**Conclusion after Experiment 14:** the two big levers were both no-op redundancy
(39%, 19%) and are banked. What remains is genuine typing work - substitution /
subtyping / implicit search over DFHDL's large inferred types, mostly inside inline
expansion - with no remaining compiler-side no-op-redundancy of consequence.
Further gains are architectural and DFHDL-side (fewer / smaller inline-expanded
bodies on the hot operators, an `Exact.scala`/`Checked.scala` restructuring); the
maintainer-ruled-out `transparent`-drop cannot substitute for that.



