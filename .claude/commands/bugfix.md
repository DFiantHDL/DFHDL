# DFHDL Bug Fixing Guide

> **For diagnosing and fixing a reported DFHDL bug**, especially one where the compiler produces
> wrong or illegal HDL rather than crashing.
> Invoke this before starting work on a bug report or a "generated code is broken" issue.
> For the mechanics of writing a stage, see [/new-stage](new-stage.md). For IR shapes, [/ir-reference](ir-reference.md).

---

## The Shape of a DFHDL Bug Fix

Most DFHDL bugs are not "a stage has a typo". They are **"the IR held a shape that should never
have existed, and everything downstream faithfully processed it"**. The stage that emits the
visibly-wrong output is usually innocent. So the work has a standard order:

1. **Reproduce** and get the illegal output in front of you.
2. **Localize** the stage that first produced the bad *shape*, not the one that printed it.
3. **Decide what the rule is**, and compile the shape in every scope to find the rule's true edges.
4. **Write the check first** (elaboration or sanity), and run the suite to measure the blast radius.
5. **Fix the stage(s)** that violate it.
6. **Test at the right level**, and mind the licensing of the reporter's code.

Skipping step 4 is the most common mistake: fixing the stage first hides which other stages, doc
examples, or checked-in designs were relying on the shape.

All six steps presuppose the bug is *in the IR*. Before starting them, rule out the other species:
see [When the bug is in the front end, not a stage](#when-the-bug-is-in-the-front-end-not-a-stage).

---

## 1. Reproduce

Put the design in `lib/src/test/scala/Playground.scala` and run the pipeline. Everything is
command-line driven, so no source edit is needed:

```bash
sbtn.bat 'lib/Test/runMain Foo --nocache --log trace compile --print-backend'
```

The command line is `[design-args] [app-options] <mode> [mode-options]`, a plain subcommand
structure, so placement around the mode is load-bearing:

| Piece | Side | Examples |
|---|---|---|
| Design args + app options | *before* the mode | `--width 12`, `--nocache`, `--log trace` |
| Mode | — | `compile`, `commit`, `simulate`, `lint` |
| Mode options | *after* the mode | `--print-backend`, `-b vhdl.v2008`, `-t verilator` |

`--backend`/`-b` is a **mode** option: it goes after `compile`, not before. Full reference:
`docs/user-guide/command-line/index.md`; for the app layer behind it (how options reach the
elaboration, the step pipeline and its cache keys) see `devdocs/dfapp.md`.

Always pass `--nocache`. Re-running an unchanged design short-circuits on the on-disk cache
(`Loading committed design from cache...`) and skips the stages and the trace entirely. It disables
both caches: the app's step cache under `sandbox/<Top>/cache` and the sub-design elaboration cache
in `*/target/scala-*/dfhdl-cache/`, so the design's body is really re-elaborated and every stage
really re-runs.

**Restore the Playground when you are done.** It is a working file the user may have their own
content in. Back it up first (`cp` to the scratchpad) and restore it after each probe.

---

## When the bug is in the front end, not a stage

Steps 2 through 6 assume a stage produced a shape it should not have. A whole other species never
gets that far: **the reproducer does not compile**, so there is no DB, no trace, and no stage to
localize. The tell is a `scalac` error whose "Inline stack trace" points into
`core/src/main/scala/`, i.e. the failure is inside a DFHDL operator's own inline expansion.

Issue #427 (`out_data <> bars(0).out_data`, where the port width comes from a design `CONST`) was
one of these. It read like a macro bug and was a Scala 3 compiler bug; the DFHDL fix was six
`asInstanceOf`s. Work it in this order instead.

### Minimize outside DFHDL, early

Get off the DFHDL types as fast as possible. Two plugin-free sandboxes:

- **`internals/src/test/scala/`.** `internals` is the only plugin-free subproject with a test
  directory (`plugin` and `compiler_ir` are also plugin-free; `core` and `compiler_stages` apply
  it to their `Test` scope, which is what you are trying to escape). A file dropped there builds
  with the same compiler and none of DFHDL's machinery, so it is the fastest way to prove "this is
  not our macros". Confirm with `show <proj>/<scope>/scalacOptions` rather than by reading
  `build.sbt`; the settings names do not map to scopes the way they read.
- **`scala-cli`**, once the repro has no DFHDL dependency at all:
  `scala-cli compile x.scala -S 3.nightly --server=false`.

Reducing #427 to `class Box[T]` plus `class Owner: val w: Int = 8; val b: Box[w.type]` turned a
DFHDL bug report into a fifteen-line compiler bug report.

### Bisect the compiler version before blaming the nightly

The build tracks a Scala nightly, so the reflex is "the nightly broke it". Check first:

```bash
for V in 3.3.7 3.7.4 3.nightly; do scala-cli compile repro.scala -S $V --server=false; done
```

#427 failed identically from 3.3 LTS through nightly, so it was long-standing rather than a
regression. That decides whether a workaround is needed now or an upstream revert is coming.

### `-explain` is the tool here, not the plain error

A mismatch between two types that **print almost identically** is the signature of this species.
The plain error is useless; `-explain` prints the subtyping trace and shows where it bottoms out:

```
==> (?2.w : Int)  <:  (?1.w : Int)
  ==> (?2 : Owner)  <:  (?1 : Owner)
    ==> Owner  <:  (?1 : Owner)  = false
```

Two skolems, `?1` and `?2`, standing for one prefix. Enable it with
`sbtn.bat 'set core/Test/scalacOptions += "-explain"; core/Test/compile'`.

### Bracket the trigger

Vary one axis at a time, with all the variants in one file so a single compile classifies them.
For #427 the trigger needed `transparent` **and** an `inline` parameter **and** a pattern with a
type variable to instantiate; dropping any one of the three compiled. That set is what makes an
upstream report actionable, and the variant that compiles is usually the workaround.

### The `core` inline-operator idiom, and its two traps

When an `inline` operator takes an operand's type apart, match the operand **retyped as the
operator's own type parameter**, never as written:

```scala
inline lhs.asInstanceOf[L] match
  case ___lhs: DFVal[lt, lm] => ...
```

`L` was derived once, at the call site. The operand *as written* may be a reference whose
underlying type the compiler re-derives per query, minting a fresh skolem each time for a prefix
that is not a stable path (`bars(0).out_data`). `<>`, `compare` and `DFBoolOrBit.sel` all carry
this now.

Two tidier-looking rewrites of that line are both wrong:

- **Do not extract the cast into a shared `inline def retyped[T](inline x: T): T`.** It compiles,
  but the helper's expansion carries *its own* position, which then replaces the user's position in
  elaboration errors. `ElaborationChecksSpec`'s "forward referenced value" case caught it, blaming
  `DFVal.scala` instead of the user's line.
- **Do not bind the scrutinee to your own `val`.** The compiler's inline-match scrutinee binding
  is named `$scrutineeN`, which the plugin skips; a hand-written `val` becomes a **named DFHDL
  value** and appears in the generated code. `DFBoolOrBitSpec`'s "selection operation" caught it.

### Every member-creating front-end op needs a trydf'd, cleanly-named runtime def

A raw `dfhdl.core.DFError$Derived` stack trace (instead of a formatted, positioned elaboration
error) means some inline op creates IR members with **no `trydf` on its runtime path**: an inner
TC conversion traps its own error and returns an errored value, and the first thing to touch it
(`DFVal.Func`'s arg walk) throws the `Derived`, which nothing catches. The fix is never to wrap
the inline body itself; move the `DFVal.Func` call (and, via by-name parameters, the TC-conversion
arguments) into a **runtime def** wrapped in `trydf { ... }(using dfc, CTName("<opname>"))`
(`DFBoolOrBit.Val.Ops.selRuntime` is the model; `CTName` is passed explicitly so the reported
operation name stays the user-facing one).

Two properties of that runtime def are load-bearing and easy to break:

- **It must be public** (or at least reachable without a synthetic accessor). A `private` def
  referenced from an inline body is compiled into an `inline$foo` accessor, and the plugin's
  meta-context fallback deliberately skips `$`-named applies. The stamp it would have applied is
  what *anonymizes* the propagated context, so without it a statement-positioned member silently
  inherits the **design instance's own name** from the constructor DFC (`PrintCodeStringSpec`'s
  "Boolean selection operation" caught three members all named after the outer `val id` binding).
  The `treeOwnerApplyMap` + anonymous-fallback pair in `MetaContextGenPhase.transformApply` IS the
  naming mechanism: spine applies of a `val` get the val's name, everything else gets an anonymous
  stamp, and both assume they can stamp the op's context apply.
- **Its error position comes from the plugin, not the DFC it happens to receive.** Applies inside
  a *library* inline expansion carry the library's own tree positions, and for TASTy-unpickled
  sources those are mangled (the tell: `DFBoolOrBit.scala:120:5642`, a line near the source's
  line count with an offset-sized column). A **macro-synthesized** apply (e.g. the
  `ExactOp3.apply` call that `exactOp3Macro` builds) is just as bad: its trees carry the
  position of the quote inside the macro's own source (`Exact.scala:505`), even though
  `Position.ofMacroExpansion` read *inside* that macro is the user span. `MetaContextGenPhase`
  keeps an `inlinedUserPosStack` of enclosing user-source `Inlined` nodes and substitutes the
  innermost user position wherever a stamp would otherwise carry an out-of-unit position; if
  positions regress to library files, start there. To see who stamps what, add temporary **file
  logging** (plugin `println` never reaches the sbtn client) around `addToTreeOwnerMap` and the
  two stamp sites in `transformApply`, filtered to the Playground unit.

Note the position such stamps produce is the innermost user-code inline call, which for a nested
op is the failing *sub-expression*, not the whole statement; `DFDecimalSpec`'s "Runtime error
positions" pins the exact spans.

The **compile-time** twin of this disease is separate: a raw `compiletime.summonInline` failure
inside an inline op's body reports at the summon site in the library
(`DFBoolOrBit.scala:120:6431`-style once TASTy-mangled), with no outer position chain for the
reporter to recover. Prove plugin-independence first with
`-P:dfhdl.plugin:disableCustomPrinter`: the raw compiler output is identical, so neither the
`CustomReporter` outer-drop nor any transform phase is the cause. The ops that report at the
user's code get their positions from **Exact-boundary macros** that bind the user's expression at
the call site, before inlining. Three cheaper spellings do NOT work from inside the inline body,
because the inliner rewrites substituted argument trees to body-local positions (verified by
macro file-logging: the user's literal argument arrives carrying a `DFBoolOrBit.scala` span): a
TrapError-style given splicing `compiletime.error`, extra transparent-inline nesting around the
summon, and a boundary macro taking the inline arg.

The fix that works is restructuring the op through an `exactOp*` boundary: `sel` became a thin
`transparent inline` forwarder to `exactOp3`, with its type-level dispatch re-encoded as
mutually-exclusive `ExactOp3` given instances (disjointness via `NotGiven` guards, so no given
prioritization). Two properties of that conversion carry the diagnostics:

- **Search the op instance under the `ControlledMacroError` trap** (`activate()` before
  `Implicits.search`, read `getLastMacroAbortError` on failure, `deactivate()` after — the
  `DualSummonTrapError` protocol). Without the trap, a candidate whose nested TC resolution fails
  through a reporting fallback macro RESOLVES with a stray `compiletime.error` spliced into the
  instance, and that leftover is later reported at a library-internal span; with it, the
  candidate aborts and the specific message (e.g. ``Unsupported value of type `"1"` for DFHDL
  receiver type `Bit`.``) is captured.
- **Report the trapped message at `Position.ofMacroExpansion`**, which inside an Exact-op macro
  IS the user's expression span (the flattenInlined instrumentation confirmed it), not at any
  tree position reachable from the operands.

`exactOp1`/`exactOp2` still use the untrapped generic-message report and would benefit from the
same upgrade. When converting an inline-dispatch op this way, the behavior matrix (which operand
drives the result type, and every exception to it) must be transcribed case by case into disjoint
givens; the op's existing print/selection spec tests are the safety net, and `UnstablePathSpec`
guards the skolem concern that the old `asInstanceOf[OT]` retype was carrying (exactInfo's
widening covers it at the macro boundary).

`Exact.flattenInlined` is a related but distinct position-stripper, and worth ruling out
explicitly when chasing a position bug: instrumenting it shows it discards `Inlined` wrappers
whose `call` carries the user span (e.g. `method + @ Playground:<225..233>`) and hoists their
proxy bindings into a flat macro-built Block, which is exactly why `MetaContextGenPhase`'s
args-descent workaround exists ("macros (e.g., flattenInlined in Exact) strip Inlined wrappers
that prepareForInlined relied on"). With that workaround the Exact-op stamps land correctly (the
plugin debug log shows `ExactOp2.apply` stamped at user positions). It was NOT in the chain of
either `sel` issue: the runtime junk stamps came from raw (non-Exact) inline bodies, and the
compile-time `sel` failure happens before any Exact macro runs, because `sel`'s generic `OT`/`OF`
params take the argument as-is; the INLINER itself repositions the substituted argument (macro
logging showed the user's `"1"` literal arriving with the span of the `onTrue` reference inside
the `sel` body).

Both cost a full suite cycle to find, and neither is visible in the file being edited.

### Changing a type-level algebra: pick the mechanism by when it costs

`IntP` decides widths at the type level, and there are three mechanisms for such a rule. They
differ most in **when they cost compile time**, and that is what should decide between them:

| mechanism | fires | cost |
|---|---|---|
| match type (`IsConstInt2`, `FoldConst1`) | during type reduction | none beyond the reduction |
| a `using` parameter (type class) | on **every** call site of the operation | can be ruinous |
| a `given Conversion` | only after an expression **failed to conform** | none on code that already compiles |

A `MaxOf[L, R]` type class summoned once per arithmetic operation did not finish compiling `lib`
in over half an hour; the same rule as a conversion built the whole tree in 3m33s. The type class
is not inherently the problem, though: `UBound` is also summoned twice per arithmetic operation
and is fine, because its given matches by a cheap **subtype** test (`T <: UB`), whereas
`MaxOf.same[W]: MaxOf[W, W]` made the constraint solver unify one variable against two deep width
trees and backtrack on every failure. Prefer the conversion when the rule only has to apply where
something would otherwise fail; prefer the match type when it must always apply.

### A type-level predicate must get STUCK, not answer "false"

Match-type reduction skips a case only when that case is **provably disjoint**; otherwise it gets
stuck. Stuck is the *safe* answer, because it defers and reduces later once the type is known. A
predicate that answers `false` about something merely undetermined commits the wrong branch
permanently. Both failure modes are real and they pull in opposite directions:

- `case (Int & Singleton, ...)` can never refute plain `Int`, so `Max[Int, Int]` sticks. Safe and
  useless: a collapsed width can then never feed a further operation.
- `IsConst` answers `false` for anything whose reduction is pending
  (scala/scala3#26683), so a guard that is not handed a bare type parameter collapses silently at
  the **definition** site, before the call site can supply a literal.

So an `IsConst` guard's argument must be a plain type parameter or a `compiletime.ops`
application. Four spellings are not, and all four bit in one change:

1. a nested application of the guarded operators (`CLog2[+[V, 1]]`)
2. the same composition spelled infix, inside a scope that does `import IntP.{-, +}`
3. a path-dependent type from a `using` parameter (`ubLW.Out`, `icL.OutW`)
4. a path-dependent type in a **return** type (`.bits` giving `DFBits[w.Out]`), which poisons
   every later operation on that value rather than one site

The remedy for all four is the same: bind the width to a type parameter, and express a composed
width as ONE named operation whose body does the whole calculation in `compiletime.ops.int`.
Naming those operations (`CLog2P1`, `ArithMaxWidth`, `PartSelectHigh`, `RangeWidth`) is worth doing
for its own sake, and it makes the guard-once rule visible at each site.

### Weakening a type does not break values, it deletes diagnostics

Making the type level say less is safe for the generated hardware, because the IR carries the real
width in `IntParamRef` and elaboration checks it there. It is dangerous for *error reporting*. The
failures to expect are therefore specs that assert an error and find none: `assertCompileError` and
`assertDSLErrorLog` reporting `No error found`. Note `assertDSLErrorLog` asserts **twice**, a
compile error for its snippet and then an elaboration error for its block, so "which half failed"
is a real question and the failure position does not tell you.

### Sibling op givens drift like twin helpers do

The "twin helpers drift" rule from §2 applies to `ExactOp*` given families too. Issue #445: the
commutative and non-commutative arith givens both carried wildcard-`Int` adaptation
(`checkWildcardFit` + adapt to the bit-accurate operand), while the carry givens
(`evOpCarryAddSubDFXInt`, `evOpCarryMulDFXInt`) had none, so an `Int <> CONST` parameter fell
through to its runtime representation (signed 32-bit) and silently produced `SInt[33]` where
`UInt[11]` was expected. When one given of a family handles a species of operand specially, diff
the siblings for that branch before concluding the behavior difference is intentional.

Two mechanism notes from that fix:

- **Two operand species can be type-level identical and runtime distinct.** A Scala `Int`
  (literal or runtime) and a DFHDL `Int` parameter both reach an op given as
  `OutS = Boolean, OutW = Int, OutN = Int32`, but at runtime the Scala `Int` candidate has
  already built a bit-accurate const at the value's minimal width, while the parameter is still
  `DFInt32`. When the two need different semantics (carry ops: literals keep minimal width,
  pinned by `100 *^ u8 == UInt[15]`; parameters adapt), dispatch on
  `dfType.asIR.isDFInt32` at runtime and leave the static `Out` degraded, rather than inventing
  an `IsConst`-style type-level discriminator (the §"stuck, not false" traps).
- **For a new operand-legality rule on type-level `Boolean`/`Int` values, prefer a
  `Check1`/`Check2` object (`Checked.scala`) over `AssertGiven`.** One object holds the condition
  and message for every use site (alias it in `Constraints`, e.g. `CarryCheck`), it fails at
  compile time when the types reduce, and the same instance is runtime-invocable with runtime
  witnesses for the widened case. Empirically its failure inside an **untrapped** `exactOp2`
  candidate resolution still surfaced the *specific* message at the *user's expression* span
  (the spliced `compiletime.error` reports at the inlined call), so the generic-message caveat
  above does not always cost you the diagnostic; verify per case with `assertCompileError` plus
  one manual compile for the position.

### Probing type-level behaviour

Two traps, each of which cost several cycles here:

- **Reproduce in the scope the code actually lives in.** A probe at file scope resolves `+` to
  `dfhdl.internals.+`, while the site under diagnosis may sit inside `import IntP.{-, +}`. The
  same source text is then a different type function, and the probe cheerfully proves the opposite
  of the truth.
- **Control every probe against `HEAD`.** `summon[BitIndex.CheckNUB[8, 8]]` succeeds both before
  and after the change, so it establishes nothing. Stash the change, re-run the same probe, and
  believe it only if the two answers differ.

When hypotheses keep missing, stop reasoning and bisect your own change (`git stash push -- <the
files>`, re-run the failing spec). That is what found all four spellings above, after three wrong
guesses at the mechanism.

### Test in `core`, then report upstream

The regression test belongs in `core/src/test/scala/CoreSpec/`, not `StagesSpec`: no stage is
involved. `core` cannot run the compile pipeline, so assert on the freshly elaborated DB
(`dsn.getDB`, then `DefaultPrinter(using db.getSet).csDB`) and expect the **raw** member names,
before the stages that rename and reorder members. `UnstablePathSpec` is the model. Then file the
compiler bug upstream on `scala/scala3`, with the minimized repro, the version bisect and the
variant set, and link it from the code comment carrying the workaround; #427 became
scala/scala3#26681.

**One false alarm to expect.** Editing `core/` and then compiling `lib` incrementally against it
reproducibly threw `scala.MatchError: 23 ... TreeUnpickler.readConstant` on this nightly. That is
stale TASTy, not the change under test: `sbtn.bat 'clean; clearDFHDL; Test/compile'` clears it. Do
not chase it, and do not trust a suite run that followed one.

---

## 2. Localize the stage that introduced the shape

### When something throws

The `SanityCheck` that throws fires *immediately after* the offending stage. Read the
`Running stage X....` sequence and take the first failure, not the first suspect.

### When nothing throws (the harder, more common case)

A run that succeeds end-to-end and emits HDL the tool rejects (`syntax error, unexpected
TOK_ASSIGN`) fires no check, because the DB is structurally fine. The shape is merely
unrepresentable in the target language. Use the `--log trace` code dumps:

- Read the dumps **forward** and find the first printout containing the offending construct.
  Attribute it to the stage that ran just before that dump.
- Then walk **backward** through each handoff asking: *is this IR shape legal, or merely tolerated
  by everything downstream?* The earliest stage that produced an illegal shape is the culprit, and
  it is usually several stages away from the symptom.

In issue #426 the backend faithfully printed `assign` inside an `always_comb`; the connection was
planted by `ExplicitNamedVars` several stages earlier, and the *named value* it wrapped was created
two stages before that by `NamedVerilogSelection`, which was the real culprit.

### When the same code works or fails depending on what ran before it

Minimize by **deleting the earlier statements, not the failing one**. If removing an unrelated
line above the failure makes the failure go away, the bug is not in the failing operation at all:
something is memoized under a key that does not capture everything the answer depends on, and the
first query poisoned the slot for the second.

Issue #430 read like "`.toScalaInt` cannot fold parameter arithmetic". It folded fine on its own;
it only failed when the same expression had already been consumed as a parametric width. Both go
through `DFVal.getConstData`, which takes a `ConstData.CachePolicy` that decides whether design
parameters resolve to their applied data or stay an opaque `UnknownConst` — two different answers,
one cache slot. The half-measure already in the code (invalidate when `this.isDesignParam`) is the
signature of this genus: someone saw the collision on the parameter itself and missed that it
propagates to every expression built over it.

So when a cached field feeds off a policy, mode, or `using` flag, the fix is at the cache, and the
question to ask is *which policies produce interchangeable answers* — not which node types to
special-case.

**Then keep the cache.** Disabling it for every non-default policy is correct and is the wrong
answer: it silently turns a memoized walk into a full re-walk on a path (`.toScalaInt`,
`getConstDataOrDefault`) that user code hits constantly. Look for the asymmetry instead. Here only
ONE node type reads the policy, and it diverges in one direction only: `Always` answers
`UnknownConst` exactly where the resolving policies would fold. That makes any *other* `Always`
answer provably policy-independent, so the resolving path can consult the shared cache first and
re-walk only on `UnknownConst` — which confines recomputation to the parameter-dependent spine
while every parameter-free subtree still answers from cache. Note the direction matters: the
mirrored rule (cache the resolved answer when it is `KnownConst`) is *unsound*, because that
`KnownConst` may have come through a parameter and would then fold a value that must stay
parametric. Prove which way the asymmetry runs before exploiting it, and pin **both** consumption
orders in the regression test.

### When the bug only appears across a serialization or cache boundary

An internal `NoSuchElementException: key not found: "TW_..."` that fires only when the sub-design
cache serves a hit, while a live elaboration of the identical source passes every check, is a
**ghost binding**: a refTable VALUE whose member object was removed from the member list after the
binding was made (issue #449). Live runs tolerate ghosts because the tokens a ghost emits still
resolve in their own run; adoption re-mints tokens for members only, so a ghost's tokens dangle in
the loading run. Lessons that generalize:

- **The report's trigger may be cache-bypass, not cause.** A coarser cache above the buggy one
  (the DFApp step cache replays the whole design on identical re-runs) can mean the failing run is
  the FIRST to ever exercise the buggy path. "Edit + rebuild crashes, identical rebuild is fine"
  read as invalidation; the truth was "adoption of this entry always crashes, and only the edit
  makes elaboration actually run". Reproduce with two elaborations in one JVM through the
  `MapSubDesignCache` seam before believing any staleness theory.
- **Token forensics.** A ref token prints as `TW_<grpId1Hex>_<grpId2Hex>_<id>` with
  `grpId = (position.hashCode, per-position JVM counter)`. In-JVM double elaboration gives the
  storing run counter 0 and the loading run counter 1, so the failing token's counter says
  immediately whether an unfreshened STORED token leaked through re-minting.
- **Validate an artifact over its refTable VALUES, not only its keys.** "Every ref a member emits
  is bound" (key closure) does not imply "every binding target is a member" (value re-uniting),
  and only the second catches ghosts. `SanityCheck.refCheck` reports the same defect stage-side as
  "Ref exists for a removed member"; `SubDesignEntry.isSelfContained` is the entry-level contract,
  kept at SANITY level (asserted in the cache specs, never computed on the production store/lookup
  path: always-on validation was rejected as redundant, since only a DFHDL bug or a dirty dev loop
  can violate it). The stored entry is JSON, so all of this is checkable offline in a Playground
  `@main` with no compiler edits.
- **A removal decided on "unreferenced NOW" is unsound when a front-end handle can bind refs
  LATER.** `MergeAssocFunc` absorbed an intermediate `+` Func and removed it before `lsbitsAt`
  bound the offset refs to it (a method parameter is a handle; anonymity is about naming, not
  about reachability from Scala code). A first fix made the removal resurrectable (un-ignore on
  bind), and it worked, but was retired as compensation for a decision made at the wrong time.
  The adopted principle instead, scoped to OPERATION SIMPLIFICATIONS
  (arithmetic/logic/casting/conversion): a simplification never `setMember`s/`replaceMember`s/
  removes an anonymous member; it builds a NEW member with fresh refs and leaves the superseded
  one as debris for a snapshot-boundary sweep (`endDesign`, where "is it read?" has its final
  answer). A blanket non-anonymous-target guard was rejected as too broad: construction
  protocols (`initForced`, conditional-header retyping, `setName`/`tag`) legitimately keep
  revision semantics; a ghost from one of those would surface loudly via `DB.check` /
  `SanityCheck.refCheck` and the sanity-level `isSelfContained` contract in the cache specs.
  Converted sites: `SimplifyFunc` (all extractors, with `rebindMeta` naming by `Ident` wrap and
  the `=~` comparisons made ident-transparent via `stripTypePreservingAliases`) and the
  DFDecimal carry peel/retype; the DFVal `AsIs` in-place conversions were audited and KEPT (a
  revision, unlike a removal, cannot ghost: same-context bindings are re-pointed, cross-context
  bindings to anons never exist).
- **`clearDFHDL` before trusting a full-suite run that follows core elaboration edits.** Stale
  `dfhdl-cache` entries stored by the pre-edit build stay digest-valid under uncommitted edits
  (the `dfhdl@<version>` fold only changes on a commit), and adopting mixed-era entries can shift
  the dclName enumeration: the AES `FullCompileSpec` file-NAME comparison failed with
  `mulByte_0/1/2` renamed to `_1/2/3`, which reads like an enumeration bug and is cache debris.

### Two habits that pay off

- **Check the other backend.** Re-run with `compile --backend vhdl.v2008` (or `verilog`). If both
  are wrong in *different* ways, you have two bugs and the shared IR shape is the root cause.
  VHDL tends to fail *silently* where Verilog fails loudly: the same #426 shape emitted a signal
  assignment inside a VHDL process whose next statement read the value a delta cycle too early.
- **Grep `lib/src/test/resources/ref/` for the construct.** If no reference output contains it,
  that code path is untested, which is why the bug survived. That also tells you the fix needs a
  new reference test, not just a patched stage.

### An exemption phrased by shape swallows every construct with that shape

When a stage's criteria carry an exemption written as a pattern (`case Ident(_) => false`, "skip
values referenced by X"), the comment above it names the *one* construct the author had in mind,
while the pattern matches every construct that happens to build the same node. Enumerate the
creation sites before trusting it: grep for who constructs that node type. Anonymous idents, for
instance, come from three unrelated places (a conditional-expression branch result, a fall-through
step block, and a method's return wiring), and an exemption meant for the first silently swallowed
the third, so a method returning a conditional expression was never lowered and only surfaced as
`Unsupported member for this VerilogPrinter` at the very end of the pipeline.

Fixing it means narrowing the pattern to the construct the intent names, and narrow it in the
direction that keeps unenumerated cases on today's behavior — here `!ident.getOwner
.isInstanceOf[DFDesignBlock]`, which changes the def-return case alone, rather than an allow-list
of owners that would also change anything not yet thought of.

### Twin helpers drift, and only one of them gets fixed

Two stages that lower the same construct at different points often carry near-identical recursive
helpers (`ExplicitNamedVars.patchChains` and `ExplicitCondExprAssign.patchChains`). When one has a
case the other lacks, that is a bug report, not a design difference: diff them line by line. The
version that lowers a *named* conditional was missing both the ident removal and the `DFUnit`
retype that the other one performs on a nested header, so every nested conditional expression in a
branch was quietly broken, independently of the bug being chased.

### Fix the shared base, not the subclass you happened to find

If the culprit is one of several stages sharing an abstract base (the `NamedAliases` family, the
`ComposedDFTypeReplacement` family), check whether siblings reproduce it before fixing the one you
found. #426 surfaced via `NamedVerilogSelection`, but `NamedVHDLSelection` reproduced it and
`NamedAnonMultiref` is backend-independent, so the fix belonged in the base.

---

## 3. Establish the rule, and find its real edges

Before writing any check, state the invariant as a sentence, then **compile the shape in every
scope it can appear in**. The intuitive rule is usually broader than the real one.

For #426 the intuitive rule was "a conditional expression branch is not a scope, so it cannot hold
a named value". Compiling that shape in five scopes showed four of them lower correctly:

| Conditional expression sits in | Named value in a branch | Why |
|---|---|---|
| ED domain body (**concurrent**) | **illegal** | branch is not a block; the drive becomes a connection |
| ED `process` | legal | branch lowers to a procedural block |
| RT / DF domain | legal | same |
| conditional *statement* branch (`dfType == DFUnit`) | legal | branch is a scope in its own right |

A uniform rule would have forced pointless rewrites of two stage specs that were exercising
working behavior. **Let the mechanism set the boundary, not the intuition**: here the line is
exactly the predicate `ExplicitNamedVars` uses to choose a connection over an assignment
(`isInEDDomain && !isInProcess`), because a connection is the only drive with nowhere to live.

Watch for **exemptions inside the rule** too. A named conditional *header* is legal anywhere,
because `ExplicitNamedVars` drives it through `patchChains` (an assignment per branch, never a
connection). A check written without that exemption rejects working user code. Probe each
sub-shape of the rule separately, and when the check rejects something, verify it *actually*
miscompiled before accepting the rejection.

### A scope rule must model what the BACKEND renders, not what the IR nests

"A value declared inside a block cannot be read outside it" is the obvious phrasing and it is wrong,
because an **anonymous value has no place of its own**: the printers emit it inline at whoever reads
it. Written literally, that rule flagged a shape `DropRTProcess` legitimately produces (an anonymous
`!go` parked in one case block and read from another, which inlines harmlessly in both). Making
anonymous values *transparent* instead, and checking only what they transitively reach (a
declaration or a named value, which do have a place), took the blast radius from one stage failure
to zero and made the error name the real culprit, the iterator `k`, rather than an unnamed
expression.

The general form: before phrasing an invariant over the IR, ask what the backend does with each
node kind. A node that is copied to its use site cannot violate a placement rule; only a node that
is *emitted where it sits* can.

And **the IR owner is not always the scope the backend gives a member**. A `for` iterator's `Dcl` is
owned by the ENCLOSING block (elaboration creates it just before the loop) while every backend
emits it in the loop header. A scope check has to special-case that, and the first version that did
not silently passed the very bug it was written for. Check where the printer puts a declaration, not
where the IR hangs it.

### A report can hold two independent defects, including one the reporter dismissed

Issue #433's second listed defect was a missing `;` on a declaration inside a generated `for` block,
and the reporter's own verification pass concluded it "is not part of this repro". Reproducing it
directly, with a plain `val v = SInt(16) <> VAR` in a loop body and no `var` anywhere, showed it was
a separate and more general bug: `DropLocalDcls` climbs out of conditional and step blocks but not
loop blocks, so Verilog emitted a declaration without its terminator and VHDL put a `variable`
inside a `loop`, which is illegal outright. Re-derive each listed symptom from scratch instead of
inheriting the reporter's attribution; theirs was reasoned from one file, and the minimal repro for
a *different* defect is usually a different program.

### Classify every position in one run

Building that table costs one compile if you write it as a table. Put each variant in the Playground
as its own `@top(false)` design and drive them all from one `@main`:

```scala
@main def probe(): Unit =
  def go(name: String, dsn: => core.Design): Unit =
    println(s"===== $name =====")
    try println(dsn.getCompiledCodeString)
    catch case e: Throwable => println(s"[${e.getClass.getSimpleName}] ${e.getMessage}")
  go("S1", S1()); go("S2", S2()); ...
```

`getCompiledCodeString` runs the whole pipeline, so every variant lands in one of three buckets:
legal HDL, a clean error, or a crash. Seven `OPEN` positions (issue #434) took a single 34-second
run to classify, and the spread was nothing the stage sources suggested: one worked, two crashed
(one at elaboration, one only in the backend printer) and **four silently emitted illegal HDL**.
Those four are the reason to compile every variant instead of reasoning about them — a shape that
does not crash is not thereby legal.

Run it with `sbtn.bat 'lib/Test/runMain probe'`, and do **not** add your own
`given options.ElaborationOptions.OnError = _.Exception` to the Playground: `ElaborationChecksSpec`
already declares one at top level in the same (root) package, and a second makes every `@top` in
the file ambiguous, with 226 errors that never name the duplicate given as the cause.

---

## 4. Write the check first

### Elaboration check vs `SanityCheck`

| | `DB.check` (elaboration) | `SanityCheck` (stage pipeline) |
|---|---|---|
| Runs | once, right after elaboration | after every non-`NoCheckStage`, debug mode |
| Answers | is the *user's design* well-formed? | did a *stage* corrupt the DB? |
| Wire it up in | `DB.subDBCheck` / `rootDBCheck` in `DB.scala` | `SanityCheck.transformSubDB` |
| Test it in | `lib/.../ElaborationChecksSpec.scala` | the failing `<Stage>Spec` |

**The deciding question is: can a user write this by hand?** If yes it is an elaboration check,
*even when the bug you are chasing reached you through a compiler stage*. A rule only a stage can
violate (ref-table integrity, ownership ordering, an HDL-method body restriction laundered through
a helper `def`) belongs in `SanityCheck` — see the comment on `SanityCheck.hdlMethodCheck` for why
it is deliberately not on the elaboration path.

**The two overlap, deliberately.** `SanityCheck.transformSubDB` runs the whole of `DB.subDBCheck`
(the per-design half of `DB.check`) on top of its own structural checks, so:

- A **per-design** check added to `DB.subDBCheck` binds the user *and* every stage, for free. This
  is where a rule belongs whenever a user can write it by hand.
- `rootDBCheck` (the cross-design half: dangling ports, clock rates, device-top placement) is
  elaboration-only. Those checks assume a shape the pipeline deliberately rewrites, so they cannot
  run between stages.
- A rule only a stage can violate goes in `SanityCheck` itself. Note `DB.check` is a `lazy val`
  invoked from exactly one place, `Design.onCreateEnd` — it runs once, on the elaborated user
  design, and never again.

Every `StageSpec` calls `sanityCheck` directly, so all stage tests enforce these regardless of log
level (in a normal compile `SanityCheck` only runs at `logLevel >= DEBUG`).

Expect that wiring to fail immediately, and treat that as the point: a stage whose *own output*
trips the check cannot be repaired by a later cleanup stage, because the DB between them is
invalid. The fix has to be inside the offending stage's own patch.

Keep the check and the stage predicate textually tied: put a comment on each pointing at the other
and saying they must agree. They encode the same fact and will drift otherwise.

### "It crashes with a stack trace" is a missing check, not a reporting bug

A report of an internal-looking crash misnames the defect twice, and both need correcting before
you start:

- **The stack shows where the shape was first *queried*, not where it was created.**
  `connectionTable` is a `lazy val`, so a net it cannot resolve surfaces wherever something first
  forces it, which can be the backend printer many stages after elaboration. The reported source
  position is still correct, and that is exactly what makes the trace read like a printer bug.
- **You cannot judge the user-facing output from an sbt run.** `exitWithError` branches on
  `OnError`, which defaults to `Exception` under sbt (so the build survives) and to `Exit`
  everywhere else, where it is `println` + `sys.exit(1)`. An elaboration error is therefore already
  clean for the user running scala-cli, and the trace they pasted is itself evidence that the error
  was **not** on the elaboration path. Moving the case onto that path is the whole fix; there is no
  formatter to go looking for.

### Give the analysis a verdict, do not enrich its fallback

`getConnToMap` derives each net's direction and parks the undecidable ones; once nothing is left to
re-examine it throws "Unable to determine directionality" with a list of positions and nothing else.
That throw is a backstop for shapes nobody has ruled on, so a shape reaching it is a **missing
rule**, not a message worth improving. Add the verdict as a `newError` inside `getConnToMap`: its
`case Nil if errors.nonEmpty` arm is matched before the pending-net arm, so the specific message
wins over the generic one automatically, and it arrives in the standard connectivity-error block
(position, hierarchy, LHS, RHS) at no cost.

### A conservative check over parametric bounds: prove, resolve, and only then reject

A check that compares parameter-dependent index/width expressions (the slice-overlap check of
issues #442/#447 is the archetype) must not collapse "parametric" to "unknown": that rejects
`o(W-1, 0)` next to `o(2W-1, W)`, which are disjoint for every W. The machinery that fixed it
generalizes:

- **Decide on linear forms.** `IntExprCalc` decomposes an integer `DFVal` expression into
  `Σ ci·basei + offset`; `Slice.Symbolic` carries `(lo, width)` as such forms, and
  `IntExprCalc.DataCalc.proveNonNeg` proves `e >= 0` using validity facts (every slice width is
  `>= 1` on the valid parameter domain). The single-fact proportional rule is enough for the
  equal-bin family (`k*W` slices of width `W`) at any pair distance. When neither disjointness
  nor overlap is provable, keep the conservative error but say *why* (a distinct message for
  "cannot be proven disjoint"), the generic message misled the #442 reporter into a wrong theory.
- **Resolve applied parameters through the instantiation site, never by gating on `isTop`.**
  Under the hierarchical model *and* in DBs flattened from it (the backend printer's flat DB),
  every design block's `ownerRef` is empty, so `isTop` reads true where it must not — that gate
  silently kept a sub-design's `W` symbolic. `GoThroughDesignParams` is wrong in the other
  direction: it folds even the elaboration root's parameters (that is `toScalaInt`'s job), and a
  root parameter must stay symbolic because it is overridable in the generated HDL.
  `DesignParam.instAppliedConstDataOpt` is the correct primitive: cached instance during
  elaboration, `designBlockInstMap` on flat DBs, `parentSubDBOpt` walk-up on hierarchical
  sub-DBs, and `None` exactly for the elaboration root.
- **The check re-runs where you don't expect.** `connectionTable` is forced again by the backend
  printer on the *flat* DB, so a connectivity-analysis fix must resolve under every DB model; a
  test that only elaborates is blind to the print-time re-run. Pin it with
  `getCompiledCodeString` (`ElaborationChecksSpec`'s sub-design slice test is the model).
- **`clearDFHDL` between probe re-runs after compiler edits.** The sub-design elaboration cache
  serves API-driven probes (`getCompiledCodeString`) too, not just DFApp runs; a cached child
  elaboration skips the very code you just changed and the probe "reproduces" stale behavior.
- One departial-coordinate trap fixed alongside: a vector `ApplyRange`'s indices are in **cell**
  units and must be scaled by the cell width into bit coordinates; the old `shift(idxLow)` mixed
  units and falsely errored even fully-literal `o(0, 1)` / `o(2, 3)` vector range connections.
- **Symbolic elimination is a per-site semantic choice, not a smarter equivalence.** The width-fit
  checks accept `LHS >= RHS` after a mixed `max`/`min` drops its symbolic operands
  (`16 >= WIDTH max 16` decides as `16 >= 16`; `IntParamRef.compare(..., elimSymbolicMaxMin =
  true)`), which deliberately tolerates the symbolic case's truncation. Two rules keep it safe:
  it must NEVER back `=~`/`isSimilarTo` (calling `max(W,16)` similar to `16` would skip the
  resize insertion in `toDFXIntOf` and miscompile), and every sibling decision site of the same
  construct must adopt it together — carry promotion (`carryPromoteWidthCheck`) had to switch
  with the TC width-fit check, or `sum := x + y` (anonymous, carry-promoted to `max+1`) would be
  definitively rejected while `val xy = x + y; sum := xy` passes.

### Then measure the blast radius

Run the full suite with the check in and **no stage fixes yet**. The failures are the deliverable
of this step: they tell you which stages violate the rule and whether any checked-in design or doc
example depended on the shape. Report them before fixing, because "this is bad code we should fix"
and "the rule is too strict" are the user's call, not yours.

That call extends to shapes a **stage** synthesizes, which the suite may not cover at all.
`ConnectUnused` turns every `@unused` port into `<> OPEN`, so an `@unused` *input* port trips the
new check. Narrowing the stage to output ports looked obviously correct and was not: the annotation
exists to silence a check, an input port still has to be driven, and the new error is therefore the
right outcome, with the user's design the thing that needs fixing. So a producer the tests do not
exercise is a question, not a to-do.

### Position-sensitive elaboration tests

`ElaborationChecksSpec` expectations embed `<file>:<line>:<col>` of the offending expression.
scalafmt reflows the test design (a braces-on-one-line block becomes multi-line), which silently
shifts those positions. Write the design in the already-normalized indented form so reformatting
does not move it, and re-check the positions after running scalafmt.

---

## 5. Fix the stage

Load [/new-stage](new-stage.md) **before editing any stage**, including a one-line change. It
carries the invariants (determinism, idempotency, printability), the patch merge table, and the
structural rules that decide whether your fix is even the right shape.

Two structural rules that come up constantly in bug fixes:

- **A new phase inside an existing stage is almost always wrong.** Either the work is a
  self-sustained, idempotent, fix-point transformation, in which case it is its own **stage**, or
  it belongs in the **same patch** as the existing work.
- **But if the work exists to keep the stage's own output legal, "separate stage" is off the table
  too** — see the `SanityCheck` point above. It must be the same patch. When that looks
  unmergeable, check whether a different `Patch.Replace` config dissolves the collision before
  concluding it is impossible: for replace-and-relocate, `ChangeRefAndRemove` plus moving the
  *replacement* keeps the two patches on different keys, where `FullReplacement` would collide
  with the `Remove` that `Patch.Move` emits per moved member.
- **A fix that can decline is not a fix.** If your transform has a case it refuses to handle, that
  case still emits the illegal shape; you have narrowed the bug, not fixed it. Make the transform
  total, even when that means carrying more with it (a conditional expression relocates as
  `header :: blocks ::: contents`). Watch for two transform sites wanting to relocate the same
  sub-tree — gate each pass to the innermost and drive the stage `@tailrec`.

### Moving members: everything must still be defined above the anchor

When a fix relocates a member, check every ref of every moved member. A dependency that lives in
the region you are moving out of, but is not moving with them, is left behind as a forward
reference, and its drive stays put — inferring a latch in the generated HDL. Abandon the whole move
in that case rather than splitting it.

`SanityCheck.orderCheck` catches the forward reference itself (`Failed member order check!`), so
the symptom now arrives right after the offending stage instead of as mystery HDL. It does not
catch the *drive* left behind, which is the part that infers the latch — that is still on you.

`collectRelMembers` will not warn you: it recurses only into **anonymous** values, so it stops at
any dependency a sibling patch in the same pass just named. A guard written against the cone
silently passes. Check the members' `getRefs` directly.

### When the culprit really is the printer

Not every wrong-HDL bug is a wrong IR shape. Sometimes the IR is right and the backend flattens a
distinction the target language makes. `DFNet.Op.Assignment` is one IR construct, but VHDL picks
the operator from the target's **object class** (`:=` for a variable, `<=` for a signal) where
DFHDL picks it from blocking-vs-non-blocking. `VHDLPrinter.csAssignment` hard-coded `:=`, so every
assignment to a signal was illegal VHDL. Two lessons generalize:

- **Hand the backend the IR member, not a pre-digested boolean.** `csAssignment` took
  `shared: Boolean` because Verilog wanted one bit for a lint pragma, which left VHDL no way to ask
  its own question. Passing the LHS declaration lets each backend derive what it needs, and the
  next distinction costs no signature change. Issue #437 collected on exactly this: once shared
  writes became `:==`, `csNBAssignment` needed the same `lhsDcl` (VHDL has no non-blocking form
  for a variable), and the signature change was one parameter because the pattern was in place.
- **When two print sites decide the same fact, derive both from one predicate in `analysis`.** The
  declaration keyword (`signal` / `variable` / `shared variable`) and the assignment operator are
  the same question asked twice; kept apart they drift into declaring a `signal` you then write
  with `:=`. `DFVal.Dcl.isHDLVariable` answers it once, with a comment at each call site saying so.
  This is the printer analogue of the check/predicate pairing in §4.

When you rewrite an existing predicate into a shared one, expand both forms case by case and
confirm they agree on every branch, including the ones no test reaches (a `VAR.SHARED` inside an
HDL method). A "simplification" that quietly moves an edge case is a second bug riding along.

- **One literal-format knob can be semantically overloaded across print contexts.** The Verilog
  bubble digit was `?` everywhere, which is correct in a `casez` pattern (where `?` aliases `z`,
  the wildcard) and wrong in every value position (where it *drives* high-impedance; the value
  don't-care is `x`). Issue #438's fix exposed it: the new comb defaults printed `tmp = 8'h??`
  and Yosys warned about tri-state logic; the checked-in ALU example was likewise assigning
  32 bits of `z` in a `case` default arm. When one format hook serves two target-language
  contexts with different semantics, split by *printing context*, not by node type. Carry the
  context as an explicit parameter threaded through the method signatures, defaulted to the
  common case (`inPattern: Boolean = false` on `csConstData` down to `csDFBitBubbleChar`) and
  turned on only at the special site — review rejected both a printer `var` (a printer may be a
  shared object) and a sibling printer-instance mechanism in favor of the explicit flag. The
  interception point is exact: for an anonymous const, `refCodeString` IS `csConstData(dfType,
  data)` (no wrapping), so `csDFCasePattern` can re-route singletons through the flagged call
  print-identically. The same overload can hide in a STATEMENT form instead of a digit: VHDL
  keeps one don't-care digit (`'-'`) but splits wildcard semantics by construct (`case?` vs
  `case`), and the VHDL printer was emitting a plain `case` for wildcard matches, leaving every
  wildcard arm dead in simulation, silently (GHDL analyzes it without a warning). When auditing
  one backend's context split, audit the OTHER backend's rendering of the same IR feature: the
  distinction always exists somewhere, either in the literal or in the construct.

- **A diagnostic that names IR members through the code printer inherits the code printer's
  scoping, which is wrong for errors.** `refCodeString` renders a reference relative to the
  reference's OWN design (and prints a `DesignParam` bare unconditionally), which is correct for
  printing code and degenerate in an error message: two same-named constants from different
  designs print identically ("width (OUTPUT_WIDTH) differs from width (OUTPUT_WIDTH)", issue
  #448). Error messages must render relative to the ERROR SITE (`getRelativeName(dfc.ownerOption
  ...)` → `c.OUTPUT_WIDTH` vs `OUTPUT_WIDTH`); that lives in a dedicated sibling
  (`refErrorString` / `widthErrorString`), never in a change to the code-printing path. Related:
  such runtime elaboration messages are untouched by the plugin's `disableCustomPrinter`, which
  only affects scalac diagnostics; if a bad message survives that flag, stop suspecting the
  custom printer.

The blast-radius step still applies, and here it reads inverted: a fully green suite with no
reference output changed is not evidence the fix is inert, it confirms the whole branch was
untested. The `ref/` grep from §2 predicts this: only the shared-variable form of `:=` appeared
anywhere under `lib/src/test/resources/ref/`, which is exactly the one form that was already right.

---

## 6. Test at the right level

**Prefer stage specs, one per stage you touched.** A `<Stage>Spec` test pins the mechanism at the
layer that owns it and runs in milliseconds. Reach for an end-to-end test only for a property no
single stage owns. A fix spanning three stages needs three stage tests, not one on whichever stage
was easiest to assert on — see [/new-stage](new-stage.md) "Test Authoring Rules" for the rule and
for what to do when a stage has no spec file yet.

**When the input shape is unwritable by hand.** If the bug's IR shape is one elaboration now
rejects, a self-contained spec input is impossible by construction. Express the *pre-naming* form
and let the stage's declared `dependencies` build the shape it consumes — that is what dependencies
are for. Say so in a comment, since it deliberately departs from the self-contained-input rule.

**Code-string assertions beat lint.** `assertNoDiff(design.getCompiledCodeString, ...)` is
deterministic and needs no external tool. `.compile.lint` under `options.LinterOptions.WError` will
fail on warnings unrelated to your fix — an `abs`-style design trips `UNUSEDSIGNAL` on the high bit
of every intermediate that is only part-selected.

**Do not copy the reporter's code into the repo.** Issue reports usually carry no license. Write a
minimal design of your own that exercises the same path; if the shape is fully covered by stage
specs, no `issues/iNNN.scala` file is needed at all.

**`assertCompileError` snippets bypass the plugin's pre-typer rewrites.** The quoted snippet is
type-checked via `compiletime.testing.typeCheckErrors` inside the typer, so `PreTyperPhase` (which
fixes `<>` precedence, among others) never sees it. `Bits(8) X 4 <> VAR.SHARED` therefore
mis-associates as `Bits(8) X (4 <> VAR.SHARED)` and reports a bewildering `Required:
IntParam[D]` mismatch instead of the error under test. Parenthesize in snippets — `(Bits(8) X 4)
<> VAR.SHARED` — and expect the same for any other syntax the plugin normalizes before typing.
The same code OUTSIDE a snippet (a positive-control design in the same spec) is unaffected.

### Prove the test fails without the fix

Stash the fix (`git stash push -- <the stage file>`), re-run the new test, confirm it fails, then
`git stash pop`. Do this for **every** regression test you add. It costs one command and it is the
only thing that distinguishes a regression test from decoration.

When the spec's own entry point (`extension ... def <stage>`) lives in the file you stashed, the
test will not compile and the run reports nothing at all — which reads exactly like "no failures".
Revert only the changed guard in place instead, and watch for a silent run.

**A stashed `compiler_ir` / `core` file can leave zinc serving the fix you just removed.** The test
then passes, and the honest reading ("my reproducer is wrong, go find a different shape") sends you
chasing a distinction that does not exist. The tell is a `scala.MatchError: <n> (of class
java.lang.Integer)` from `compileIncremental` on some *other* subproject during the same session —
the same corrupted-incremental-state symptom as after any front-end edit. A
`dotty.tools.dotc.core.Denotations$StaleSymbolException` ("stale symbol ... referred to in run")
while compiling a *downstream* subproject is the same disease, and so is a phantom
`[E046] Cyclic Error ... Cyclic reference involving val <import>` in an untouched `core` file
right after a `compiler_ir` edit — even a body-only one. Run `clean` before trusting a stashed
run, and re-confirm on a clean build before concluding the test does not reproduce.

This is not paranoia. A `<Stage>Spec` asserts on the DFHDL *printout*, and two different IRs can
print identically — the printout is the stage contract precisely because it hides representation.
A conditional-expression fix that removed a leftover ident placeholder and retyped a nested header
produced a byte-identical `assertCodeString` while the unfixed IR went on to fail an ownership
check and crash the Verilog printer. An existing spec test had been covering that exact shape for
as long as the bug existed, passing the whole time.

So when the stage-level assertion cannot see the difference, the regression test belongs in
`PrintVerilogCodeSpec`/`PrintVHDLCodeSpec` instead, where the backend renders what the DFHDL
printer elides. Keep the stage test only if it does fail without the fix.

---

## Checklist

- [ ] Ruled out a **front-end** bug first: does the reproducer even compile? If not, the rest of
      this list does not apply
- [ ] Type-level change: mechanism picked by **when it costs** (match type / `using` / conversion),
      and every `IsConst` guard handed a bare type parameter, never a composition or a `.Out`
- [ ] Every type-level probe run in the scope the code lives in, and controlled against `HEAD`
- [ ] Reproduced with `--nocache --log trace compile --print-backend`; Playground backed up and restored
- [ ] Located the stage that **introduced** the shape, not the one that printed it
- [ ] Checked the other backend (a silent VHDL failure often shadows a loud Verilog one)
- [ ] Checked whether sibling stages sharing a base reproduce it
- [ ] Stated the invariant, and compiled the shape in **every** scope to find its real edges, in
      one `@main` run rather than one at a time
- [ ] Probed each sub-shape for exemptions; confirmed anything the check rejects really miscompiles,
      including the ones that emit HDL silently rather than crashing
- [ ] Check written **first**, and wired into **both** `DB.check` and `SanityCheck` if a stage can violate it too
- [ ] Full suite run with the check in and no stage fixes yet, to measure blast radius
- [ ] Blast radius reported to the user before fixing stages
- [ ] [/new-stage](new-stage.md) invoked before touching any stage
- [ ] Fix is in the offending stage's own patch, never a new phase and never a later cleanup stage
- [ ] Fix is total: no case it declines to handle
- [ ] Regression tests at stage level; no unlicensed code copied in
- [ ] Every new test **verified to fail** with the fix stashed (a stage spec can be blind to it)
- [ ] Both skills updated with anything general learned

---

## Keeping This Skill Up to Date

Add a lesson here when it is about **finding and shaping a fix** — diagnosis technique, where a
rule belongs, how to scope an invariant, how to test it. Lessons about **writing a stage** (patch
mechanics, MetaDesign, IR APIs) belong in [/new-stage](new-stage.md) instead. Keep the split clean
so neither file becomes the dumping ground.

Front-end bugs (typer, macro, inline expansion, anything that fails before an IR exists) also live
here, in their own section, rather than in a skill of their own: the entry point is the same
question, "a bug was reported", and the first move is deciding which species it is.
