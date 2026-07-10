# `initial` Blocks Plan

> Status: **approved design; Step 0 (endless `wait`) and Phase A (IR `Sensitivity.Initial`,
> frontend `initial`, elaboration checks, DFHDL printer) IMPLEMENTED** — phases B onward not
> yet implemented (the backend printers already emit Verilog `initial` / VHDL-unsupported as a
> Phase-A side effect, but Phase B's end-to-end docExample + ref updates remain).
> Decisions locked: loop-rotation mechanism (not goto tagging); synthetic `S_0` fallback for
> non-convertible first-step `onEntry` (accepted behavior change); RT initial assignments are
> const-RHS only; both RT and ED initial blocks are semantically once-only.

## Motivation

`DropRTWaits` Rule 6 forces every RT process to start with a step, because the initial
transition from reset cannot be expressed under the RT domain (initials exist only as
declaration `init`). Consequences:

1. A for-loop at the start of a process wastes a cycle: SimplifyRTOps emits `i.din := 0`
   before the while loop, which forces a synthetic `S_0` step.
2. A pure Step-block FSM whose first step has an `onEntry` block loses that `onEntry` when
   entered from reset (`DropRTProcess` only inlines `onEntry` at transition sites).

The fix is a Verilog-like `initial` block that behaves differently between RT and ED domains,
plus dropping/conditioning Rule 6 so prologue code migrates into a generated `initial` block.

## Semantics

- An `initial` block defines **once-only initialization values** in both RT and ED domains.
- Under RT **with a reset**: the block's content is re-applied on every reset assertion (it is
  lowered into the reset branch). This is indistinguishable from once-only precisely because RT
  initial assignments are restricted to constant RHS.
- Under RT **without a reset** and under ED: power-on initialization (declaration inits /
  Verilog `initial` block).
- **RT initial content restriction**: only blocking assignments with constant RHS
  (`dfVal.isConst`), combinational for-loops (iterator indexing on the LHS is fine), and
  conditionals whose guards/`match` selectors are all constant (`dfVal.isConst` — an
  iterator-dependent guard, e.g. a for-comprehension `if` filter, is NOT constant).
  No TextOut, no waits, no `:==`. REG targets are assigned through `.din` as usual (IR-wise a
  `.din` assignment *is* an assignment to the REG Dcl, so nothing special is needed — the
  printer keeps the `.din` form inside `initial` too). The const-RHS rule makes non-blocking
  conversion into reset branches trivially sound (no read-after-write possible) and
  per-variable splitting sound.
- **ED initial content**: blocking assignments, combinational loops/conditionals, TextOut.
  No waits, no `:==`.
- **Conflict rules** (new elaboration checks): a variable may be assigned by at most one
  `initial` block; declaration `init` and `initial`-block assignment are mutually exclusive.
  **Phase A finding**: `DB.check` (which hosts `initialCheck()` in `subDBCheck`) is also run by
  `SanityCheck` after *every* stage — generated IR is checked too. So stages must keep the IR
  conflict-free at every stage boundary: `DropRTProcess` must *move* an existing decl `init`
  (e.g. SimplifyRTOps' `initForced` iterator init) into the generated `initial` block rather
  than leaving both alive for `SplitInitialBlocks` to reconcile later ("initial wins" is thus
  enforced structurally, not as a special stage rule).

## Step 0 — endless `wait` (prerequisite) — IMPLEMENTED

- **IR**: modeled as `ir.Wait` whose trigger is an anonymous `DFBool` const-`false`
  ("block until trigger is true" → never resumes; consistent with existing model). Detection
  helper: `wait.isEndless` extension in `compiler/ir/.../analysis/DFValAnalysis.scala`.
- **Frontend**: `final def wait(using DFC): Unit` in `Wait.ContainerOps`
  (`core/src/main/scala/dfhdl/core/Wait.scala`). Resolution finding: bare `wait` (no parens)
  does NOT auto-apply to Java's `Object.wait()` in Scala 3 (E100 demands explicit `()`), so the
  contextual overload wins cleanly — no plugin rewrite needed. The plugin's `LoopFSMPhase`
  bare-`wait` error guard was changed from a type-shape heuristic (`!tree.tpe.isContextualMethod`,
  which misfired on the fully-applied contextual overload) to a symbol check
  (`tree.fun.symbol.owner == defn.ObjectClass`), so it now errors only on Java's
  `wait()`/`wait(millis)` overloads.
- **Printers** special-case `isEndless`: DFHDL → `wait`, VHDL → `wait;`, Verilog → `wait(0);`.
- **`SimplifyRTOps`**: skips endless waits in both `transformsThisPass` and the bool-trigger
  wait→while rewrite, so they reach `DropRTWaits` intact.
- **`DropRTWaits`**: endless wait becomes a terminal self-looping step
  (`def S_N: Step = ThisStep`) — the FSM halts there. A process ending in an endless wait has
  no wrap-around `NextStep`, so the prologue naturally runs only at initialization.
- **`DropTimedRTWaits`**: matches `DFTime` triggers only — leaves it untouched (verified).
- **Tests**: `SimplifyRTOpsSpec` (pass-through + DFHDL-printer roundtrip), `DropRTWaitsSpec`
  (terminal + only-member cases), `PrintVHDLCodeSpec`/`PrintVerilogCodeSpec` wait tests extended.
  Note: those two printer specs embed hardcoded source positions (a `debug` call's file:line) in
  expected strings — inserting lines above shifts them and they must be updated.

## IR change — IMPLEMENTED (Phase A)

`case object Initial extends Sensitivity` in `ProcessBlock.Sensitivity`
(`compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala`); `prot_=~`, `getRefs`,
`copyWithNewRefs` are trivial (no refs); derived `ReadWriter` on the sealed trait covers
serialization (IR format bump ⇒ DiskCache invalidates via version tag).

Match/extractor sites updated/audited (all done in Phase A):

| Site | Action |
|---|---|
| `compiler/ir/.../printing/DFOwnerPrinter.scala` `csProcessBlock` | prints `initial:` |
| `compiler/stages/.../verilog/VerilogOwnerPrinter.scala` (alwaysKW + senList) | keyword `initial`, no sensitivity suffix (works for all dialects incl. v95/v2001) |
| `compiler/stages/.../vhdl/VHDLOwnerPrinter.scala` senList | `printer.unsupported` (SplitInitialBlocks forced under VHDL) |
| `compiler/ir/.../analysis/ProcessBlockAnalysis.scala` | `isSequential` → false for Initial; added `pb.isInitial` (no getSet needed) + `member.isInInitialBlock` helpers |
| `compiler/stages/.../DropProcessAll.scala`, `VHDLProcToVerilog.scala` | skip naturally (narrow All/List extractors in `collect`) — audited |
| `core/src/main/scala/dfhdl/core/Process.scala` | `Block.initial` + `Ops.initial` constructor |

Note: a `.din` assignment to a REG is IR-wise a plain `DFNet` assignment to the REG Dcl (the
DFHDL printer adds `.din` for any REG-LHS assignment, incl. inside `initial`), so REG
initialization inside `initial` needs no special IR/printer/frontend handling at all.

## Frontend — IMPLEMENTED (Phase A)

- `Scope.Initial extends Local` in `core/src/main/scala/dfhdl/core/DFC.scala`.
- `initial` constructor in `Process.scala` `Ops` (exported via `hdl.scala`), body takes
  `DFC.Scope.Initial ?=> Unit`; guards: `InitialNotDFDomain`, `InitialNotInsideProcess`,
  `NoNestingInitial` (the latter also added to all three `process` constructors so processes
  can't nest inside `initial`).
- Inside `Scope.Initial`: steps/waits-as-steps won't compile (they require `Scope.Process`);
  `:==` is blocked by a `NotInInitial:==` guard in `DFVarOps` — **placed before**
  `InsideProcess:==` in the using-parameter list so its message wins (given-resolution reports
  the first failing `AssertGiven` in declaration order); `InsideProcess:=` extended with
  `DFC.Scope.Initial` in its union so ED-domain `:=` works inside `initial`; in RT, REG
  initialization uses the regular `x.din := const` form (same IR as any `.din` assignment;
  the printer keeps `.din`).
- Elaboration checks (backstop for scope laundering through `def`s): `initialCheck()` in
  `DB.scala`, part of `subDBCheck` (per-design), covering the content restrictions
  (RT: const-RHS blocking assignments + for-loops + const-guarded/const-selector
  conditionals + iterator dcls only; ED: also non-const conditionals, while loops, text
  output, local dcls; both: no NB assignments, connections, waits, gotos, or nested owners) and the conflict rules (one initial block per dcl; decl
  `init` XOR initial assignment; dcls local to the initial block are exempt).
- Tests: `core/CoreSpec.InitialSpec` (5 compile-guard errors), `StagesSpec.PrintCodeStringSpec`
  ("initial block printing under ED/RT", the RT one incl. a const-guard `if`),
  `lib/ElaborationChecksSpec` (RT content errors + conflict errors + non-const
  conditional guard/selector errors).

## `SplitInitialBlocks` (new stage)

- `dependencies: List(ExplicitClkRstCfg)` (reads resolved `@timing.reset` presence off the
  timing owner's meta, like `AddClkRst` does); `ToED` adds it to its own dependencies.
- Transforms an initial block when **(a)** RT-domain and its resolved timing owner has a reset,
  or **(b)** the backend is VHDL.
- **Rule 1 (per-variable split)**: for each assigned variable, clone the block
  (`plantClonedMembers`), keep only that variable's assignments plus enclosing control flow,
  drop unreferenced anons. Sound because of the const-RHS / no-cross-read restrictions.
- **Rule 2 (init conversion)**: a block reduced to a single full-width assignment
  (`departialDcl` slice full) with const RHS is deleted and becomes `initRefList` on the Dcl.
  After this, most RT initial content needs zero new ToED logic — the existing init/reset
  machinery takes over. "Initial wins" over an existing generated decl init.
- **Rule 3 (VHDL sim content)**: residual non-assignment statements (asserts/prints in ED
  initial blocks) become a `process.forever` terminated by the endless `wait` — the VHDL
  printer emits the classic one-shot `process ... wait; end process;`. Only multi-statement
  per-variable *assignment* blocks still need the VHDL init-function form
  (function generation plugs into the declarations region assembled at
  `VHDLOwnerPrinter.scala:162-166`).
- Determinism: iterate `designDB.members`; idempotency: a block already assigning exactly one
  variable doesn't re-match the split predicate.

## `usesRst` participation

Extend `usesRst` (`compiler/ir/src/main/scala/dfhdl/compiler/ir/DB.scala:1050-1062`): a domain
containing an RT `initial` block that assigns a REG counts as using reset (semantically
identical to the reg having an init). This breaks the circularity between reset resolution
(`ExplicitClkRstCfg`) and `SplitInitialBlocks`' init conversion (which changes
`hasNonBubbleInit`).

## `ToED`

At `ToED.scala:264-328`:

- **RT initial + reset**: plant the (post-split, non-converted) initial blocks' members into
  the reset-active branch after `regInitBlock()`'s entries (`ToED.scala:271-274`), converting
  assignments to non-blocking (`:==`). Works for both sync (`:317`) and async (`:322`) shapes.
- **RT initial, no reset**: leave the block as-is — exits ToED as an ED initial block (Verilog
  `initial`; under VHDL nothing remains, since condition (b) split everything).
- Exclude initial blocks from `processBlockAllMembers` / `hasSeqProcess` accounting so they
  don't trigger a bogus sequential process.

## RT process improvement (the payoff)

### Prologue semantics — precise definition

The prologue (statements before the first step, plus the first step's `onEntry`) runs in
exactly two situations:

1. **Initialization** — via the `initial` block that `DropRTProcess` generates (a plain copy —
   `.din` assignments to REGs are kept as-is, since a `.din` assignment is IR-wise an
   assignment to the REG Dcl). With a reset this lands in the reset branch; without one, as
   declaration inits / power-on initial block.
2. **Forever wrap-around** — when the *last* step's *implicit* `NextStep` rolls back to the
   first step, a copy of the prologue executes on that exit path.

It does **not** run on explicit `FirstStep`/`ThisStep`/named gotos targeting the first step.
A step-less RT process has no prologue: its whole body remains every-cycle logic.

### Mechanism: loop rotation (approved)

By `DropRTProcess` time, FlattenStepBlocks Phase 3 has resolved every relative goto — a
wrap-around `NextStep` is then indistinguishable from a user-written `FirstStep` goto in the
last step. Therefore the wrap-around copy is done **in FlattenStepBlocks**, as a Phase-0
sibling sub-phase: clone the prologue statements (`plantClonedMembers`, including anon deps) to
just before the wrap-around `NextStep` goto of `deepestLastChild(lastStep)`, found with
`findNextStepGoto` (which correctly lands inside the else-branch of a while-shaped last step,
so `ThisStep` loopbacks don't re-run it). Classic loop rotation:
`forever { P; S1..Sn }` ≡ `initial P; loop { S1..Sn; P }`.

Fusion bonus: at a fused first step, the rotated `i.din := 0` becomes a pending assignment for
FirstStepFusion's value forwarding, so the loop-restart guard folds statically — the forever
restart costs **zero** cycles (better than today's one-cycle `S_0` pass).

`DropRTProcess` then only handles case 1: build the `initial` block (MetaDesign sibling before
the process) from the leading prologue members + a clone of the first step's `onEntry` members,
and `Patch.Remove` the prologue originals — they must not survive the `pbPatch` unwrap as
every-cycle domain members. Regular `onEntry` site inlining (Rule 2) continues unchanged.

### Convertibility gate & fallback (replaces unconditional Rule 6)

`isInitialConvertible` (shared analysis helper, used by `DropRTWaits` to decide and
`DropRTProcess` to assert): every prologue/`onEntry` member is an anonymous dep or a blocking
assignment through `.din` to a REG with `dfVal.isConst` RHS.

- Not convertible (non-const RHS, prints, non-REG targets) → `DropRTWaits` keeps today's
  synthetic `S_0`; nothing else changes (the fallback **is** the status quo).
- **Approved behavior change**: a process starting directly with a step whose `onEntry` is
  non-convertible today silently loses `onEntry` at reset; the fix adds an `S_0` bootstrap
  (one extra cycle, correct semantics). Document in release notes.

### Edge cases (defined)

- **Last step ends with an explicit goto / endless wait** (no `NextStep` wrap): no wrap path;
  prologue runs only at initialization; no rotation clone is placed.
- **Single-step process**: wrap = self `NextStep`; rotation still applies (clone lands before
  that goto) — prologue runs at init and every loop-back; one FSM state, no wasted cycle.
- **Step-less process** (`process.forever { x.din := y }` with no waits): `stateBlocks.isEmpty`
  → `DropRTProcess` keeps today's plain unwrap (every-cycle logic); no initial conversion.
- **RT-stage audit**: `SimplifyRTOps`, `DropTimedRTWaits`, `DropRTWaits`, `FlattenStepBlocks`,
  `DropRTProcess`, and ToED's domain partitioning all skip `Sensitivity.Initial` blocks and
  their members — for-loops inside `initial` must survive untransformed.

## Follow-up (separate effort, not in scope)

Teach FirstStepFusion a "reset virtual site": fuse a candidate first step by evaluating its
dispatch with initial values as the pending assignments, setting the FSM's `stateInit` to the
statically-resolved target — eliminates the remaining bootstrap cycle for loop-first FSMs.
Also consider fusing steps whose only non-regular child is an initial-convertible `onEntry`
(today `hasNonRegularChild` disqualifies them).

## Sequencing

| Phase | Content | Gate |
|---|---|---|
| 0 (DONE) | endless `wait` (IR-less; frontend + printers + DropRTWaits/DropTimedRTWaits) | specs + full test |
| A (DONE) | IR `Sensitivity.Initial`, `Scope.Initial`, frontend `initial`, elaboration checks, DFHDL printer | core/elab error tests + PrintCodeString |
| B | Verilog printer `initial` (ED path end-to-end) | docExample + ref update |
| C | `SplitInitialBlocks` + `usesRst` extension | `SplitInitialBlocksSpec` (self-contained: write initial-block IR directly) |
| D | ToED reset planting / pass-through | `ToEDSpec` |
| E | Rule 6 conditional + rotation + `DropRTProcess` initial generation | `DropRTWaitsSpec`, `FlattenStepBlocksSpec` (nested + prologue matrix), `DropRTProcessSpec`, end-to-end sim test: cycle counts + mid-run reset across the wrap-around |
| F | reset-site fusion (follow-up) | — |

Each phase goes through the verification ladder (individual specs → `testOnly StagesSpec.*` →
full `test`). After implementation, update `.claude/commands/new-stage.md` and
`.claude/commands/ir-reference.md` with any general lessons (new Sensitivity variant, initial
scope, etc.).
