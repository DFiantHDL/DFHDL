# `initial` Blocks Plan

> Status: **ALL PHASES (0, A, B, C, D, E) IMPLEMENTED.** Remaining follow-ups: Phase F
> (reset-site fusion), the VHDL init-function form for RT-without-reset non-convertible
> blocks, and an end-to-end simulation test (cycle counts + mid-run reset across the
> wrap-around) once the sim environment allows.
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

## `SplitInitialBlocks` (new stage) — IMPLEMENTED (Phase C)

- `HierarchyStage`; `dependencies: List(ExplicitClkRstCfg)` — reads the resolved
  `@timing.reset` presence off the timing owner's meta, walking the `@timing.related` chain
  like `ToED` does; `nullifies: Set(DropUnreferencedAnons)`. `ToED` adds it to its own
  dependencies in Phase D.
- Transforms an initial block when **(a)** RT-domain and its resolved timing owner has a reset,
  or **(b)** the backend is VHDL.
- **Soundness gate (implemented)**: a block whose statements *read* a declaration that is
  *assigned within the same block* (RHS trees, guards/selectors, TextOut args, and LHS
  selection indexes — but not the LHS target itself) is left untouched — splitting would lose
  the intra-block initialization order. Const-RHS RT content passes trivially.
- **Rule 1 (per-variable split)**: groups = assigned dcls in first-assignment order + one
  residual group for sim-only statements (TextOut). Per group, a keep-list is computed
  (seed statements + transitive in-block value deps + enclosing control flow; conditional
  chains pull predecessors in backwards via `prevBlockOrHeaderRef`, so a group keeps exactly
  the branches on its path — verified by the "conditional split" spec test) and planted via
  `plantClonedMembers` into a fresh initial block created `Before` the original. The original
  block's descendants are removed in the same patch; the (now empty) block itself in a second
  patch phase (the `Before`-anchored Add patch is keyed by the block, so it cannot also carry
  its Remove — same-member patch conflict).
- **Rule 2 (init conversion)**: a block reduced to a single full-width assignment
  (`departialDcl` slice `isFullOf == Tri.Yes`) with const RHS is deleted; the RHS is cloned
  before the Dcl (`cloneAnonValueAndDepsHere` — note: returns `ir.DFVal`, no `.asIR` needed)
  and a Dcl copy with `initRefList = List(clonedInit.refTW[ir.DFVal.Dcl])` replaces it via
  `MetaDesign(dcl, ReplaceWithLast(FullReplacement))`. After this, most RT initial content
  needs zero new ToED logic — the existing init/reset machinery takes over.
- **Rule 3 (VHDL one-shot process, user-revised in Phase D)**: under VHDL, *any* ED-domain
  initial block remaining after Rules 1/2 (sim-only content, cross-reading blocks,
  non-convertible assignment blocks) becomes a `process` (empty sensitivity list) + endless
  `wait` appended `InsideLast` (two patch phases: sensitivity replace, then wait insertion —
  both keyed on the block otherwise), preserving the block's sequential time-zero execution.
  The VHDL printer emits the classic one-shot `process ... wait; end process;` form.
  RT-domain blocks are left for ToED's reset-branch planting; an RT-without-reset
  non-convertible block under VHDL remains a documented gap (VHDL printer `unsupported`).
- Determinism: iterate `subDB.members`; idempotency: a block already assigning exactly one
  variable doesn't re-match the split predicate; post-Rule-3 blocks are no longer initial.
- **Latch-check interaction (Phase C finding)**: `StateAnalysis.getImplicitStateVars` (the
  RT latch check + DF `.prev` analysis) now skips initial blocks entirely — a variable
  assigned only (or partially) inside an initial block is not an implicit state variable, and
  an initial assignment does not "cover" a partial combinational assignment elsewhere.

## `usesRst` participation — IMPLEMENTED (Phase C)

`usesRst` in `DB.scala` refined: the blanket "any RT process block ⇒ uses reset" rule now
excludes initial blocks — an RT `initial` block counts as using reset only when it assigns a
REG (semantically identical to the reg having an init). This breaks the circularity between
reset resolution (`ExplicitClkRstCfg`) and `SplitInitialBlocks`' init conversion (which changes
`hasNonBubbleInit`). `usesClk` similarly excludes initial blocks (a REG the block assigns
already implies a clock via its Dcl).

## `ToED` — IMPLEMENTED (Phase D)

- `SplitInitialBlocks` added to ToED's dependency list (after `ExplicitCondExprAssign`,
  before `AddClkRst`).
- **Exclusion (critical finding)**: `domainOwnerMemberList` groups members by owner *domain*,
  so an initial block's inner members appear in the domain's member list too — without
  exclusion they leaked into `getProcessAllMembers` and were moved into the generated
  process(all)/seq processes. ToED now computes `initialPBs`/`nonInitialMembers` up front and
  runs all combinational/sequential accounting on `nonInitialMembers`.
- **RT initial + reset**: `regInitBlock()` plants clones of each initial block's members
  (assignments converted to non-blocking) after the reg-init entries — works for both sync
  and async reset shapes; the planted blocks (and all their members) are then removed.
  Planting/removal is gated on `plantInitialPBs = hasSeqProcess && rstAnnotOpt.isDefined &&
  initialPBs.nonEmpty` (`hasSeqProcess` hoisted out of the seq MetaDesign for this).
- **RT initial, no reset**: left as-is — exits ToED as an ED initial block (Verilog
  `initial`; under VHDL only the RT-without-reset non-convertible case remains, see Rule 3).
- **RT-stage audit (pulled forward from Phase E — required for pass-through)**:
  `DropRTWaits`, `DropRTProcess`, `FlattenStepBlocks` (all 4 collect sites) now match
  `pb.isInRTDomain && !pb.isInitial`; `SimplifyRTOps.isTransformableForLoop` adds
  `!fb.isInInitialBlock` (a for-loop inside an initial block would otherwise be rewritten to
  a while+iterator-REG and then FSM-ified by Rule 6).
- Tests: `ToEDSpec` "initial block planted into the reset branch" (REG vector for-loop init →
  reset branch, direct REG NB assignment, no `_din` redirection) and "initial block without a
  reset passes through as an ED initial block"; `PrintVHDLCodeSpec` "initial block" (decl init
  + one-shot process end-to-end).

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

## Phase E — IMPLEMENTED (final architecture, user-revised)

The rotation lives in FlattenStepBlocks after all, keyed so the stage remains a fix-point
(`f(f(x)) == f(x)`): the clone is triggered ONLY by the relative `NextStep` wrap goto, which
the same run resolves into a named goto — so a re-run has no trigger left. (Two interim
designs were rejected along the way: an unconditionally-anchored rotation clone, which
re-cloned on every run, and a `WrapGotoTag` carried to DropRTProcess, which the
`NextStep`-trigger insight made unnecessary.) Final division of labor:

- **`isInitialConvertible(members)`** (ProcessBlockAnalysis): blocking const-RHS REG
  assignments + anonymous values are convertible; process-local **Dcls and DFRange members
  are neutral** (SimplifyRTOps leaves the iterator REG dcl, the range bookkeeping, and the
  while-guard func in the prologue region — they must not block conversion, and they are
  never moved either).
- **DropRTWaits — conditional Rule 6**: the bootstrap `S_0` is skipped when the prologue and
  the first step's `onEntry` are initial-convertible. A process *starting* with a step whose
  `onEntry` is non-convertible now gets a bootstrap `S_0` (approved change — `onEntry` fires
  on reset entry instead of being silently lost). **Trailing-share gate**: conversion is
  refused when a trailing statement (relocated by FlattenStepBlocks to the wrap-around exit)
  assigns a prologue-assigned dcl — the wrap-site prologue re-init would shadow it in the
  same cycle (preserves the fork-join start/done handshake's one-cycle low pulse).
  Step-less processes fall under this gate naturally (prologue == trailing); empty
  processes now simply dissolve.
- **FlattenStepBlocks — rotation triggered by the relative `NextStep` (user-final)**: while
  computing Phase 3's goto resolution, the `NextStep` goto whose resolution wraps past the
  last step back to the first (`nextStepMap` path, owning step == last flat step) anchors a
  rotation clone of the prologue's assignment-net closures, bundled into the Phase-0 patch
  call (Move-before-Add ordering keeps relocated trailing statements before the clone).
  **Fix-point holds because the trigger is consumed by the same run**: Phase 3 replaces the
  `NextStep` with a named goto, so a re-run finds no wrap trigger and creates no further
  copies (verified by a double-application spec test). Explicit/`FirstStep` gotos never
  trigger the copy — matching the "prologue runs exactly twice" definition. No tag needed.
- **DropRTProcess**: the prologue's assignment-net closures
  (`net :: net.collectRelMembers`) are CLONED into a generated `initial` block before the
  process and the originals are REMOVED (they must not survive the unwrap as every-cycle
  logic); the first step's convertible `onEntry` is cloned in as well (original stays for
  Rule-2 site inlining); assigned dcls' existing decl inits are stripped ("initial wins")
  with their orphaned anonymous init trees removed; `nullifies` gained
  `DropUnreferencedAnons`. Only fall-through cascades past the last step still plant the
  prologue here (position-based, no tag needed).
- **Patch-system pitfalls found** (documented in the new-stage skill): `plantMembers`
  re-owning + a same-list `Replace(ChangeRefAndRemove)` on the old owner loses the re-own
  (replaceMember's reverse `memberTable` index is not updated by Add patches) — clone
  instead; and Replace patches must precede MetaDesign Adds whose members reference the
  replaced instance (ref-table effects apply in patch-list order).
- Tests: `DropRTWaitsSpec` (no-S_0 for convertible prologue), `FlattenStepBlocksSpec`
  (prologue untouched + wrap resolution), `DropRTProcessSpec` (initial generation from
  prologue and from first-step onEntry; explicit-goto-only processes get init-only
  prologue), `ToEDSpec` end-to-end for-loop payoff (reset init via reset branch, no
  bootstrap state, wrap re-init at the loop-exit goto site, zero extra cycles).

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
| B (DONE) | Verilog printer `initial` (ED path end-to-end; `PrintVerilogCodeSpec` sv2005 + v95 through the full backend pipeline) | docExample + ref update |
| C (DONE) | `SplitInitialBlocks` + `usesRst` extension + StateAnalysis initial-skip | `SplitInitialBlocksSpec` (6 tests: split+convert, non-convertible, no-reset untouched, VHDL sim content, cross-read gate, conditional-chain split) |
| D (DONE) | ToED reset planting / pass-through + RT-stage initial skips | `ToEDSpec` + `PrintVHDLCodeSpec` |
| E (DONE) | Rule 6 conditional + rotation + `DropRTProcess` initial generation | `DropRTWaitsSpec`, `FlattenStepBlocksSpec` (rotation), `DropRTProcessSpec` (prologue + onEntry initial generation), `ToEDSpec` end-to-end for-loop payoff test (sim test still pending) |
| F | reset-site fusion (follow-up) | — |

Each phase goes through the verification ladder (individual specs → `testOnly StagesSpec.*` →
full `test`). After implementation, update `.claude/commands/new-stage.md` and
`.claude/commands/ir-reference.md` with any general lessons (new Sensitivity variant, initial
scope, etc.).
