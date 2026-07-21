# Initial Blocks

How `initial` blocks provide once-only initialization in the RT and ED domains, how each
backend lowers them, and how they eliminate the RT process bootstrap cycle by absorbing the
process prologue.

This document covers the IR model, the frontend surface and its guards, the elaboration
checks, the per-backend lowering pipeline (`SplitInitialBlocks`, `DropInitialBlocks`,
`ToED`), and the RT process prologue optimization built on top. The user-facing side is
covered in [docs/user-guide/processes/index.md](../docs/user-guide/processes/index.md)
(semantics, cycle rules) with the generated-static-function form cross-referenced from
[docs/user-guide/methods/index.md](../docs/user-guide/methods/index.md).

Related: [methods.md](methods.md) for the static-function model that the VHDL init-function
lowering generates into, and [scoping.md](scoping.md) for the capability lattice that
`Scope.Initial` participates in.

## 1. Semantics

An `initial` block defines **once-only initialization values** in both RT and ED domains.

- **RT with a reset**: the content is re-applied on every reset assertion (lowered into the
  register reset branch). This is indistinguishable from once-only precisely because RT
  initial content is restricted to constant values.
- **RT without a reset, and ED**: power-on initialization (declaration inits, a Verilog
  `initial` block, or the VHDL forms of §5).

Content rules, enforced at elaboration (§3):

| | RT | ED |
|---|---|---|
| blocking assignments | constant RHS only | any RHS |
| `for` loops | yes (iterator LHS indexing is fine) | yes |
| `while` loops | no | yes |
| conditionals | constant guards/selectors only (an iterator-dependent guard is NOT constant) | any |
| text output | no | yes |
| local dcls | iterator dcls only | yes |
| `:==`, waits, connections, gotos, nested owners | no | no |

Conflict rules: a declaration may be assigned by at most ONE `initial` block, and a
declaration `init` and an `initial`-block assignment are mutually exclusive (dcls local to
the block are exempt).

REG targets are assigned through `.din` as usual: a `.din` assignment is IR-wise a plain
`DFNet` assignment to the REG Dcl, so `initial` needs no special IR, frontend, or printer
handling for REGs (the DFHDL printer keeps the `.din` form inside `initial` too). The
const-RHS rule is what makes non-blocking conversion into reset branches trivially sound (no
read-after-write is possible) and per-variable splitting sound.

## 2. IR and analysis

`case object Initial extends Sensitivity` in `ProcessBlock.Sensitivity`
([DFMember.scala](../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala)). It
carries no refs, so `prot_=~`, `getRefs`, and `copyWithNewRefs` are trivial, and the derived
`ReadWriter` on the sealed trait covers serialization.

Analysis helpers
([ProcessBlockAnalysis.scala](../compiler/ir/src/main/scala/dfhdl/compiler/analysis/ProcessBlockAnalysis.scala)):

| Helper | Purpose |
|---|---|
| `pb.isInitial` | sensitivity match (no getSet needed) |
| `pb.isSequential` | false for `Initial` |
| `member.isInInitialBlock` | ownership walk up to the nearest process |
| `pb.hasResolvedRstCfg` | resolved `@timing.reset` presence, walking the `@timing.related` chain (decides reset-branch vs decl-init lowering) |
| `assignedDcls(members)` | the dcls a block assigns, in first-assignment order (the grouping key for splitting/conversion) |
| `isInitialConvertible(members)` | whether an RT process prologue (given as its full flattened content) can lower into a generated initial block (§6) |
| `initialConvertibleMoveList(prologue)` | the exact statement-closure subset of the prologue that moves into the generated block; shared by `FlattenStepBlocks` (rotation clone) and `DropRTProcess` (§6) |

Interactions with existing analyses:

- **`usesRst`/`usesClk`** ([DB.scala](../compiler/ir/src/main/scala/dfhdl/compiler/ir/DB.scala)):
  the blanket "any RT process block uses reset" rule excludes initial blocks. An RT `initial`
  counts as using reset only when it assigns a REG (semantically identical to the reg having
  an init), and never counts as using a clock. This breaks the circularity between reset
  resolution (`ExplicitClkRstCfg`) and the decl-init conversion, which changes
  `hasNonBubbleInit`.
- **`StateAnalysis.getImplicitStateVars`** (the RT latch check and DF `.prev` analysis) skips
  initial blocks entirely: a variable assigned only (or partially) inside one is not an
  implicit state variable, and an initial assignment does not "cover" a partial combinational
  assignment elsewhere.

## 3. Frontend and elaboration checks

- `Scope.Initial extends Local` in [DFC.scala](../core/src/main/scala/dfhdl/core/DFC.scala);
  the `initial` constructor in [Process.scala](../core/src/main/scala/dfhdl/core/Process.scala)
  takes a `DFC.Scope.Initial ?=> Unit` body. Guards: `InitialNotDFDomain`,
  `InitialNotInsideProcess`, `NoNestingInitial` (the last also sits on all three `process`
  constructors, so processes cannot nest inside `initial` either).
- Inside `Scope.Initial`, steps and waits do not compile (they require `Scope.Process`);
  `:==` is blocked by a `NotInInitial` guard in `DFVarOps`, placed BEFORE the broader
  `InsideProcess` guard in the using-parameter list because given-resolution reports the
  first failing `AssertGiven` in declaration order; `InsideProcess:=` includes
  `DFC.Scope.Initial` in its union so ED-style `:=` works inside `initial`.
- **`initialCheck()`** in `DB.scala` (part of `subDBCheck`) is the backstop for scope
  laundering through helper `def`s: it enforces the §1 content and conflict rules per design.
  Because `SanityCheck` runs `DB.check` after every stage, generated IR is checked too, so
  every stage must keep the IR conflict-free at its boundary. This is why `DropRTProcess`
  STRIPS an existing decl init when it generates an initial block for the same dcl (§6):
  "initial wins" is enforced structurally, not as a special stage rule.

Tests: `CoreSpec.InitialSpec` (compile guards), `ElaborationChecksSpec` (content and conflict
errors), `PrintCodeStringSpec` (ED/RT printing).

## 4. The endless `wait`

A bare `wait` (no duration or condition) is modeled as `ir.Wait` whose trigger is an
anonymous `DFBool` const-`false`: "block until the trigger is true" never resumes, which is
consistent with the existing wait model and needs no new IR. Detection is `wait.isEndless`
(DFValAnalysis).

- **Frontend**: `def wait(using DFC): Unit` in `Wait.ContainerOps`. Bare `wait` does NOT
  resolve to Java's `Object.wait()` in Scala 3 (E100 demands explicit `()`), so the
  contextual overload wins with no plugin rewrite. The plugin's `LoopFSMPhase` bare-`wait`
  error guard keys on the symbol owner (`defn.ObjectClass`), erroring only on Java's
  overloads.
- **Printing**: DFHDL `wait`, VHDL `wait;`, Verilog `wait(0);`.
- **Stages**: `SimplifyRTOps` skips endless waits (both in `transformsThisPass` and the
  bool-trigger wait-to-while rewrite); `DropTimedRTWaits` matches `DFTime` triggers only;
  `DropRTWaits` turns an endless wait into a terminal self-looping step
  (`def S_N: Step = ThisStep`), halting the FSM. A process ending in an endless wait has no
  wrap-around, so its prologue (§6) runs only at initialization.

## 5. Lowering pipeline

Terminal forms by context:

| Context | Terminal form |
|---|---|
| RT with reset (any backend) | decl `init` (single-constant) or content planted into the reset branch by `ToED` |
| RT without reset / ED, Verilog | `initial begin ... end` (all dialects; no sensitivity suffix) |
| RT without reset / ED, VHDL, single-constant | decl default (`signal v : t := ...`) |
| RT without reset / ED, VHDL, constant multi-statement | decl default computed by a generated static function |
| ED, VHDL, everything else | one-shot `process ... wait; end process;` |

### `SplitInitialBlocks`

Backend-agnostic normalization
([SplitInitialBlocks.scala](../compiler/stages/src/main/scala/dfhdl/compiler/stages/SplitInitialBlocks.scala)),
a `HierarchyStage` depending on `ExplicitClkRstCfg` (it reads the resolved reset off the
timing owner's meta). It triggers when the block is RT with a resolved reset, or the backend
is VHDL.

- **Cross-read gate**: a block whose statements READ a declaration ASSIGNED within the same
  block (RHS trees, guards/selectors, TextOut args, and LHS selection indexes, but not the
  LHS target itself) is left whole; splitting would lose the intra-block initialization
  order. Const-RHS RT content passes trivially.
- **Rule 1 (per-variable split)**: one group per assigned dcl in first-assignment order,
  plus a residual group for simulation-only statements. Each group's keep-list is the seed
  statements, their transitive in-block value deps, and their enclosing control flow
  (conditional chains pull predecessors in backwards via `prevBlockOrHeaderRef`, so a group
  keeps exactly the branches on its path). Groups are re-emitted as fresh per-declaration
  initial blocks via `plantClonedMembers`; the original block and its members are removed in
  the same bundled patch.
- **Rule 2 (decl-init conversion)**: a group reduced to a single full-width constant
  assignment (`departialDcl` slice `isFullOf == Tri.Yes`, const RHS) is deleted; the RHS is
  cloned before the dcl and a dcl copy carrying `initRefList` replaces it
  (`MetaDesign(dcl, ReplaceWithLast(FullReplacement))`). From here the existing init/reset
  machinery takes over.

### `DropInitialBlocks`

VHDL-only elimination of whatever remains
([DropInitialBlocks.scala](../compiler/stages/src/main/scala/dfhdl/compiler/stages/DropInitialBlocks.scala)):
`runCondition = backend.isVHDL`, depends on `SplitInitialBlocks`. `ToED` lists BOTH stages in
its dependencies, because a `runCondition`-skipped stage does not pull in its own
dependencies. RT blocks with a resolved reset are skipped (they are `ToED`'s, below).

- **Init-function conversion**: a single-declaration block computable from constants alone
  becomes a generated STATIC FUNCTION: a `Def` design in the Static domain built raw inside
  a MetaDesign, holding phantom input formals for captured design-local constants, a local
  variable (named like the dcl) that the cloned body assigns, and an `IdentTag`'d ident of
  it connected to an `o` output port. The declaration's `init` becomes the anonymous
  `Func`/`Op.Def` call carrying the captures as actuals. Reads of the initialized
  declaration redirect to the local variable, so SELF-READING blocks
  (`v := 0; v := v + 1`) convert faithfully: the split's cross-read gate does not apply to
  the function form, whose local variable preserves the in-block order. The def design is
  then EXTRACTED into its own sub-DB, which is why this stage manages the sub-DB map itself
  instead of extending `HierarchyStage`; the full mechanics (never enter a design block in a
  MetaDesign, raw ref minting, the `ownerRef -> DFMember.Empty` head binding, refTable
  partitioning) are recorded as Pattern 14 in
  [.claude/commands/new-stage.md](../.claude/commands/new-stage.md).
- **One-shot process conversion**: any ED-domain block the function conversion cannot take
  (simulation-only content, non-constant reads, cross-reading multi-declaration blocks)
  becomes a `process` with an empty sensitivity list terminated by an endless `wait`,
  printed as the classic one-shot form.
- An RT-without-reset block the function conversion cannot take is left untouched and the
  VHDL printer rejects it (see Open issues).

### `ToED`

- **Member accounting**: `domainOwnerMemberList` groups members by owner DOMAIN, so an
  initial block's inner members appear in the domain's member list too. `ToED` computes
  `initialPBs`/`nonInitialMembers` up front and runs all combinational/sequential accounting
  on `nonInitialMembers`; without this, initial content leaks into the generated
  process(all)/sequential processes.
- **RT initial + reset**: `regInitBlock()` plants clones of each initial block's members
  (assignments converted to non-blocking) after the reg-init entries, for both sync and
  async reset shapes, then removes the planted blocks. Gated on
  `hasSeqProcess && rstAnnotOpt.isDefined && initialPBs.nonEmpty`.
- **RT initial, no reset**: passes through and exits `ToED` as an ED initial block.

### RT-stage pass-through audit

`SimplifyRTOps` (`isTransformableForLoop` adds `!fb.isInInitialBlock`), `DropRTWaits`,
`FlattenStepBlocks`, `DropRTProcess`, and `ToED`'s domain partitioning all skip
`Sensitivity.Initial` blocks and their members. A for-loop inside `initial` must survive
untransformed (otherwise it would be rewritten to a while + iterator REG and FSM-ified).

## 6. The RT process prologue

The payoff that motivated the feature: the statements before a process's first step, plus
the first step's `onEntry` body, form the **prologue**, and it runs in exactly two
situations:

1. **Initialization**: via the `initial` block `DropRTProcess` generates. With a reset this
   lands in the reset branch; without one, as declaration inits or a power-on initial block.
2. **Forever wrap-around**: when the last step's IMPLICIT `NextStep` rolls back to the first
   step, a clone of the prologue executes on that exit path.

It does NOT run on explicit `FirstStep`/`ThisStep`/named gotos targeting the first step. A
step-less process has no prologue: its body stays every-cycle logic.

Division of labor, each stage remaining a fix-point (`f(f(x)) == f(x)`):

- **`isInitialConvertible(members)`**: every member is an anonymous value dependency, a
  blocking const-RHS assignment to a REG (the `.din` form), a
  combinational (`COMB_LOOP`) for loop with constant range bounds, or a conditional with
  constant guards/selectors. Text output and while loops are NOT convertible (prints are
  rejected in RT `initial` blocks; a non-constant while guard breaks the const model), so a
  printing or comb-while prologue keeps the bootstrap step. The vetting runs on
  the region's FULL FLATTENED content, so owner contents are vetted individually and a
  step/wait hiding inside a conditional fails the check. Process-local Dcls and DFRange
  members are NEUTRAL: `SimplifyRTOps` leaves the iterator REG dcl, range bookkeeping, and
  the while-guard func in the prologue region; they must not block conversion and are never
  moved. The moved subset is computed by **`initialConvertibleMoveList(prologue)`**: the
  statement closures (nets, comb for loops with iterator/range bookkeeping,
  const-guard conditional chains, each with contents and anonymous deps); it is the single
  source of truth for BOTH the rotation clone and the initial-block generation.
- **`DropRTWaits` (conditional Rule 6)**: the synthetic bootstrap `S_0` is skipped when the
  prologue and the first step's `onEntry` are initial-convertible. The folded prologue and
  trailing regions are EXPANDED (owner + flattened members) before vetting, matching the
  flattened contract above. A process starting with a
  step whose `onEntry` is NOT convertible gets a bootstrap `S_0` (deliberate behavior
  change: `onEntry` fires on reset entry instead of being silently lost). The
  **trailing-share gate** refuses conversion when a trailing statement (relocated by
  `FlattenStepBlocks` to the wrap-around exit) assigns a prologue-assigned dcl, since the
  wrap-site re-init would shadow it in the same cycle (this preserves the fork-join
  start/done handshake's one-cycle low pulse). Step-less processes fall under this gate
  naturally; empty processes dissolve.
- **`FlattenStepBlocks` (loop rotation)**: `forever { P; S1..Sn }` is rotated to
  `initial P; loop { S1..Sn; P }`. The prologue clone is anchored ONLY at a relative
  `NextStep` goto whose resolution wraps past the last step back to the first. The fix-point
  holds because the trigger is consumed by the same run: Phase 3 replaces the `NextStep`
  with a named goto, so a re-run finds no wrap trigger (guarded by a double-application spec
  test). Explicit and `FirstStep` gotos never trigger the copy, matching the
  runs-exactly-twice definition. Fusion bonus: at a fused first step the rotated
  `i.din := 0` becomes a pending assignment for `FirstStepFusion`'s value forwarding, so the
  loop-restart guard folds statically and the forever restart costs zero cycles. The wrap
  goto the rotation plants inside the FIRST step's own exit branch is a self-goto; the
  fusion validation explicitly allows it for the process's first step only (expansion
  resolves the re-entry by constant pruning on the re-initialized values, and a genuinely
  dynamic re-entry still falls back via the expansion visit limit).
- **`FirstStepFusion` (reset-site fold)**: after every jump site is inlined, a fused first
  step survives only as the one-time reset bootstrap state. When its dispatch const-folds
  under the prologue's pending values (single path: every guard resolves statically, every
  emitted statement is a const-RHS full REG assignment, the fold ends at a goto to the
  member-order-next step, and nothing still jumps to the bootstrap), the folded assignments
  are appended to the prologue (so `DropRTProcess` lowers them into the generated `initial`
  block) and the bootstrap state is removed. The FSM then resets directly into the fold's
  target state: a waiting loop costs exactly its wait cycles with zero bootstrap cycles, so
  `wait(100.us)`, `for(...) wait(...)`, and nested-loop equivalents finish at identical
  simulation times.
- **`DropRTProcess`**: clones the prologue's statement closures (via
  `initialConvertibleMoveList`) into a generated `initial` block before the process and
  REMOVES the originals (they must not survive the process unwrap as every-cycle logic);
  clones the first step's convertible `onEntry` in as well (the original stays for the
  regular transition-site inlining); strips assigned dcls' existing decl inits together
  with their orphaned anonymous init trees ("initial wins", required by the every-stage
  `initialCheck`, §3). `COMB_LOOP` tags are stripped on the initial-block clones (the
  content runs once; the marker is process-only), via `plantClonedMembers`' transform
  parameter, since a pre-mapped owner copy would break the clone map's ownership matching
  and `mutableDB.setMember` is a no-op in meta-programming mode. The in-process rotation
  and fall-through clones keep the tag. Only fall-through cascades past the last step
  still plant the prologue here (position-based).

Edge cases: a last step ending in an explicit goto or endless wait has no wrap path, so no
rotation clone is placed and the prologue runs only at initialization; a single-step process
wraps via its self `NextStep`, so rotation applies and the prologue runs at init and every
loop-back with one FSM state and no wasted cycle.

Tests: `DropRTWaitsSpec` (incl. the comb-for convertible prologue and the textout/comb-while
bootstrap fallbacks), `FlattenStepBlocksSpec` (rotation and its fix-point, incl. the
loop rotation clone and the first-step wrap self-goto fusion + reset-site fold with its
fix-point), `DropRTProcessSpec` (incl. the comb-loop and const-guard conditional
initial-block generation and the single-state waiting-loop FSM), and the `ToEDSpec`
end-to-end for-loop payoff (reset provides the iterator and first output values, single
fused state, wrap re-init at the loop-exit site, one cycle per iteration).

## 7. Printing

- **DFHDL**: `csProcessBlock` prints `initial:`. REG-LHS assignments keep `.din` inside
  `initial` like everywhere else.
- **Verilog**: the `initial` keyword with no sensitivity suffix, valid in all dialects
  including v95/v2001.
- **VHDL**: `csProcessBlock` rejects `Sensitivity.Initial` (`printer.unsupported`) by
  construction; `DropInitialBlocks` eliminates the blocks beforehand. The architecture
  declarative region orders constants, then static functions, then signal/variable
  declarations, then ED methods, because a signal default may CALL a generated init function
  while a static function only reads constants (and an ED method may read signals).
- Note for spec upkeep: `PrintVHDLCodeSpec`/`PrintVerilogCodeSpec` embed hardcoded source
  positions (a `debug` call's file:line) in some expected strings; inserting lines above
  them shifts the positions.

## 8. Where each rule is enforced

| Rule | Enforcement |
|---|---|
| no `initial` in DF / inside a process / nested | frontend guards (`InitialNotDFDomain`, `InitialNotInsideProcess`, `NoNestingInitial`) |
| no steps/waits in `initial` | type-level (they require `Scope.Process`) |
| no `:==` in `initial` | `NotInInitial` guard in `DFVarOps` (declared before `InsideProcess`) |
| content rules (RT const-only, ED extensions) | `initialCheck()` at elaboration, re-checked by `SanityCheck` after every stage |
| one initial per dcl; decl `init` XOR initial | `initialCheck()` conflict rules |
| reset-vs-decl-init lowering choice | `pb.hasResolvedRstCfg` in the lowering stages |
| VHDL never prints `initial` | `DropInitialBlocks` (with `VHDLOwnerPrinter` `unsupported` as the safety net) |

## 9. Open issues

### Pending work

1. **Fusing steps with an initial-convertible `onEntry`.** Reset-site fusion ("Phase F") is
   IMPLEMENTED (see §6: `FirstStepFusion` reset-site fold), so a fused loop-first FSM pays
   zero bootstrap cycles. What remains from the original item: consider fusing steps whose
   only non-regular child is an initial-convertible `onEntry` (today `hasNonRegularChild`
   disqualifies them).
2. **Init-function form for design-local parameterized types.** `DropInitialBlocks` gates
   out a block whose declaration type or captured constants reference design-local values
   (e.g. `SInt(W) X D <> VAR` with `W` a design parameter). Under ED it falls back to the
   one-shot process; under RT without a reset it stays `initial` and the VHDL printer
   rejects it, the one remaining unsupported combination. Full support means extending the
   phantom-formal mechanism to TYPE references (the local variable's and out port's types
   referencing the phantom formals).
3. **Simulation validation.** No `testApps` case exercises any lowering path: Verilog
   `initial`, VHDL decl-init, the generated init function, the one-shot process, or the
   reset-branch planting. The wait-equivalence rules were verified MANUALLY (2026-07-21,
   questa): `wait(100.us)`, `for(10) wait(10.us)`, and `for(10) for(10) wait(1.us)` all
   finish at the identical simulation time, but no automated simulation coverage exists
   (prologue re-init across a mid-run reset at the wrap-around remains unverified);
   DFacsimile cannot host it yet (no process-block support), so an external-simulator
   `testApps` case is the practical path.

### Unverified corners

- **Generated-name collisions.** The init function is named `<dcl>_init`. Elaborated designs
  get same-dclName enumeration through the design-load machinery, but the stage-created def
  bypasses that path; a user method or design named `<dcl>_init` in the same design has not
  been tested.

### Out of scope

Static variables inside initial blocks (falls out of the folding interpreter, see
[static-function-eval-plan.md](static-function-eval-plan.md), plus the static-scope rules);
initial blocks in the DF domain (initialization there is declaration `init` by design).
