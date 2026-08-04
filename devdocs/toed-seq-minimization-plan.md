# ToED Sequential-Process Maximization Plan

> Status: Phases A and B IMPLEMENTED (2026-08-04): `NameVarVersions` (position capture) and the
> `ToED` slicing rework are in, with `NameVarVersions` a `ToED` dependency. Phase C (Rule 10)
> remains deferred. Replaces the narrower "shared-variable write capture" follow-up of issue
> #437 with a general lowering discipline. See "Implementation notes" at the bottom for
> decisions made during implementation.

## Motivation

`ToED` currently makes an all-or-nothing decision per RT domain: if every remaining
(non-single-assignment) write targets a REG or a shared variable, the whole body lowers into one
clocked process (`domainIsPureSequential`); otherwise the *entire* body lands in `process(all)`
and the clocked process only commits `<reg> :== <reg>_din` shadows. The fallback is wasteful
(shadow signals and a large combinational process for bodies that are almost fully sequential)
and, for shared-variable writes, wrong outright: a RAM write dragged into `process(all)` is a
level-sensitive write that `DB.sharedVarCheck` now rejects.

The goal: emit into the clocked process everything that *can* be sequential, and keep a
**minimal** `process(all)` residue only for what is genuinely combinational. The two historical
blockers, named explicitly, are:

1. **Shared conditional skeletons**: one `if`/`match` guarding both combinational and
   sequential assignments.
2. **Combinational-to-sequential value dependencies**: a sequential assignment reading an
   intermediate whose *positional* (blocking) value differs from its *settled* (end-of-process)
   value once the reader moves to another process.

Both are solved below by exactly two mechanisms: **skeleton duplication** and **position
capture**.

## Semantic model

An RT domain body is a sequential program executed once per clock step:

* plain `VAR`/port assignment: immediate (blocking) value, combinationally observable outside
  the domain;
* `REG.din` write: next-state value, committed at the step end;
* shared-variable write: committed at the step end (issue #437 discipline);
* a read observes the latest prior assignment within the same step (else the settled value of
  the previous step's combinational network, i.e. the signal).

Cross-process reads in the lowered form always observe **settled** values (a signal read by the
clocked process at the edge is the fully-propagated combinational value). The lowering is
correct iff every moved statement reads values whose settled value equals the value at the
statement's original position.

## Definitions

* **Sequential sink**: a `REG.din` write or a shared-variable write. (Open question: text
  output and assertions, see below.)
* **Combinational sink**: an assignment to a non-REG, non-shared variable or port (its settled
  value is the domain's combinational output), and any value read concurrently or by another
  domain.
* **reads(S)**: the named declarations reachable from statement S's references through
  anonymous expression members (the anonymous cone bottoms at named values).
* **Externally settled value**: never assigned in this domain body (inputs, other-domain
  signals, constants, parameters). Always safe to read from either process.
* **Settled at position P**: a domain-assigned value v is settled at P iff no assignment to v
  appears at any program position after P (syntactic, path-insensitive, conservative).
* **Directly movable statement**: a sequential-sink statement S at position P such that every
  v in reads(S) is externally settled or settled at P. The same test applies to every guard
  and `match` selector on S's skeleton path, each at its own position.

## The rules

### Rule 1: slice by sink

Every assignment statement belongs to the **seq slice** iff its target (via `departialDcl`) is
a sequential sink; otherwise to the **comb slice**. Conditional skeletons (if/match headers and
blocks) belong to whichever slices their contents belong to, possibly both.

### Rule 2: skeleton duplication

A skeleton with mixed content is emitted **twice**: the comb copy holds only comb-slice
statements, the seq copy (inside the clocked process, at the same relative order) holds only
seq-slice statements. Branches left empty in a copy are dropped; a skeleton whose content is
entirely one slice is emitted once, in that slice's process. Guards and selectors are
re-emitted per copy (cloned anonymous cones), subject to the settled-read test at their
positions.

### Rule 3: settled-read soundness

A seq-slice statement (and each guard on its path) moves as-is when directly movable: its
operands are read in the clocked process either as inputs or as settled comb signals. This is
the common case (e.g. `if (we) ram(addr) := data` with `we/addr/data` inputs lowers to
`if (we) ram(addr) :== data` inside the clocked process, with no helper signals at all).

### Rule 4: position capture (the universal fallback)

When a read v in reads(S) (or in a guard cone) violates settledness at P, insert a fresh comb
variable `v_cap` with:

* a constant default at the top of `process(all)` (latch prevention; the value is consumed
  only when the duplicated guard path fires), and
* a capture assignment `v_cap := v` at position P inside the **comb** copy of S's skeleton
  (which therefore must exist in the comb process even if S was its only content).

The moved statement (or guard) reads `v_cap`. Position capture is always sound, so with Rules
2+4 every sequential sink can reach the clocked process; capture is applied only where Rule 3
fails, to keep the output minimal.

**Precedent**: this is the `NameRegAliases` versioning mechanism at a different boundary. There,
a `.reg` on a multi-assigned wire snapshots the wire's positional value into `x_verN_reg` with
the din assignment planted `Before` the alias position (`regDinPatch`), while settled relVals
(immutable, single-assigned, or REG outputs, whose reads are position-independent) place the
chain at the declarations area. Capture snapshots across the comb-to-clocked process boundary
within one step, so the snapshot is a plain comb variable (a register would be a cycle late)
and it needs the latch-prevention default; the trigger is also finer (an actual settled-at-P
violation rather than any multi-assigned wire). Naming follows the same version convention:
`x_ver1`, `x_ver2`, ... without the `_reg` suffix, enumerated per capture site, deduplicated by
the unique-names machinery.

### Rule 5: per-REG commit form

For each REG, choose one of two forms (never mixed):

* **Direct form**: every assignment site is movable (post-capture) and `.din` is never read.
  Sites commit `r :== <rhs>` at their duplicated-skeleton positions. No `_din` shadow, no
  default (a cycle without a write holds the register naturally).
* **Shadow form** (today's fallback): `.din` is read somewhere, or a site is inside an atomic
  combinational region (Rule 7). The existing `<reg>_din` machinery applies unchanged,
  including `dclREGRequiresDefaultSet` and the VHDL din-read process-variable (RMW) locals.

Shared variables always take the direct form; a site that cannot move (Rule 7) is an error,
because `DB.sharedVarCheck` forbids the combinational fallback.

### Rule 6: what remains combinational

The residue: comb-slice statements, their exclusive anonymous cones, capture assignments, and
shadow-form `_din` defaults/assignments. If the residue is empty, no `process(all)` is emitted;
the pure-sequential outcome becomes the degenerate case of the same algorithm rather than a
separate mode. **Compatibility requirement**: bodies that are pure-sequential today must
produce byte-identical output; the single-assignment-to-connection extraction also stays as is
(with the shared-variable exclusion, since a shared target must never become a concurrent
connection under `DB.sharedVarCheck` Rule 2).

### Rule 7: loops are atomic (v1)

A `for`/`while` loop moves whole (all content seq-slice), stays whole in comb (all content
comb-slice, using shadow-form REGs), or, when mixed, stays comb as an atomic unit. Loop-carried
positional dependencies make per-statement slicing inside loops substantially harder; defer.
Consequence: a mixed loop containing a shared-variable write cannot lower legally and must
produce a clear error ("split the loop"), since the comb fallback is forbidden for shared
writes. `match` constructs are NOT atomic; they split like `if` chains (selector under Rule 3/4).

### Rule 8: ordering preservation

Both processes preserve the original relative statement order of their slices. This keeps the
output readable and keeps the shared-variable read-before-write ordering guidance meaningful
for the VHDL rendering (`:=` executes in order within the clocked process).

### Rule 9: operator conversion

In the clocked process: REG and shared targets take `:==`; any process-local temporaries take
blocking `:=` (they are HDL variables; the printers already dispatch on the object class). In
the comb process: blocking `:=` for Verilog, the existing VHDL non-blocking conversion applies
(except din-read RMW locals), unchanged.

### Rule 10 (optional, phase C): intermediate-variable migration

A non-REG, non-shared variable whose every read is by seq-slice statements, which is not a
port, not read concurrently or cross-domain, and is always assigned before read within the
step, may migrate into the clocked process as a process-local variable, shrinking the residue
further. Deferred; measure need first.

## Worked example (the P3b probe shape)

```scala
val a = new RTDomain:
  status := b"00"
  if (we) status := b"11"        // comb sink, multi-assigned
  if (we) ram(addr) := data      // shared write, all reads external
  q.din := ram(addr)             // REG write, reads external + shared
```

Today: everything (including the RAM write) lands in `process(all)` with a `q_din` shadow;
rejected by `DB.sharedVarCheck`. Under the rules:

```
process(all):                    // minimal residue: the comb sink only
  status := b"00"
  if (we) status := b"11"
process(clk):
  if (clk.actual.rising)
    if (we) ram(addr) :== data   // Rule 2 duplicated skeleton, Rule 3 direct move
    q :== ram(addr)              // Rule 5 direct form: no q_din, no default
```

As a bonus, registered shared-variable reads move into the clocked process, which also
eliminates the VHDL `process(all)` staleness gap for them (a shared variable is not a signal,
so comb processes never re-trigger on its change); only genuinely asynchronous reads (feeding
comb sinks) keep that documented VHDL limitation.

## Staging: a dedicated pre-ToED versioning stage

Position capture (Rule 4) is implemented as its own stage (`NameVarVersions`, IMPLEMENTED with
its spec and wired as the last `ToED` dependency),
running immediately before ToED, after every stage that can create or move RT assignments
(`NameRegAliases`' planted version-register din writes are themselves capture candidates). Its
contract: after it runs, every sequential-sink statement's read set, guards and `match`
selectors included (and text-output/assertion arguments, which are sequential sinks per the
resolved questions below), is settled, so ToED performs direct moves only and ASSERTS the
invariant instead of handling violations. The stage's output is plain hand-writable RT code
(a `VAR`, one positional assignment, redirected reads), so it is printable, idempotent (the
rewritten read is single-assigned, hence settled), and testable in its own spec.

Relation to `NameRegAliases`: shared analysis and conventions, not a shared stage skeleton. A
merged stage fails on ordering (versioning must run early, capture as late as possible), and an
abstract common base has nothing real to abstract (History-alias-driven member replacement vs
read-violation-driven ref redirection). Shared pieces, extracted to `compiler/analysis` and
common helpers: the settledness trigger (of which `NameRegAliases`' `getAssignmentsTo.size > 1
&& !dcl.isReg` is the coarse, position-insensitive form; optionally refine it later, dropping
redundant version registers for aliases after the wire's last assignment), the `_ver`
naming/enumeration convention, and the position-anchored planting idiom (`regDinPatch(alias,
Before)`).

## Algorithm phases

1. **Analysis**: per-statement read/write sets over named declarations; last-assignment
   positions; sink classification; skeleton content classification; per-REG form selection.
2. **Emission**: one walk of the original statement tree per target process (Rule 2), with
   direct moves (Rule 3), captures (Rule 4), op conversion (Rule 9), then the existing reset
   structure wrapped around the seq content (unchanged), and the residue planting (unchanged
   paths: single-assignment extraction, VHDL conversions, din-read machinery).

Implementation phasing:

* **Phase A** (DONE): slicing + skeleton duplication + direct moves + per-REG forms in ToED;
  shared writes included; loops atomic; settled-read violations fall back to shadow form
  (REGs) or error (shared).
* **Phase B** (DONE, landed together with A): the `NameVarVersions` pre-stage (see Staging
  above), unlocking violating reads (including shared write index/data through
  later-reassigned intermediates); the remaining `ToED` fallbacks/errors cover only the
  v1-skipped shapes (guard-path hazards and loops).
* **Phase C** (deferred): intermediate-variable migration (Rule 10).

## Blast radius and testing

* Existing pure-sequential outputs must not change (TrueDPR and friends).
* Non-pure-sequential outputs change substantially and mostly improve (e.g. ToEDSpec "Basic
  wires and reg": guarded REG writes move directly into the clocked process, dropping their
  shadows) - sizeable ToEDSpec and backend-ref churn is expected and intentional.
* DFacsimile consumes the pre-ToED IR and is therefore the semantic oracle: add lockstep
  simulations (DFacsimile vs verilator/GHDL) for a mixed comb/seq design with a shared RAM,
  including a same-address write/read collision and a cross-port collision.
* `DB.sharedVarCheck` (elaboration + SanityCheck) and `SanityCheck.sharedAssignCheck` bind
  every intermediate DB, so a slicing bug that leaks a shared write into the residue fails
  right after ToED in spec runs.

## Resolved questions

1. Text output and assertions in a domain body are sequential sinks (fire once per clock
   step, matching RT semantics); their arguments are positional reads subject to Rules 3/4.
2. Loops are atomic in v1, and a mixed loop containing a shared-variable write is an error.
3. Rule 10 (locals in the clocked process) is DEFERRED.
4. Capture variables reuse the `NameRegAliases` version naming (`<name>_verN`, comb form
   without the `_reg` suffix); see the precedent note under Rule 4.
5. Position capture is a dedicated pre-ToED stage (`NameVarVersions`) sharing analysis and
   naming helpers with `NameRegAliases`; see the Staging section.

## Implementation notes (2026-08-04)

Decisions and mechanics that were settled during implementation, beyond the rules above:

* **Shared settledness analysis**: `RTDomainAnalysis` (`dfhdl.compiler.analysis`, compiler_ir)
  holds the per-domain positional analysis used by `NameVarVersions`, `ToED`, and
  `DB.sharedVarCheck`: `posOf`, `lastAssignPos`, `connectionWireNets`, `readConeAndLeaves`,
  `guardPathHazard`, `loopRootOf`, and the movability predicates (`stmtMovable`,
  `stmtUncapturable`, `loopSeqMovable`). `ToED`'s single-assignment-to-connection extraction
  consumes `connectionWireNets` directly, so the consumers cannot drift on what counts as a
  promoted wire.
* **Unmovable shared writes are elaboration errors**: `DB.sharedVarCheck` Rule 3 (a guard-path
  hazard or `.din` read on a shared write, which no capture can fix) and Rule 4 (a shared
  write inside a loop that is not all-sequential with settled reads) reject the user-writable
  shapes at elaboration with the standard positioned error, tested in `ElaborationChecksSpec`.
  `ToED`'s own error remains only as an internal backstop for shapes an intermediate stage
  introduces (e.g. a register demotion cascading into a loop); stage specs hold
  transformation snapshots only.
* **Connection-promoted wires are settled everywhere** (an addition to Rule 3's "externally
  settled" class): a wire whose single whole-target domain-level assignment is promoted to a
  concurrent connection is a settled combinational signal at every position, including reads
  BEFORE the assignment. Without this, the common counter idiom (`cnt := cnt.reg + 1`, whose
  NameRegAliases-planted din write reads `cnt` first) would gain a useless capture in every
  design.
* **Parametric bubbles**: a constant member must stay concrete (`DFVal.Const` requires a
  ref-free `DFType`), and a bubble is only literally representable in Bits, so
  `Bubble.constValOf` builds a parametric-width bubble as a single-bit bubble constant
  repeated by the width parameter, cast to the original type when that type is not Bits:
  `b"?".repeat(width)` for `Bits`, `b"?".repeat(width).uint` / `.sint` for `UInt`/`SInt`.
  Every form is plain user-writable DFHDL, so printing round-trips with no printer special
  cases. This also fixed user-level `x := ?` on parametric-width values, which crashed before
  (two failure modes: `DFBits.createBubbleData` needs a concrete width, and the decimal path
  tripped the parametric assert), so the capture default is uniformly the bubble.
* **Clockless shared-write domains**: `DB.usesClk` now counts an RT-domain shared-variable
  write as clock usage. Previously a domain with only a shared write (no registers) resolved
  to no clock and the write silently lowered into `process(all)`, level-sensitive.
* **Originals vs clones in the seq copy**: when the comb residue is empty (the
  purely-sequential degenerate case) the original member instances move, keeping the output
  byte-identical to the previous pure-sequential mode; with any residue the whole sequential
  slice is cloned (`plantClonedMembers`), since mixed skeletons and shared read cones stay
  behind combinationally.
* **Ordering in the clocked process**: the moved sequential slice comes first (original
  relative order, Rule 8), then the shadow-form register commits (`r :== r_din`), which are
  order-independent non-blocking writes.
* **Statement-free leftovers** (e.g. a conditional whose branches carry no statements) keep
  their previous process placement rather than being sliced.
