# Plan: SystemVerilog / VHDL `generate` statements (ED domain only)

> Add native **generate** statements (for-generate, if-generate, case-generate) to the
> SystemVerilog and VHDL backends, driven by a frontend `generate:` block under which `if` / `for`
> / `match` become *structural* generate statements accepting only DFHDL constants as guards /
> iterators / selectors — instead of the elaboration-time unrolled/inlined code produced today.
>
> **Scope restriction: `generate:` is only permitted inside an ED (event-driven) design domain.**
> This mirrors `process`, which is ED/RT-only, and dramatically shrinks the compiler-stage surface
> (see "Why ED-only matters").

## Locked decisions

1. **IR representation:** reuse the existing `DFForBlock` / `DFIfHeader`+`DFIfElseBlock` /
   `DFMatchHeader`+`DFCaseBlock`, marked with a new **`GenerateTag`** (not a new field). Expose it via
   `isGenerate` accessors on `DFConditional.Header`, `DFConditional.Block`, and `DFLoop.Block`.
2. **Domain:** ED design domain only. `generate:` is rejected in DF/RT domains and inside a `process`.
3. **Constructs:** for-generate, if-generate (with else-if / else), and case-generate.
4. **Dialects:** native emission on **SystemVerilog** and **VHDL-2008**; lower to supported forms for
   VHDL-93 / Verilog-2001, with an elaboration-time unroll fallback for anything a dialect can't
   express.

---

## Background: what is actually changing

Today, structural replication / conditional inclusion happens at **Scala elaboration time**:
`0 until N` resolves to a Scala `Range` (or a procedural `DFForBlock` inside a process), and a
structural `if` picks a branch in Scala. The result is **unrolled/inlined** members in the IR.
Procedural `DFForBlock` / `DFIfElseBlock` / `DFMatchHeader` already survive to the printer, but they
print as *sequential* constructs **inside a process** (`for…loop`, `for(…) begin`, `if…then`).

A **generate** construct is fundamentally different: it is a **concurrent / structural** statement
living at the architecture/module level, parameterizable by elaboration constants (including
generics/parameters, not just literals), that can contain signal declarations, instances,
processes, and nested generates. The work is: introduce a `generate:` scope where `if`/`for`/`match`
build *generate-tagged, concurrent* IR, keep those blocks on the concurrent path, and teach the SV
and VHDL backends to print `generate` syntax (adapting per dialect).

The single hardest sub-problem is already documented as a TODO in
`core/src/main/scala/dfhdl/core/DFRange.scala:6-9`: the IR does not yet "differentiate between a
constant iterator and a constant literal." For-generate needs a **const iterator** so that
iterator-derived parameters (e.g. `Bits[i.type]`-style widths, per-iteration array indices) are
legal and so the iterator prints as a genvar / generate parameter rather than being folded away.

---

## Why ED-only matters (stage-surface simplification)

Restricting `generate:` to ED designs removes the entire RT→ED procedural-lowering interaction,
which was the riskiest part of the domain-agnostic plan:

- **`SimplifyRTOps`** (which rewrites `DFForBlock` → `DFWhileBlock`) gates every rule on
  `isInRTDomain` (`SimplifyRTOps.scala:129,146,148,162,180`). ED generate for-blocks are never touched.
- **`DropRTWaits`** is RT-only for the same reason.
- **`ToED`** only transforms RT / non-ED domain owners (`ToED.scala:80-81` matches `DomainType.RT`;
  `:384` matches `domainType != ED`). An ED design is already ED, so ToED's `process(all)` /
  `process(clk)` collection logic never runs over a generate block.

That leaves only a small set of ED-relevant stages to make generate-aware (see §4). **Action item:**
add a regression assert that `generate:` in a DF/RT domain is a frontend elaboration error, and a
test that an ED generate design is byte-for-byte untouched by the RT stages.

---

## VHDL / SystemVerilog generate reference (target syntax)

**VHDL** (concurrent statements in the architecture body; **labels are mandatory**):
- for-generate: `g: for i in 0 to N-1 generate … end generate g;` — `i` is an implicit constant per instance.
- if-generate (VHDL-93): `g: if COND generate … end generate g;` — **no else/elsif**.
- if-generate (VHDL-2008): adds `elsif COND generate` / `else generate`; per-branch alt-labels optional.
- case-generate (VHDL-2008 only): `g: case EXPR generate when CH => … end generate g;`

**SystemVerilog / Verilog**:
- SV: `generate`/`endgenerate` optional; inline `for (genvar i = 0; i < N; i++) begin : g … end`;
  `if (COND) begin : g … end else …`; `case` inside generate. Named `begin : g` blocks give the
  hierarchical instance path.
- V2001 (fallback): `genvar i;` (separate decl) then
  `generate for (i=0;i<N;i=i+1) begin: g … end endgenerate`; no inline genvar.

All of these are **concurrent**, contrasting with the current procedural loop/if printers.

---

## Layer-by-layer changes

### 1. Frontend DSL — `generate:` block + scope (`core/`)

- **New scope marker** in `core/src/main/scala/dfhdl/core/DFC.scala:130-147`:
  `sealed trait Generate extends Local; object Generate extends Generate` — mirrors `Process`.
  `Generate extends Local` means `0 until N` still resolves to a `DFRange` inside it (the
  `summonFrom { case given DFC.Scope.Local … }` in `DFRange.scala:54-59` already covers Local).
- **New `Generate` object** (model on `Process.scala` / `Fork` / `LocalBlock`):
  `object generate: def apply(block: DFC.Scope.Generate ?=> Unit)(using DFC): Unit`. It does **not**
  create its own IR member — it only switches the scope; child if/for/case are placed concurrently
  in the enclosing ED design/domain. Export via `core.Generate.Ops.*` in
  `core/src/main/scala/dfhdl/core/hdl.scala:80-82`.
- **Domain / nesting guards** (mirror `Process.Ops`, `Process.scala:29-40`):
  - `EDDomainOnly[dt.type]` — reject DF/RT.
  - `NotGiven[DFC.Scope.Process]` — reject `generate:` inside a `process`.
  - (Nested `generate:` inside `generate:` is allowed and maps to nested generate statements.)
- **Const constraints on the entry points:**
  - for-generate: a generate-mode `foreach` requiring `DFRange[CONST]` and yielding a
    **`DFConstOf[DFInt32]`** iterator (resolving the `DFRange.scala:6-9` TODO). The body lambda then
    sees a const iterator, so const-derived params/indices type-check.
  - if-generate / case-generate: guard / selector must be `DFConstOf[…]`.
  - Enforce via type-level CONST where the plugin call shape allows, plus an **elaboration-time
    const check** (`dfVal.asIR.isConst` via `getConstData`) as a positioned-error backstop
    (consistent with `assertElaborationErrors`).

### 2. Compiler plugin (`plugin/`)

Existing transforms already route `if` → `DFIf.fromBranches` (`CustomControlPhase.scala`),
`range.foreach` → `DFFor.plugin` (`LoopFSMPhase.scala:337-353`), and `match` → `DFMatch`.
**Strategy: keep the transforms; make the runtime constructors scope-aware** rather than adding new
plugin recognition. Required plugin work is mostly **scope threading**: ensure the synthesized
branch/body lambdas are invoked with `DFC.Scope.Generate` (as `process` threads `DFC.Scope.Process`
in `Process.scala:47`) so nested generates and const-ness propagate. Verify the `BooleanHack`
if-detection still fires for a const guard (it should — the guard is still a `DFVal`).

### 3. IR (`compiler/ir/`)

- **`case object GenerateTag extends DFTag`** in the tags file. Accessors (mirroring
  `isCombinational` at `DFMember.scala:1503`):
  - `DFLoop.Block.isGenerate` = `hasTagOf[GenerateTag]` (only meaningful for `DFForBlock`; always
    false for `DFWhileBlock`, which is fine).
  - `DFConditional.Header.isGenerate` = `hasTagOf[GenerateTag]`.
  - `DFConditional.Block.isGenerate` = resolve the header via `prevBlockOrHeaderRef` chain and read
    its tag (generate-ness is a property of the whole if/case construct, i.e. the header).
  The frontend generate constructors tag the `DFForBlock` / `DFIfHeader` / `DFMatchHeader` when in
  `DFC.Scope.Generate`.
  - **Rationale for tag over field:** the generate-capable members sit in disjoint hierarchy
    branches (for-block is a `DFBlock`; if/case headers are `DFVal.CanBeExpr` expressions), so a
    "common parent field" has no natural home and would be an orthogonal mix-in implemented per
    class anyway. All these members `derives ReadWriter`; a tag rides the existing `tags` field with
    zero serialization/constructor churn, and it matches how `CombinationalTag`/`FallThroughTag`
    already model behavior-controlling variants on these exact blocks. A field's only real edge
    (forcing every site to decide) is low-value here because only a few frontend constructors create
    these and ED-only removes the mis-lowering risk. Promoting tag→field later is mechanical.
- **Const iterator** — the key IR change. Make the for-generate iterator `Dcl` report as
  const-but-unknown. The existing `UnknownConst` case in `Data.scala:70-90` is exactly the right
  model (const, value unknown — like a generic). Update `getConstData` so a
  `GenerateTag`/`IteratorTag` iterator yields `UnknownConst`, enabling iterator-derived params/indices
  while not folding to a literal. **This is the highest-uncertainty item** — budget extra
  verification on width/param derivation.

### 4. Compiler stages (`compiler/stages/`) — ED-relevant only

Thanks to ED-only scoping (§ "Why ED-only matters"), the RT stages need no changes. Remaining work:

- **`MatchToIf`** (`MatchToIf.scala`) — make generate-aware: keep case-generate when the dialect
  supports it (SV, VHDL-2008), else lower it to an **if-generate chain** (carrying `GenerateTag`),
  not a procedural if.
- **New lowering stage** (`DropUnsupportedGenerates` or similar; use `/new-stage`): dialect
  down-conversion —
  - VHDL-93: rewrite if-generate `elsif`/`else` into nested/negated separate if-generates.
  - Verilog-2001: force a separate `genvar` declaration (no inline genvar).
  - case-generate → if-generate where unsupported.
- **Unroll fallback stage** (escape hatch): elaborate generate-tagged blocks back into inlined
  members (today's behavior) for any dialect/scenario that cannot express the construct.
- **Labels / naming**: VHDL generate statements require labels; SV named `begin : g` blocks define
  the hierarchical path. Extend `VHDLUniqueNames` / `VerilogUniqueNames` (and naming) to assign
  labels to anonymous generate blocks and per-iteration named blocks.
- **Concurrent placement check**: confirm no ED-path stage (e.g. `ExplicitCondExprAssign`,
  `DropProcessAll`, `ExplicitNamedVars`) sweeps a generate-tagged concurrent conditional into a
  process; add guards on `isGenerate` if any does.

### 5. Backend printing (`compiler/stages/.../vhdl`, `.../verilog`)

Dispatch already routes `DFForBlock` → `csDFForBlock` and (via `csDFConditional`) if/match. Add
`isGenerate` branches plus new abstract hooks in `DFOwnerPrinter`, implemented per backend:

- **`csDFForBlock`** (`VHDLOwnerPrinter.scala:252-272`, `VerilogOwnerPrinter.scala:296-315`): if
  generate, emit `g: for i in <range> generate … end generate g;` (VHDL) /
  `for (genvar i = …) begin : g … end` inside `generate…endgenerate` (SV inline; V2001 separate
  genvar). Reuse the existing range rendering.
- **`csDFConditional`** (`DFOwnerPrinter.scala:139-180`): if-generate and case-generate variants —
  new `csDFGenerateIfStatement/ElseIf/Else/End` and `csDFGenerateCase…` hooks; VHDL-2008 native
  elsif/else + case-generate, SV native, older dialects rely on §4 pre-lowering.
- **Concurrent body**: generate bodies must print via the **concurrent** member path (architecture /
  module statements: concurrent assignments, instances, processes, nested generates), not the
  sequential process path.
- **Dialect gating flags** alongside existing `forInteratorDclSupport` / `uniqueSupport` /
  `insideSupport` to select native vs pre-lowered emission.

### 6. Testing

- `StagesSpec` `assertCodeString` cases for for/if/case-generate on **SystemVerilog** and
  **VHDL-2008** (native), plus **Verilog-2001** and **VHDL-93** (lowered), including a generate
  parameterized by a design generic/parameter (not just a literal).
- `assertElaborationErrors` for: `generate:` outside ED, `generate:` inside a `process`, and
  non-const guard/range/iterator/selector inside `generate:`.
- Regression: an ED generate design is untouched by the RT stages.
- `testApps` simulation of a parametric generate design; refresh snapshots in
  `lib/src/test/resources/ref/` via `docExamplesRefUpdate`.

---

## Suggested phasing

1. **IR + frontend foundation**: `GenerateTag` + `isGenerate` accessors, const iterator
   (`UnknownConst`), `generate:` scope with ED-only + no-process guards + const constraints, plugin
   scope threading. Gate on a "dump IR" test.
2. **Modern-dialect printing**: SystemVerilog + VHDL-2008, for + if + case, native.
3. **`MatchToIf` generate-awareness + old-dialect lowering + unroll fallback**: V2001 genvar,
   VHDL-93 nesting, case→if.
4. **Tests, refs, simulation.**

## Top risks

- **Const-iterator IR change** (resolving the `DFRange` TODO) — ripples into const-folding and
  width/param/index derivation; the highest-uncertainty item.
- **Scope/const propagation** through nested block lambdas in the plugin/runtime.
- **VHDL label requirement** — must synthesize labels for anonymous generate statements.
- **Concurrent-body printing** — ensuring generate bodies reuse the design-body (concurrent) print
  path rather than the process path.

---

## Key file references

| Concern | File |
|---|---|
| Scope markers | `core/src/main/scala/dfhdl/core/DFC.scala:130-147` |
| ED-only / no-nesting guard pattern | `core/src/main/scala/dfhdl/core/Process.scala:29-47` |
| Range / iterator const TODO | `core/src/main/scala/dfhdl/core/DFRange.scala:6-9,54-80` |
| DSL export point | `core/src/main/scala/dfhdl/core/hdl.scala:80-82` |
| For / If construction | `core/src/main/scala/dfhdl/core/DFFor.scala`, `DFIf.scala` |
| Plugin if / for transforms | `plugin/src/main/scala/plugin/CustomControlPhase.scala`, `LoopFSMPhase.scala:337-353` |
| IR conditional/loop members + `isCombinational` tag precedent | `compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala:1306-1553` |
| ConstData model (`UnknownConst`) | `compiler/ir/src/main/scala/dfhdl/compiler/ir/Data.scala:70-90` |
| RT gating (why ED-only is safe) | `SimplifyRTOps.scala:129`, `ToED.scala:80-81,384` |
| Match→if | `compiler/stages/src/main/scala/dfhdl/compiler/stages/MatchToIf.scala` |
| Backend pipeline | `compiler/stages/src/main/scala/dfhdl/compiler/stages/BackendPrepStage.scala` |
| Printer dispatch | `compiler/ir/src/main/scala/dfhdl/compiler/printing/Printer.scala:94-120` |
| Conditional/loop printing (shared) | `compiler/ir/src/main/scala/dfhdl/compiler/printing/DFOwnerPrinter.scala:139-180` |
| VHDL / Verilog owner printers | `.../vhdl/VHDLOwnerPrinter.scala:206-272`, `.../verilog/VerilogOwnerPrinter.scala:187-315` |
