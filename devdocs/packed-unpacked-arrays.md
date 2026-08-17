# Packed and Unpacked Arrays (Verilog backends)

How a DFHDL `DFVector` chooses between a SystemVerilog **packed** array (`logic [3:0][7:0] v`)
and an **unpacked** one (`logic [7:0] v [0:3]`), and everything that follows from the choice:
the type rendering, the aggregate-literal order, the whole-vector casts, and the part-selects.

The user-facing side is
[docs/user-guide/type-system/index.md](../docs/user-guide/type-system/index.md#DFVector-verilog-representation)
(the seven placement rules as a reader needs them, plus the two consequences). This document
covers why the decision is shaped the way it is, where each half lives, and what keeps the two
representations from ever meeting in one operation.

The VHDL backends are unaffected throughout: they emit named array types with ascending ranges,
and none of the machinery below is reachable from them.

## 1. Why there is a choice at all

Packed is the better default and unpacked is the necessary exception.

**Packed is what the language wants for data.** A packed array is a vector: it can be sliced,
cast, compared, concatenated, and passed through a port with a type a Verilog author would
recognise. Crucially it is the form a *hand-written* baseline uses for a bus-like array
(`el_t [3:0] channels`), and SystemVerilog will not connect a packed port to an unpacked one at
all, so an unpacked port is not merely a stylistic difference from such a baseline, it is an
incompatible interface.

**Unpacked is what synthesis wants for memory.** Block-RAM and ROM inference keys off the
unpacked-array-with-dynamic-index shape. Emitting a 512 KB ICCM as a packed vector would produce
a correct but unsynthesisable flop farm.

So the representation is chosen per declaration, from its **shape and usage**, and the rules
exist to guarantee the two forms never have to interoperate.

## 2. The two halves of the decision

The decision splits into a **type** question and a **declaration** question, and the split is
what makes it sound.

| | question | answer depends on | where |
|---|---|---|---|
| type | *can* this vector be packed? | the cell type alone | `supportsPackedVector` |
| declaration | *should* this one stay unpacked? | shape and usage | `hasMemAccessPattern` |

### 2.1 `supportsPackedVector`: a type property

[VerilogTypePrinter.scala](../compiler/stages/src/main/scala/dfhdl/compiler/stages/verilog/VerilogTypePrinter.scala)

Packed dimensions apply only to single-bit types, enums, packed structs/unions and other packed
arrays (IEEE 1800-2017 7.4.1), so a vector over an integer atom (`Int`), `real`, `String` or a
time value can never pack. Two further exclusions:

- **Signed cells** (`SInt`, signed fixed-point) stay unpacked. An element select of a packed
  array is a part-select, which is always unsigned, so the cell's signedness would be silently
  lost. Lifting this needs a named signed element type (IEEE 1800-2017 7.4.3), i.e. a dedicated
  stage.
- **Pre-SystemVerilog dialects** (`v95`, `v2001`) have no packed arrays at all, so
  `supportPackedArrays` is false and everything is unpacked. This is also why
  [DropWholeVecAssign](../compiler/stages/src/main/scala/dfhdl/compiler/stages/DropWholeVecAssign.scala)
  exists for those dialects.

Being a **type** property is the load-bearing part: every value of a given vector type agrees on
it, so a mixed-representation *connection* can never print.

### 2.2 `hasMemAccessPattern`: a usage property

[DFValAnalysis.scala](../compiler/ir/src/main/scala/dfhdl/compiler/analysis/DFValAnalysis.scala)

Answers "does this declaration look like a memory?" for a `DFVal.Dcl` or a named constant:

- a **port** never does, being the design interface (§1)
- an **alias-bound constant** (`val b = a`) never does; its value is a whole-vector read
- any **whole-vector use** (assignment, connection, cast, slice, function argument) or any
  **constant-index access** disqualifies it
- otherwise a **`VAR.SHARED`** qualifies (multi-ported RAM), and so does a declaration whose
  dynamic-index accesses include **exactly one read** (the single-read RAM/ROM shape, constants
  included, which covers `localparam` ROMs)

Only a *constant*-index access disqualifies; dynamic-index accesses are the memory pattern
itself. Reading "individual index access" as covering dynamic indexes too would make the
single-read rule unreachable, since every RAM write is such an access.

An **`init` reference is representation-neutral**: it neither disqualifies the initialized
declaration nor counts as a whole-vector read of the init value. Without that carve-out every
initialized memory would be forced packed by its own initializer.

The read test recurses through `DFVal.Alias.Partial`, so `mem(addr)(7, 0)` still counts as one
read of `mem` rather than a whole-vector use.

### 2.3 Putting them together

[VerilogPrinter.scala](../compiler/stages/src/main/scala/dfhdl/compiler/stages/verilog/VerilogPrinter.scala)
computes `unpackedVectorDcls` once per design DB:

```scala
if (!supportsPackedVector(vecType)) Some(dfVal)     // type says it cannot pack
else if (dfVal.isGlobal) None                       // globals are always packed
else Option.when(dfVal.hasMemAccessPattern)(dfVal)  // usage says memory
```

**A global is always packed** even if its usage looks memory-shaped, because
`hasMemAccessPattern` is a design-local analysis while a global's usage spans designs. A global
ROM that would qualify in one design and not another must not print two ways.

Two queries sit on top:

| | meaning |
|---|---|
| `isUnpackedDcl(dfVal)` | this *declaration* prints unpacked |
| `isUnpackedVal(dfVal)` | this *value* is of the unpacked representation |

`isUnpackedVal` is deliberately narrow: only a direct reference to an unpacked declaration, or
an **outer-dimension slice** of one, is unpacked. Every expression value, element select (an
inner dimension) and cast result is packed.

## 3. Rendering

### 3.1 Type and declaration

A packed vector carries its dimensions **in the type**, descending and outermost-first:

```scala
val vin = Bits(8) X 4 <> IN
```
```verilog
input wire logic [3:0][7:0] vin
```

An unpacked declaration puts its **outermost** dimension after the name (ascending) while the
cell keeps its packed type form. That mixed shape is `csDclTypeAndRange`:

```verilog
logic [7:0] mem [0:3];              // outer unpacked, cell packed
```

Under the pre-SystemVerilog dialects and for non-integral cell types, *all* dimensions go after
the name, which is what `csDFVectorRanges` yields (it returns nothing for a packed-capable
vector under SystemVerilog).

`csDFVectorPacked` assembles the packed form from the innermost non-vector cell type
(`vectorScalarCellType`) plus the accumulated dimensions, with a branch per cell kind
(`DFBoolOrBit`, `DFBitsWL`, unsigned `DFDecimal`, `DFEnum`, `DFStruct`, `DFOpaque`). Signed
decimals never reach it, per §2.1.

### 3.2 Aggregate literals

Both forms use the **index-keyed** aggregate (`idx: value`), whose keys bind element indexes and
make the spelling semantically order-free; the listing order follows the declared range
direction:

```verilog
'{3: e3, 2: e2, 1: e1, 0: e0}     // packed: listed descending, like its range
'{0: e0, 1: e1, 2: e2, 3: e3}     // unpacked: listed ascending
```

`csDFVectorElemCS(elemCS, unpackedOrder)` picks the order, and `unpackedOrder` applies to the
**outermost dimension only**: nested dimensions are always packed, so the cell recursion drops
the flag.

Tool support for index keys on a *packed* target was verified empirically: verilator executes
them with the correct element binding (keys are honored regardless of listing order) and slang
accepts them. Vanilla yosys's own SV parser is the outlier, and not because of the keys on
packed specifically: it accepts **no** assignment pattern on a packed target and no index keys
even on unpacked ones (so DFHDL's pre-existing unpacked ROM form was already unreadable there).
Any flow reading DFHDL output through yosys must use its slang frontend, which the equivalence
flow already does.

`csUnpackedInitValue` handles the one place a value's order must be adapted to its target: an
unpacked declaration's `init`/default. Only an anonymous constant-data value or a vector literal
(a `Func.Op.++`) has an order-sensitive aggregate; everything else prints its regular form.

### 3.3 The whole-vector cast is a streaming reversal

This is the subtle consequence. DFHDL's own bit order puts **element 0 in the most-significant
bits**; a packed descending array holds **element 0 at the least-significant end**. So a
whole-vector ⇄ `Bits` cast is not a reinterpretation; it is a scalar-cell-granular reversal,
emitted with the streaming operator:

```verilog
{<<8{v}}                   // cell width 8
```

The grouping width comes from `vectorScalarCellType`. The two directions differ, and the
difference is deliberate:

- `DFVector` ← `DFBitsWL` (to-vector) always streams. **A streaming concatenation is only legal
  in an assignment-like context**, not as a general subexpression, but that is the same
  restriction the element-enumerated `'{...}` form already had, so nothing is lost.
- `DFBitsWL` ← `DFVector` (from-vector) keeps the element-enumerated concatenation
  (`{v[0], v[1], ..., v[N-1]}`, element 0 at the MSB end, correct for both representations)
  whenever the length is a literal, precisely because a plain concatenation *is* a general
  expression (`v.bits | x` must print). Only a parametric-length source, which cannot enumerate
  its elements, falls back to the streaming form and inherits its context restriction (it
  replaces the previous `{v}` spelling, which was not legal SystemVerilog over an unpacked
  array either).

### 3.4 Part-selects

A packed vector's range descends, so an outer-dimension slice prints `[high:low]`, guarded on
the value not being unpacked:

```scala
case vec: DFVector if supportsPackedVector(vec) && !isUnpackedVal(relVal) =>
  s"$relVal[$idxHigh:$idxLow]"
```

## 4. Why the two forms never meet

The soundness argument is worth stating explicitly, because the whole design rests on it:

1. `supportsPackedVector` is a **type** property, so a connection between two values of the same
   vector type can never straddle the two forms.
2. `hasMemAccessPattern` admits only declarations accessed **element-by-element through dynamic
   indexes** (or through their representation-neutral `init`). A whole-vector use or a
   constant-index access disqualifies the declaration outright.
3. `isUnpackedVal` propagates unpacked-ness only through an outer-dimension slice, so no
   expression result is ever unpacked.

Together: an unpacked declaration is only ever touched one element at a time, and an element
select yields a packed cell. There is no operation in which one form has to be converted to the
other.

## 5. Consequences elsewhere

- **Interface fidelity against a Verilog baseline.** A packed port is what a hand-written
  baseline declares, so a ported module's port list now matches by type rather than merely by
  width. This is what makes per-module equivalence checking possible for array ports at all;
  see [../private-plans/logic-eq-plan.md](../private-plans/logic-eq-plan.md) §3 for why an
  unpacked port against a packed baseline port is not bit-comparable (SystemVerilog cannot
  connect them, so no bit correspondence is defined).
- **Element indexing is representation-independent.** DFHDL element `i` is Verilog element `[i]`
  in both forms. Only the *flat bit layout* differs, and only casts (§3.3) expose it.
- **Reference HDL churn.** The packed default rewrote all the vector-bearing reference outputs
  under `lib/src/test/resources/ref/`; the diffs are mechanical (type-position dimensions,
  reversed aggregates, streaming casts).

## 6. Tests

`PrintVerilogCodeSpec` pins the decision table directly:

| test | pins |
|---|---|
| `unpacked vector representation for RAM/ROM shapes` | the memory patterns of §2.2 |
| `packed vector representation overrides` | port / constant-index / whole-vector use forcing packed |
| `vector nested in a struct is packed` | the cell-type recursion |
| `SInt-cell vectors keep the unpacked representation` | the signedness exclusion of §2.1 |

## 7. Open issues

- **Signed cells cannot pack** (§2.1). Lifting it needs named signed element types per IEEE
  1800-2017 7.4.3, i.e. a dedicated stage rather than a printer change.
- **Streaming casts are assignment-context-only** (§3.3). A bits-to-vector cast (or a
  parametric-length vector-to-bits cast) used as a general subexpression has no legal packed
  rendering today; the literal-length vector-to-bits direction is covered by the
  element-enumerated concatenation.
- **`hasMemAccessPattern` is design-local**, which is why globals are excluded (§2.3). A
  cross-design usage analysis would let a global ROM stay unpacked.
- **Namespace-derived package files carry no `` `default_nettype ``/`` `timescale `` header**
  while every other emitted file does. slang rejects the mix ("design element does not have a
  time scale defined but others in the design do"), so any design using a type package fails to
  elaborate as a file set. Not strictly part of this feature, but it lands on the same emitted
  output and blocks the equivalence flow that consumes it.
