# Plan: Interfaces & Views (modports) — Frontend, IR, and Backends

> Status: draft for review. Covers DFHDL `DFInterface`/`RTInterface`/`EDInterface`
> declarations, `view` builders, `View`/`VectorView` instances, and emission to
> SystemVerilog interfaces+modports and VHDL-2019 mode views, with a portable
> flattening fallback.

---

## 1. Target-language findings (what drives the backend design)

### 1.1 SystemVerilog `interface` + `modport` (IEEE 1800-2017 §25)

- `interface name #(params) (ports); ... modport view (input a, output b); endinterface`.
- A `modport` assigns a per-signal direction **from the consuming module's point of
  view**. Master/slave/monitor are just multiple modports over the same signals with
  inverted directions. This maps 1:1 to DFHDL views and `flip`.
- Nested interfaces compose: a parent modport references `child.childModport`.
  **Composition flows downward only** — "no upward scoped hierarchical references
  within a modport", and a modport may not declare new ports.
- Arrays of interface instances exist; element selection needs an **elaboration-time
  constant index** (literal/param/genvar). Iteration is via `generate`.

**Synthesis limitations that constrain our generator (must honor):**

1. **Always emit an explicit `modport` per consumer.** Without one, nets become silent
   `inout` and variables become `ref` (Vivado does this silently). If any interface
   signal is a variable type, a modport is *required*.
2. **Never put an interface port on the top module.** Verilator rejects it (wontfix);
   clean synthesis practice flattens the top boundary to scalar ports anyway.
3. **Emit named port connections** (`.intf(intf0)`) — required by Yosys, harmless
   elsewhere.
4. **Nested interfaces are not portable** (Vivado synth: Not Supported). Keep them
   gated / lowerable.
5. **Arrays: single-dimension**, declared at module scope, indexed by genvar/param,
   iterated via `generate`. For slice connections, omit the modport qualifier on the
   connection and put it on the declaration.
6. **Never emit for synthesis:** `virtual interface`, clocking blocks, `export`
   tasks/functions, dynamic arrays, specify blocks. (None of these are in our model,
   so this is automatically satisfied.)

### 1.2 VHDL-2019 mode views (IEEE 1076-2019 §6.5.2, §16.2.8)

- VHDL-2008 records force **one shared mode** for all elements — cannot mix
  directions. VHDL-2019 fixes this with **mode views**.
- `view MasterView of StreamingIf is Valid : out; Data : out; Ack : in; end view;`
  then `port ( bus : view MasterView )`.
- The opposite side uses **`MasterView'converse`** (in↔out, inout→inout,
  buffer→in; nested view → its converse, recursively). `'converse` inverts *every* element
  mode, so it is DFHDL **`flipAll`**; plain `flip` keeps anchored ports (§3.2) and therefore
  equals `'converse` only when the view has no anchored ports.
- The connecting signal is a **plain record**, never a view. Views live only on ports.
- Arrays: `view (MasterView) of ArrType(range)`. Nested: an element may itself be a
  sub-view; `'converse` recurses.
- Rules our generator must honor: record subtype must be **unresolved** (elements may
  be resolved subtypes — `std_logic` is fine); **every** element must be listed; never
  emit `linkage`; emit only `in/out/inout` so `'converse` is a clean involution.

**Tool-support reality (critical):** as of 2024–2026 mode views are essentially
**NVC-only** for simulation. **GHDL does not implement them.** Synthesis support is at
best Vivado-only and not crisply documented. → **VHDL native mode views must be opt-in;
the portable default must flatten** (or use VHDL-2008 split-records).

### 1.3 Consequence for our backend strategy

| DFHDL concept | SystemVerilog | VHDL-2019 | Portable fallback |
|---|---|---|---|
| interface decl | `interface` | `record` type | struct / scalar ports |
| view (modport) | `modport` | mode `view` | per-port scalar ports |
| `flip`/`flipAll` | inverted modport | `'converse` (≙ `flipAll`) | inverted scalar dirs |
| nested view | nested modport | nested view | recursive flatten |
| vector view | iface array + `generate` | `view(V) of arr` | flattened array ports |

**Headline decision: flatten-first.** The default lowering expands interfaces/views to
plain scalar ports + connections, producing valid HDL on *every* tool. Native
`interface`/`modport` (SV2017/SV2019) and `view`/`'converse` (VHDL-2019) emission are
**opt-in per dialect/target**, layered on top of an already-working flattened pipeline.

---

## 2. IR design

### 2.1 What already exists (reuse surface)

- `DFDesignBlock` (DFMember.scala:1571) — a parameterized, instantiable, hierarchical
  `DFDomainOwner`. `InstMode { Normal, Def, Simulation, BlackBox }`. Hierarchy via
  per-design **sub-DBs keyed by `ownerRef`** (recently unified with
  `DFDesignInst.designRef`).
- `DFDesignInst(designRef, paramMap, ownerRef, ...)` — the instantiation site; `paramMap`
  binds parameter name → value ref.
- `DFInterfaceOwner` — a `DFDomainOwner` skeleton that previously existed (with a
  `DFVal | DFInterfaceOwner` `DFNet.Ref` union, an interface-to-interface
  `Connection.unapply`, and a `<->` printer path). **Removed** — see the note at the end
  of §2.2; the model below uses `DFDesignBlock` + `DFType`s instead.
- `DFVal.Dcl(dfType, modifier, ...)` with `Modifier(dir: {VAR,IN,OUT,INOUT}, special)`.
- `DFVal.Alias.SelectField(relValRef, fieldName, ...)`, `Alias.ApplyIdx(relValRef,
  relIdx)`, `Alias.ApplyRange(...)` — all over `relValRef: PartialRef` (a **DFVal** ref).
- `DFVal.PortByNameSelect` — selects a named port off a design instance (the existing
  precedent for "member access on an instance yields a value").
- `DFStruct(name, fieldMap: ListMap[String, DFType])`, `DFVector(cellType, dims)`.

### 2.2 Modeling (decided)

Two decisions fix the model:

1. **Interface declaration = a `DFDesignBlock` with a new `InstMode.Interface`.** An
   interface is "an empty design that cannot have processes, connections, or
   assignments." It reuses the whole design machinery — sub-DB hierarchy, params,
   `DFDesignInst`, printer-per-design dispatch — for free. The existing skeleton
   `DFInterfaceOwner` is **dropped/absorbed** (along with the `<->` `DFNet` hint for it).
2. **New composite `DFType`s.** `DFInterface` carries the port *structure* (like
   `DFStruct`) and an `interfaceRef` identity link to its declaring `DFDesignBlock`
   (mirroring `DFDesignInst.designRef`, so it is *not* a `NamedDFType`); `DFView` (a single
   concrete type) carries an `interfaceType` plus the resolved per-field directions, with
   `flip`/`flipAll`/`monitor`/`driver` transforms. All are ordinary `DFType`s, so interface/view values are
   ordinary `DFVal`s and `SelectField`, `ApplyIdx`, `ApplyRange`, `<>`/`DFNet` and printer
   dispatch are **reused unchanged**.

**(a) Interface declaration & instance.** `class MyIfcDcl(w) extends DFInterface`
elaborates to a `DFDesignBlock(instMode = InstMode.Interface)`; its body members are the
port `DFVal.Dcl`s (`p1`, `p2`, default `VAR`) plus the view **template** declarations
(`DFVal.Dcl` with `Modifier(Dir.VIEW(ViewSite.Template))` — see (c)).
`val myIfcInst = MyIfcDcl(8)` is an ordinary `DFDesignInst` (`designRef` → the interface
block, `paramMap` carries `8`). Port access `ifc.p1` reuses `PortByNameSelect`.

**(b) `DFInterface` and `DFView` composite types.** Add to `DFType.scala` next to
`DFStruct` (NOT reusing `DFStruct` — see note at end of §2.2):

```scala
// composite port structure — like DFStruct, declaration-ordered. Each field carries its
// DFType and its *declared* direction (a `DFInterface.Field`):
//   leaf field   → Field(scalar DFType, dir)    dir ∈ { VAR (flippable), IN | OUT | INOUT (anchored) }
//   nested field → Field(nested DFInterface, _) — self-describing, so `dir` is unused.
// Identified by `interfaceRef` — a OneWay ref to its interface DFDesignBlock,
// mirroring `DFDesignInst.designRef` — rather than a stored name; the name is read
// from the referenced block. NOT a `NamedDFType`; `=~` compares `interfaceRef`.
final case class DFInterface(
    interfaceRef: DFInterface.Ref,            // OneWay → the interface DFDesignBlock
    fieldMap:     ListMap[String, DFInterface.Field]
) extends ComposedDFType
object DFInterface:
  type Ref = DFRef.OneWay[DFDesignBlock]
  // extends HasRefCompare[Field] → reuses `=~`/`getRefs`/`copyWithNewRefs` and lets a
  // `ListMap[String, Field]` be compared with `=~` directly (like `tags`/`args`).
  final case class Field(dfType: DFType, dir: Modifier.Dir) extends HasRefCompare[Field]

// A view stores ONLY what it adds over `interfaceType` — no field DFTypes are repeated:
//   dirMap    → resolved direction overlay, LEAF ports only
//   nestedMap → the chosen sub-view per nested field (undirected DFInterface in the decl)
// `projectedFieldMap` derives the full directed (dfType, dir) map on demand by merging
// `interfaceType` with the overlay. The transforms recompute a new view (recursing into
// nested views); `flip` consults `interfaceType` to leave anchored leaves untouched.
final case class DFView(
    interfaceType: DFInterface,                  // underlying declaration
    name:          String,                       // "manager"/…; generic = ""
    dirMap:        Map[String, Modifier.Dir],     // LEAF ports only
    nestedMap:     ListMap[String, DFView]        // chosen sub-view per nested field
) extends NamedDFType, ComposedDFType:
  lazy val projectedFieldMap: ListMap[String, DFInterface.Field]  // interface ⋈ overlay
  def flip:    DFView   // invert flippable (VAR-declared) leaves only; recurse `flip`
  def flipAll: DFView   // invert every leaf;            recurse `flipAll`  (≙ VHDL `'converse`)
  def monitor: DFView   // force every leaf to IN;       recurse `monitor`
  def driver:  DFView   // force every leaf to OUT;      recurse `driver`
```

- **One concrete `DFView`, no `AsIs`/`Flipped` split.** With four transforms
  (`flip`/`flipAll`/`monitor`/`driver`), a lossless `Flipped` wrapper no longer fits; each
  transform eagerly recomputes the overlay and recurses into nested views. `flip` reads each
  leaf's declared dir from `interfaceType.fieldMap` and inverts only the `VAR`-declared
  (flippable) leaves, passing anchored `IN`/`OUT` leaves through unchanged (see §3.2).
- **A view stores only its overlay, never the field types.** `dirMap` holds resolved leaf
  directions and `nestedMap` holds the chosen sub-view per nested field; the field DFTypes
  live once in `interfaceType`. `projectedFieldMap` merges the two on demand (interface
  structure + resolved dirs, nested fields replaced by their sub-view), so consumers get the
  full directed `(dfType, dir)` map with nothing stored twice.
- **Nested views go in `nestedMap`.** A composed view `view(aw.manager, w.manager, …)` has
  `nestedMap(aw) = Axi4LiteAW.manager`, `nestedMap(b) = Axi4LiteB.manager.flip`, etc. — each
  a nested `DFView`.
- **`flip` is a recompute, not a wrapper, and is no longer VHDL `'converse`** when anchored
  ports exist (`'converse` inverts every mode; `flip` keeps anchored leaves — `flipAll` is
  the true `'converse`; see §1.2/§5). It is still involutive on the flippable leaves.
- The **generic** projection (`myIfcInst.ASIS`) is a `DFView` with `name = ""` whose `dirMap`
  is taken straight from the declaration (`VAR` stays `VAR`, anchored stay `IN`/`OUT`) and
  whose `nestedMap` holds generic sub-views.
- Builder composition (`view.in/out` chaining) has **no IR node** — only the final named
  view (template) is stored.

**(c) View sites — `Template` and `Projection(terminal)`.** A view exists at two sites, both
a `DFVal.Dcl` carrying a `DFView` `dfType`, distinguished by an IR-only `Modifier` direction
`Dir.VIEW(site)`:

```scala
// IR-only: never produced by a user port declaration, only by view elaboration.
enum Terminal derives CanEqual, ReadWriter:
  // 1:1 with the frontend terminals ASIS / FLIP / FLIPALL / MONITOR / DRIVER.
  case AsIs, Flip, FlipAll, Monitor, Driver

enum ViewSite derives CanEqual, ReadWriter:
  case Template                        // the view as declared inside the interface
  case Projection(terminal: Terminal)  // the view as used at an instance, via a terminal

enum Dir derives CanEqual, ReadWriter:
  case VAR, IN, OUT, INOUT
  case VIEW(site: ViewSite)
```

- **`Template`** — the view as *declared* inside the interface
  (`val reader = view.in(…).out(…)`). It defines a named, directional projection of the
  interface's ports — the IR analog of a SystemVerilog `modport` declaration or a
  VHDL-2019 mode-view declaration. There is exactly one template per declared view, and it
  lives as a member of the interface `DFDesignBlock`. A template carries no terminal.
- **`Projection(terminal)`** — the view as *used* at an interface instance via a projection
  terminal (`ifc.reader.ASIS`, `ifc.reader.FLIP`, `ifc.reader.MONITOR`, …). It is the template
  **projected onto a concrete instance**, yielding a connectable value — the IR analog of
  referencing a modport through an instance (SV `ifc.reader`) or a mode-view-typed port (VHDL).
  Many projections may derive from one template; each lives in the using design. The projected
  `DFView` `dfType` already holds the resolved directions (the `terminal` applied to the
  template), so `terminal` is **provenance**: it is not always recoverable from the dirs (a
  `Flip` over anchored ports is not a clean converse, and a hand-authored view can coincide
  with a `FlipAll` result), and it is read by diagnostics and by native VHDL `'converse`
  emission (`Flip`/`FlipAll`). `InterfaceSanityCheck` asserts `dfType == terminal.applyTo(
  templateView)` so the two cannot drift.

The view's per-leaf-port directions live in the `DFView` `dfType` (`dirMap`/`nestedMap`),
**not** in this modifier — `Dir.VIEW` is a site marker, not a direction. `special` is always
`Ordinary` for a view (asserted in the sanity check). Being a parametrized case alongside
`VAR/IN/OUT/INOUT`, `VIEW(_)` is **auto-excluded** by the existing `isPort`/`isVar`/…
predicates (their `case _ => false` fall-through), while any non-wildcard `dir` match
(e.g. a printer emitting a direction keyword) is compiler-flagged to handle views. Add
`isView` / `isViewTemplate` / `isViewProjection` predicates next to `isPort`/`isVar`.

The `Dir.VIEW(site)` marker is **authoritative and local** — stable across
`copyWithNewRefs` and every DB form, so it never relies on ref identity or on resolving
the owner. A sanity check cross-validates it against structure: a `Template` is owned by
an `InstMode.Interface` block, a `Projection` by an ordinary design.

References to a view inside a connection reuse `PortByNameSelect` **exactly like a scalar
port**. So two orthogonal distinctions coexist cleanly: `Dcl` vs `PortByNameSelect`
separates "the view value/site" from "a reference to it in a net", and `Template` vs
`Projection` separates the two value/sites. (This is why the use-site is *not* discriminated
by `PortByNameSelect` — that node is needed for the reference role regardless.)

**Vectors.** `… ASIS X 4` is a `DFVal` of `DFVector(cellType = <a DFView>, dims)`, and
`readerViewX4(i)` is the existing `ApplyIdx`. `View[T]` / `VectorView[V,D]` are the
frontend faces; no new IR index/select nodes.

**Net new IR:** `InstMode.Interface`; the `DFInterface` composite `DFType` (identified by
`interfaceRef`) and the single concrete `DFView` `DFType` (with `flip`/`flipAll`/`monitor`/
`driver` transforms); the IR-only `Modifier` direction `Dir.VIEW(site)` where
`site = ViewSite{Template, Projection(terminal: Terminal{AsIs, Flip, FlipAll, Monitor, Driver})}`
distinguishes the two view sites and records the projection terminal. **Reused unchanged:**
`DFDesignBlock`/`DFDesignInst` + sub-DB hierarchy + `paramMap`,
`Modifier`, `PortByNameSelect`, `SelectField`, `ApplyIdx`, `ApplyRange`,
`DFNet`/`Connection`, `DFVector`, printer-per-design dispatch. **Removed:** the
`DFInterfaceOwner` skeleton and its `<->` `DFNet` path.

**Why not reuse `DFStruct` for the structure:** identical shape (`name` + `fieldMap`), but
a `DFStruct` is a *packed data aggregate* — every field has a bit width/position and gets
lowered to `Bits` by `DropStructsVecs`. An interface field may be a nested `DFView` (no bit
representation) and must flatten to *ports*, not bits. Reusing `DFStruct` would violate
those invariants and force `isInterface` guards across all struct/width/packing code, so
`DFInterface` stays distinct (sharing only the `ComposedDFType` field-map mechanism).

### 2.3 Guarding interface semantics

Because an interface reuses `DFDesignBlock`, the frontend `DFInterface` container sets a
**`DFC.Scope.Interface`** (alongside the existing `DFC.Scope.Design`). The scope is used
to *prevent* illegal constructs inside an interface body at elaboration time — no
processes, no `<>`/`:=` connections/assignments, no behavioral statements — so the
"empty design" invariant is enforced where it's written rather than discovered later.

In addition, the **compiler plugin** (`MetaContextPlacerPhase`) enforces two structural
guarantees on interface declarations at compile time, so misuse is a clear type-error
rather than a confusing downstream failure:

- **Ports and parameters must be `protected`.** Every DFVal member of an interface — a
  port or a `<> CONST` parameter — must be declared `protected` (or `private`). The view
  is the interface's public surface; the ports are internal wiring reachable only through
  a view's `.ASIS` (or another projection terminal). Once marked `protected`, Scala's own access control (enforced at
  typer) blocks `ifc.port` selections and hides the members from IDE autocomplete. The
  check runs post-typer, so `<:< Interface` resolves transitively (it catches ports in
  indirect subclasses too).
- **No anonymous interface instances.** A user-written `new MyIfc() {}` is rejected — an
  interface must be a named class so it has a stable identity (the `interfaceRef` block).
  The plugin's own instance anon-classes are synthesized in the transform pass and are
  never flagged.

---

## 3. Frontend (core/)

Mirrors the `Design` frontend (`core/src/main/scala/dfhdl/core/Design.scala`,
`DFVal.scala`).

- `DFInterface` / `RTInterface` / `EDInterface` base traits, analogous to
  `DFDesign`/`RTDesign`/`EDDesign`, fixing `domainType` and elaborating to a
  `DFDesignBlock(InstMode.Interface)`. Constructor args become interface params (same
  `<> CONST` machinery as design params). `val pX = T <> VAR` declares ports. The
  container sets **`DFC.Scope.Interface`** (§2.3) to forbid processes/connections/
  assignments in the body.
- `view` object with `view.in(...)`, `view.out(...)`, `view.inout(...)`, chained
  (`view.in(p1).out(p2)`), and an apply `view(ports/views)` for already-directed members.
  Builder composition `+`, `-`, `flip` returns `ViewBuilder[T]` (`T` a named tuple).
  Builders are compile-time/typed only — they elaborate to the final `DFView` (§2.2b).
- On an interface instance: `def ASIS: View[T]` **and** `type ASIS <: View[T]`; `X` for
  vector views (reuse the generic-vector `X` construction). `flip` on a view instance.
- Anchored ports (`<> IN`/`<> OUT`) and the UPPERCASE projection terminals
  (`ASIS`/`FLIP`/`FLIPALL`/`MONITOR`/`DRIVER` on a named view; `ASIS`/`MONITOR`/`DRIVER` on
  the generic projection) — see §3.2.
- `<>` connection of two views: typeclass-checked **port-per-port through the full nested
  hierarchy** (directionality must match). Reuse `ConnectOps.specialConnect` direction
  checking, generalized to walk `DFView` elems recursively.
- (Out of scope, noted for later: view-typed method arguments.)

### 3.1 Worked example: AXI4-Lite (the hierarchical/nested case)

This mirrors the OSVVM VHDL-2019 AXI4-Lite manager/responder views and the SV
master/slave modport pattern from the language research. Directions are written **from
the manager (master) point of view**; the subordinate side is `flip` (≙ SV inverted
modport ≙ VHDL `'converse`).

**DFHDL frontend — per-channel interfaces:**

```scala
// One interface per AXI4-Lite channel. `<> VAR` ports become directed by each view.
class Axi4LiteAW(addrWidth: Int <> CONST) extends RTInterface:
  val AWADDR  = UInt(addrWidth) <> VAR
  val AWPROT  = Bits(3)         <> VAR
  val AWVALID = Bit             <> VAR
  val AWREADY = Bit             <> VAR
  val manager     = view.out(AWADDR, AWPROT, AWVALID).in(AWREADY)
  val subordinate = manager.flip

class Axi4LiteW(dataWidth: Int <> CONST) extends RTInterface:
  val WDATA  = Bits(dataWidth)     <> VAR
  val WSTRB  = Bits(dataWidth / 8) <> VAR
  val WVALID = Bit                 <> VAR
  val WREADY = Bit                 <> VAR
  val manager     = view.out(WDATA, WSTRB, WVALID).in(WREADY)
  val subordinate = manager.flip

class Axi4LiteB extends RTInterface:
  val BRESP  = Bits(2) <> VAR
  val BVALID = Bit     <> VAR
  val BREADY = Bit     <> VAR
  val manager     = view.in(BRESP, BVALID).out(BREADY)
  val subordinate = manager.flip

class Axi4LiteAR(addrWidth: Int <> CONST) extends RTInterface:
  val ARADDR  = UInt(addrWidth) <> VAR
  val ARPROT  = Bits(3)         <> VAR
  val ARVALID = Bit             <> VAR
  val ARREADY = Bit             <> VAR
  val manager     = view.out(ARADDR, ARPROT, ARVALID).in(ARREADY)
  val subordinate = manager.flip

class Axi4LiteR(dataWidth: Int <> CONST) extends RTInterface:
  val RDATA  = Bits(dataWidth) <> VAR
  val RRESP  = Bits(2)         <> VAR
  val RVALID = Bit             <> VAR
  val RREADY = Bit             <> VAR
  val manager     = view.in(RDATA, RRESP, RVALID).out(RREADY)
  val subordinate = manager.flip
```

**DFHDL frontend — top interface nesting the five channels** (nested instances + composed
nested views; `view(..)`'s apply takes already-directed sub-views):

```scala
class Axi4Lite(addrWidth: Int <> CONST, dataWidth: Int <> CONST) extends RTInterface:
  val aw = Axi4LiteAW(addrWidth)
  val w  = Axi4LiteW(dataWidth)
  val b  = Axi4LiteB()
  val ar = Axi4LiteAR(addrWidth)
  val r  = Axi4LiteR(dataWidth)
  val manager     = view(aw.manager, w.manager, b.manager, ar.manager, r.manager)
  val subordinate = manager.flip   // flips recursively through every nested channel view
```

**DFHDL frontend — instance, views, connection, and a vector view:**

```scala
val bus = Axi4Lite(addrWidth = 32, dataWidth = 32)
val mgr = bus.manager.ASIS           // View[...] — manager side
val sub = bus.subordinate.ASIS       // View[...] — subordinate side
mgr <> sub                           // legal: matches port-per-port through all nesting

// e.g. a 1-to-4 interconnect: a VectorView of 4 manager-side AXI4-Lite views
val managers = Axi4Lite(32, 32).manager.ASIS X 4    // VectorView[..., 4]
val m0 = managers(0)                                 // ApplyIdx → View[...]
```

**Lowers to (native SystemVerilog, SV2017/SV2019) — the AW channel + top:**

```systemverilog
interface Axi4LiteAW #(parameter ADDR_WIDTH = 32) ();
  logic [ADDR_WIDTH-1:0] AWADDR;
  logic [2:0]            AWPROT;
  logic                  AWVALID;
  logic                  AWREADY;
  modport manager     (output AWADDR, AWPROT, AWVALID, input  AWREADY);
  modport subordinate (input  AWADDR, AWPROT, AWVALID, output AWREADY);
endinterface

interface Axi4Lite #(parameter ADDR_WIDTH = 32, parameter DATA_WIDTH = 32) ();
  Axi4LiteAW #(ADDR_WIDTH) aw();
  Axi4LiteW  #(DATA_WIDTH) w();
  Axi4LiteB                b();
  Axi4LiteAR #(ADDR_WIDTH) ar();
  Axi4LiteR  #(DATA_WIDTH) r();
  modport manager     (aw.manager, w.manager, b.manager, ar.manager, r.manager);
  modport subordinate (aw.subordinate, w.subordinate, b.subordinate, ar.subordinate, r.subordinate);
endinterface
```

> Note: nested interfaces are **not portable for synthesis** (Vivado: Not Supported).
> For the portable default, `FlattenInterfaces` collapses the hierarchy — either into one
> interface with prefixed signals (`aw_AWADDR`, …) + flat modports, or fully into scalar
> module ports. Native nested emission above is opt-in for tools that accept it.

**Lowers to (native VHDL-2019, NVC) — the AW channel + top:**

```vhdl
type axi4lite_aw_t is record
  AWADDR  : unsigned;                       -- width constrained at signal/port use
  AWPROT  : std_logic_vector(2 downto 0);
  AWVALID : std_logic;
  AWREADY : std_logic;
end record;

view axi4lite_aw_manager of axi4lite_aw_t is
  AWADDR, AWPROT, AWVALID : out;
  AWREADY                 : in;
end view;
alias axi4lite_aw_subordinate is axi4lite_aw_manager'converse;

type axi4lite_t is record
  aw : axi4lite_aw_t;  w  : axi4lite_w_t;  b  : axi4lite_b_t;
  ar : axi4lite_ar_t;  r  : axi4lite_r_t;
end record;

view axi4lite_manager of axi4lite_t is
  aw : view axi4lite_aw_manager;   w  : view axi4lite_w_manager;
  b  : view axi4lite_b_manager;    ar : view axi4lite_ar_manager;
  r  : view axi4lite_r_manager;
end view;
alias axi4lite_subordinate is axi4lite_manager'converse;   -- recurses into sub-views
```

> Default VHDL stays flattened (GHDL/synthesis lack mode views); the records+views above
> are opt-in for NVC. The VHDL-2008 split-record fallback (one `in` record + one `out`
> record per view) is the portable middle ground.

### 3.2 Anchored-direction ports & projection terminals

By default an interface port is **flippable**: declared `<> VAR`, undirected, and given a
direction by each view (a *relative* direction — opposite on the two ends, see §3.1). A port
may instead be declared with an **anchored** direction, `<> IN`, `<> OUT`, or `<> INOUT`. Its
direction is *absolute*: the same for every consumer and never reversed by `flip`. This is the
DFHDL analog of a SystemVerilog interface *header port* (the `(input clk, …)` list) — the
textbook case is clock/reset, which enters the whole interface one way and is broadcast
identically to all sides.

**Frontend rules:**

- An anchored port may be added to a view only in its declared direction; `view.in(p)` on an
  `<> OUT` port (or vice-versa) is a compile error. Flippable `<> VAR` ports take whichever
  direction the view assigns (via `view.in`/`view.out`/`view.inout`).
- `flip` inverts only the **flippable** leaves of a view, recursing through nested views; it
  leaves anchored leaves untouched. So `manager.flip` keeps an anchored `clk : in` as `in` on
  both sides. `INOUT` is its own converse (`flip` and `flipAll` both leave it `inout`), matching
  a SystemVerilog `inout` modport and VHDL `'converse` mapping `inout → inout`.
- `flip` inverts only the **flippable** leaves of a view, recursing through nested views; it
  leaves anchored leaves untouched. So `manager.flip` keeps an anchored `clk : in` as `in` on
  both sides.

**Projection terminals.** A view is projected onto an instance with an UPPERCASE terminal that
bakes the directionality into the projection (replacing a transform-then-project step). On a
**named view**:

| terminal   | meaning |
|---|---|
| `.ASIS`    | project as declared |
| `.FLIP`    | project flipped; anchored ports respected (≙ `flip.ASIS`) |
| `.FLIPALL` | project flipped; anchored ports inverted too |
| `.MONITOR` | project with every port forced to input (observe-all) |
| `.DRIVER`  | project with every port forced to output (drive-all) |

On the **generic** projection (`ifc.…`, no named view) only `.ASIS`, `.MONITOR`, `.DRIVER`
exist: the flip family needs declared relative directions to act on, and a generic projection's
flippable ports are undirected (nothing to flip), so `FLIP`/`FLIPALL` are omitted *by
construction* rather than as a special case.

`FLIP` is the only override with a safe, reusable lowercase counterpart (`flip`, a view→view
transform used to *name* a converse view). `FLIPALL`/`MONITOR`/`DRIVER` exist **only** as
use-site terminals (there is no storable `forceX` view), so blanket/override directionality can
never be silently baked into a shared named view — the override is always visible at the
connection site. (`MONITOR`/`DRIVER` carry no `force` marker because the role already implies
overriding anchoring; there is no anchor-respecting monitor.)

**Connection rules** (anchored ports change `<>` semantics by site):

- **Sibling/peer** (`mgrView <> mgrView.FLIP` between two child designs): only the flippable
  ports connect; anchored ports are *not* auto-connected (both ends see `clk : in`, so they
  cannot drive each other) and must be wired separately (e.g. `clk` from a parent/source).
- **Parent→child** (same-orientation `view <> view`, no flip): *all* ports connect, flippable
  and anchored alike — the parent passes its anchored inputs (clk/rst) down into the child.

**IR modeling** (extends §2.2):

- An anchored port is an ordinary `DFVal.Dcl` with `Modifier(Dir.IN|OUT)` in the interface
  block (vs `Dir.VAR` for flippable). The `DFView.flip` transform reads each leaf's declared
  `Dir` from `interfaceType.fieldMap`: it inverts `VAR`-origin leaves and passes `IN`/`OUT`-origin
  leaves through.
- Each projection is a `Projection(terminal)`-site `DFView` whose resolved overlay
  (`dirMap`/`nestedMap`) is the named template transformed by that terminal
  (`AsIs`/`Flip`/`FlipAll`/`Monitor`/`Driver`).
  `FlipAll`/`Monitor`/`Driver` override anchoring (all-inverted / all-`IN` / all-`OUT`); `Flip`
  respects it; `AsIs` is the template unchanged. The `terminal` is recorded as provenance
  (§2.2c).
- `InterfaceSanityCheck` enforces the anchored-direction view rule, the per-site connection
  rules above, and the `dfType == terminal.applyTo(templateView)` invariant.

---

## 4. Compiler stages

`compiler/stages/`. Stage base: `GlobalStage.transformGlobal(DB)` /
`HierarchyStage.transformSubDB(rootDB)`.

Because an interface is a `DFDesignBlock(InstMode.Interface)`, every existing stage must
decide per-stage to **apply** its transform to interface blocks (e.g. RT→ED lowering of a
port's type still applies), **skip** it (behavioral stages — process/clk-rst/magnet
handling have nothing to act on, since the `DFC.Scope.Interface` guard kept those out),
or **specialize** it. Add an `isInterface` predicate on `DFDesignBlock` and audit the
~50 stages; most behavioral `Drop*`/`Add*` stages become no-ops over interface blocks.

1. **InterfaceSanityCheck** — verify view directionality consistency and that `<>`
   connections match port-per-port through nesting; enforce the SV/VHDL legality rules
   we depend on (no upward refs in a view, every element directed, etc.).
2. **FlattenInterfaces** (the portable baseline, default-on) — a `HierarchyStage` that
   expands each interface instance (`DFDesignInst` → interface block) into its individual
   scalar `DFVal.Dcl`s and rewrites each view connection (`DFView`-typed `DFNet`) into
   per-port `DFNet`s. Model it on `DropStructsVecs` / `DropLocalBlocks`. After this stage
   the rest of the pipeline + existing Verilog/VHDL printers work with **zero** interface
   awareness. *This is what lands first and makes the feature usable everywhere.*
3. **Native emission gating** — when the target dialect supports it (SV2017/SV2019,
   VHDL-2019), *skip* `FlattenInterfaces` and instead keep interfaces structural for the
   printer. Always **flatten the top-design boundary** to scalar ports regardless
   (Verilator/synthesis rule). Guard nested interfaces & multi-dim vector views (lower
   them even in native mode if the target can't take them).

Placement: sanity check early (post-elaboration); `FlattenInterfaces` late, before
backend-prep, alongside the other `Drop*` lowering stages.

---

## 5. Backends / printers

- Abstract: `compiler/ir/.../printing/{DFOwnerPrinter,DFValPrinter,DFTypePrinter}.scala`.
- `VerilogPrinter.scala` (dialects Verilog2005 / SV2017 / SV2019),
  `VHDLPrinter.scala` (87/93/2008 — **add 2019**).

The projection `terminal` (§2.2c) drives backend choice: a `Flip`/`FlipAll` projection lets
**VHDL-2019 emit natively** a `view Vc is V'converse` alias (`FlipAll` is the exact converse;
a `Flip` with anchored ports falls back to a concrete `view` since `'converse` would also flip
the anchored leaves). **SV and the flatten path materialize every terminal** from the resolved
`DFView.projectedFieldMap` directions (concrete modport / scalar-port directions), since
neither has a converse operator. `Monitor`/`Driver` always emit concrete directions.

**SystemVerilog (SV2017/SV2019, opt-in):**
- Emit `interface name #(params)(...); <vars> <modports> endinterface` per interface decl.
- One `modport` per view; directions from consumer's viewpoint; `flip` ⇒ inverted modport.
- Instances as `IfcType #(args) inst(...)`; **named** port connections; modport selected
  at the port declaration of the consuming module.
- Arrays: single-dim, module-scope, `generate`-loop connect; slices without modport
  qualifier. Lower nested interfaces & multi-dim before printing.
- Never an interface port on the top module (top is flattened scalars).

**VHDL-2019 (opt-in, target NVC):**
- Emit `record` type (unresolved, all elements listed) + `view V of Rec is ...` +
  `view Vc is V'converse` for `flip`. Ports as `bus : view V`; connecting signals are
  plain records. Arrays as `view (V) of ArrType(range)`. Place views in a package with
  the record.
- **Default VHDL stays flattened** (GHDL/synthesis lack mode views). Optionally offer a
  VHDL-2008 **split-record** fallback (one `in` record + one `out` record per view) as a
  middle ground; the LRM defines mode-view ports as exactly equivalent to per-element
  ports, so all three forms are semantically identical.

---

## 6. Phasing

0. **IR foundations** — add `InstMode.Interface` + `isInterface`; add the `DFInterface`
   `DFType` (identified by `interfaceRef`, OneWay → its block, like `designRef`; `fieldMap`
   carries each field's `dir`) and the single concrete `DFView` `DFType` with the
   `flip`/`flipAll`/`monitor`/`driver` transforms (ReadWriter/`=~`/`copyWithNewRefs`); add the
   IR-only `Dir.VIEW(site)` modifier with `ViewSite{Template, Projection(terminal: Terminal)}`
   (+ `isView`/`isViewTemplate`/`isViewProjection` predicates); remove the
   `DFInterfaceOwner` skeleton + its `<->` `DFNet` path.
1. **Frontend** — `DFInterface`/`RTInterface`/`EDInterface` + `DFC.Scope.Interface`
   guard, `view` builders, anchored ports (`<> IN`/`<> OUT`), projection terminals
   (`ASIS`/`FLIP`/`FLIPALL`/`MONITOR`/`DRIVER`), `X` vectors, `flip`, `<>` connect typing.
2. **Elaboration + InterfaceSanityCheck** — direction/connectivity rules through nesting.
3. **FlattenInterfaces** — portable end-to-end; existing backends emit correct HDL on
   every tool. *Feature is usable here.*
4. **Native SystemVerilog** emission (SV2017/SV2019) + top-boundary flatten + nested/
   multi-dim lowering guards.
5. **Native VHDL-2019** mode-view emission (NVC) + optional VHDL-2008 split-record
   fallback.
6. **Polish** — vector/nested views in native backends, tool-specific guards,
   `docExamples` + `StagesSpec` reference outputs for each dialect.

Verification ladder per phase: targeted `Playground` iteration → individual
`StagesSpec.*` → `testOnly StagesSpec.*` → full `test`; then `testApps` for the
flattened path against installed simulators.
