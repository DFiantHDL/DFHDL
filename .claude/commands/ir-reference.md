# DFHDL IR Reference

> **For contributors working with the DFHDL compiler internals.**
> This skill is version-controlled alongside the codebase — keep it updated when IR types or analysis helpers change.

Complete reference for the IR (intermediate representation) layer at
`compiler/ir/src/main/scala/dfhdl/compiler/ir/` and the analysis helpers at
`compiler/ir/src/main/scala/dfhdl/compiler/analysis/`.

This is the data model that every stage `transform` method pattern-matches on and patches.
See `/new-stage` for how to write a stage that uses this reference.

---

## Top-level sealed hierarchy

```
DFMember  (sealed)
├── DFMember.Empty          — sentinel / placeholder (no owner, no meta)
├── DFVal                   — any value in the design
│   ├── DFVal.Const             — literal constant
│   ├── DFVal.Dcl               — port / variable / const declaration
│   ├── DFVal.Func              — computed expression / operator
│   ├── DFVal.Alias             — alias / cast / partial selection
│   │   ├── DFVal.Alias.AsIs        — type cast  (.as(T)  /  .actual)
│   │   ├── DFVal.Alias.History     — prev / pipe  (.prev, .reg)
│   │   ├── DFVal.Alias.ApplyRange  — bit-range slice  (x(hi, lo))
│   │   ├── DFVal.Alias.ApplyIdx    — vector / bits indexing  (x(i))
│   │   └── DFVal.Alias.SelectField — struct field  (x.fieldName)
│   ├── DFVal.DesignParam       — design-level parameter reference
│   ├── DFVal.PortByNameSelect  — port selected by string path
│   └── DFVal.Special           — NOTHING | OPEN | CLK_FREQ
├── Statement               — executable statements
│   ├── DFNet                   — assignment / connection
│   ├── Wait                    — process wait
│   ├── TextOut                 — print / assert / finish
│   └── Goto                    — FSM step jump
├── DFBlock                 — containers for child members
│   ├── DFDesignBlock           — module / design definition
│   ├── DomainBlock             — clock-domain grouping
│   ├── ProcessBlock            — always / process block
│   ├── StepBlock               — FSM step
│   ├── DFConditional.Block     — if / match clause
│   │   ├── DFIfElseBlock
│   │   └── DFCaseBlock
│   └── DFLoop.Block            — loop body
│       ├── DFForBlock
│       └── DFWhileBlock
├── DFConditional.Header    — if / match header expression
│   ├── DFIfHeader
│   └── DFMatchHeader
├── DFInterfaceOwner        — interface abstraction
└── DFRange                 — for-loop range
```

Every `DFMember` carries three common fields:

| Field | Type | Purpose |
|---|---|---|
| `ownerRef` | `DFOwner.Ref` | Points to the enclosing owner/block |
| `meta` | `Meta` | Name, source position, doc, annotations |
| `tags` | `DFTags` | Extensible tag map |

---

## DFMember — common API

```scala
// Navigation (all require using MemberGetSet)
m.getOwner                    // immediate owner (throws if global)
m.getOwnerBlock               // nearest enclosing DFBlock
m.getOwnerDesign              // nearest enclosing DFDesignBlock
m.getOwnerDomain              // nearest DFDomainOwner (design, domainBlock)
m.getOwnerProcessBlock        // nearest ProcessBlock
m.getOwnerStepBlock           // nearest StepBlock
m.getThisOrOwnerDesign        // this if DFDesignBlock, else getOwnerDesign
m.getThisOrOwnerDomain        // this if DFDomainOwner, else getOwnerDomain
m.isMemberOf(owner)           // direct child of owner?
m.isInsideOwner(owner)        // anywhere inside owner at any depth?
m.isSameOwnerDesignAs(that)   // same immediate design?
m.getOwnerChain               // List[DFBlock] from root to direct owner

// Domain checks (extension, compiler.ir package)
m.getDomainType               // DomainType of nearest domain owner
m.isInDFDomain                // true iff DF domain
m.isInRTDomain                // true iff RT domain
m.isInEDDomain                // true iff ED domain
m.isInProcess                 // true iff inside a ProcessBlock

// Tag helpers (extension)
m.getTagOf[MyTag]             // Option[MyTag]
m.hasTagOf[MyTag]             // Boolean

// Meta helpers (on DFMember.Named)
m.isAnonymous                 // meta.nameOpt.isEmpty
m.getName                     // resolved name string
m.getFullName                 // fully-qualified  owner.owner.name
m.getRelativeName(callOwner)  // shortest unambiguous name from callOwner

// References
m.getRefs                     // List[DFRef.TwoWayAny] — all two-way refs in this member
m.copyWithNewRefs             // deep copy with fresh reference IDs (using RefGen)
```

---

## DFVal — value subtypes

### DFVal trait (common)

```scala
dfVal.dfType          // DFType
dfVal.width           // Int  (delegates to dfType)
dfVal.isGlobal        // true when ownerRef points to DFMember.Empty
dfVal.isAnonymous     // true when meta.nameOpt.isEmpty
dfVal.isFullyAnonymous // true when this and all its arg deps are anonymous
dfVal.isConst         // true when getConstData.nonEmpty
dfVal.getConstData    // Option[Any] — constant-fold result (cached)
dfVal.isSimilarTo(that) // semantic equivalence (type + data, ignoring identity)
dfVal.updateDFType(t) // create copy with new DFType
```

**Sub-trait markers:**
- `DFVal.CanBeExpr` — can appear as an expression (Const, Func, Alias, DesignParam, Special)
- `DFVal.CanBeGlobal` — can live at top-level scope (Const, Func, Alias.Partial)

**Extension methods on any DFVal:**

```scala
dfVal.isPort          // Dcl with IN/OUT/INOUT modifier
dfVal.isPortIn        // Dcl with IN modifier
dfVal.isPortOut       // Dcl with OUT modifier
dfVal.isVar           // Dcl with VAR modifier
dfVal.isReg           // Dcl with REG special modifier
dfVal.isOpen          // DFVal.Special(OPEN)
dfVal.isDesignParam   // DFVal.DesignParam instance
dfVal.dealias         // follow alias chain → Option[DFVal.Dcl | DFVal.Special]
dfVal.departial       // strip partial selections → (DFVal, Range)
dfVal.departialDcl    // strip partial selections → Option[(DFVal.Dcl, Range)]
dfVal.stripPortSel    // PortByNameSelect → underlying Dcl, else identity
dfVal.isBubble        // contains a don't-care / bubble constant
```

---

### DFVal.Const

```scala
final case class DFVal.Const(
    dfType:   DFType,
    data:     Any,           // actual value; cast to dfType.Data
    ownerRef: DFOwner.Ref,
    meta:     Meta,
    tags:     DFTags
)
```

Extends `CanBeExpr`, `CanBeGlobal`.
`data` is typed as `dfType.Data` — e.g. `Option[BigInt]` for `DFUInt`, `(BitVector, BitVector)` for `DFBits`.

---

### DFVal.Dcl

```scala
final case class DFVal.Dcl(
    dfType:      DFType,
    modifier:    DFVal.Modifier,   // dir + special
    initRefList: List[Dcl.InitRef],
    ownerRef:    DFOwner.Ref,
    meta:        Meta,
    tags:        DFTags
)
// companion
type Dcl.InitRef = DFRef.TwoWay[DFVal, Dcl]
```

**`DFVal.Modifier`:**

```scala
final case class Modifier(dir: Modifier.Dir, special: Modifier.Special)
enum Dir:     VAR, IN, OUT, INOUT
enum Special: Ordinary, REG, SHARED

mod.isPort    // IN | OUT | INOUT
mod.isReg     // special == REG
mod.isShared  // special == SHARED
```

**Extension methods on `Dcl`:**

```scala
dcl.initList          // List[DFVal]  — resolved init values
dcl.isClkDcl          // dfType is DFOpaque(kind = Clk)
dcl.isRstDcl          // dfType is DFOpaque(kind = Rst)
dcl.hasNonBubbleInit  // initRefList non-empty and first element is not bubble
```

---

### DFVal.Func

```scala
final case class DFVal.Func(
    dfType:   DFType,
    op:       Func.Op,
    args:     List[DFVal.Ref],   // type DFVal.Ref = DFRef.TwoWay[DFVal, DFMember]
    ownerRef: DFOwner.Ref,
    meta:     Meta,
    tags:     DFTags
)
```

Extends `CanBeExpr`, `CanBeGlobal`.

**`Func.Op` enum** (all operators):
```
+  -  *  /  ===  =!=  <  >  <=  >=  &  |  ^  %  ++
>>  <<  **  ror  rol  reverse  repeat
unary_-  unary_~  unary_!
rising  falling
clog2  max  min  abs  sel
InitFile(format, path)
```

`Func.Op.associativeSet` = `{+, -, *, &, |, ^, ++, max, min}`

---

### DFVal.Alias subtypes

All aliases share:
```scala
val relValRef: DFRef.TwoWay[DFVal, Alias]   // the value being aliased
```

Two sub-traits:
- `Alias.Consumer` — `relValRef: ConsumerRef` — consumes the source entirely (History)
- `Alias.Partial` — `relValRef: PartialRef` — partial view of source; propagates mutability (AsIs, ApplyRange, ApplyIdx, SelectField)

#### DFVal.Alias.AsIs
```scala
final case class DFVal.Alias.AsIs(
    dfType:   DFType,          // target type (may differ from relVal.dfType for casts)
    relValRef: PartialRef,
    ownerRef: DFOwner.Ref,
    meta:     Meta,
    tags:     DFTags
)
```
Used for: `.as(T)` type casts, `.actual` on opaques, `IdentTag`-tagged identity aliases.

Tag-based extractors:
```scala
Ident(underlying)    // AsIs tagged IdentTag  → underlying DFVal
Bind(underlying)     // Alias tagged BindTag  → underlying DFVal
OpaqueActual(relVal) // AsIs where relVal.dfType is DFOpaque and alias.dfType == actualType
AsOpaque(relVal)     // AsIs where alias.dfType is DFOpaque and relVal.dfType == actualType
```

#### DFVal.Alias.History
```scala
final case class DFVal.Alias.History(
    dfType:        DFType,
    relValRef:     ConsumerRef,
    step:          Int,
    op:            History.Op,      // State | Pipe
    initRefOption: Option[History.InitRef],
    ownerRef:      DFOwner.Ref,
    meta:          Meta,
    tags:          DFTags
)
enum History.Op: State, Pipe   // State = .prev in DF / .reg in RT; Pipe = DF pipe constraint

history.initOption              // Option[DFVal]
history.hasNonBubbleInit        // Boolean
```

#### DFVal.Alias.ApplyRange
```scala
final case class DFVal.Alias.ApplyRange(
    dfType:    DFType,
    relValRef: PartialRef,
    idxHighRef: IntParamRef,    // high bit index (inclusive)
    idxLowRef:  IntParamRef,    // low  bit index (inclusive)
    ownerRef:  DFOwner.Ref,
    meta:      Meta,
    tags:      DFTags
)
applyRange.elementWidth   // width of each element in the range
```

#### DFVal.Alias.ApplyIdx
```scala
final case class DFVal.Alias.ApplyIdx(
    dfType:   DFType,
    relValRef: PartialRef,
    relIdx:   DFVal.Ref,        // the index value
    ownerRef: DFOwner.Ref,
    meta:     Meta,
    tags:     DFTags
)
// Companion extractor for compile-time constant indices:
DFVal.Alias.ApplyIdx.ConstIdx(idx: Int)  // unapply on DFVal.Const → Option[Int]
```

#### DFVal.Alias.SelectField
```scala
final case class DFVal.Alias.SelectField(
    dfType:    DFType,
    relValRef: PartialRef,
    fieldName: String,
    ownerRef:  DFOwner.Ref,
    meta:      Meta,
    tags:      DFTags
)
```

---

### DFVal.DesignParam
```scala
final case class DFVal.DesignParam(
    dfType:        DFType,
    defaultValRef: DesignParam.DefaultValRef,  // → default value or DFMember.Empty
    ownerRef:      DFOwner.Ref,
    meta:          Meta,
    tags:          DFTags
)
type DesignParam.DefaultValRef = DFRef.TwoWay[DFVal | DFMember.Empty, DesignParam]
```

Key accessor methods on `DesignParam`:
- `param.appliedValRefOpt` — `Option[DFDesignBlock.ParamRef]` — `Some` when the owner design has an applied value in its `paramMap`, `None` otherwise
- `param.appliedValOpt` — `Option[DFVal]` — resolved applied value (if any)
- `param.appliedOrDefaultValRef` — `DFVal.Ref` — applied ref if present, else `defaultValRef`
- `param.appliedOrDefaultVal` — `DFVal` — applied value if present, else default value

---

### DFVal.Special
```scala
final case class DFVal.Special(
    dfType:   DFType,
    kind:     Special.Kind,
    ownerRef: DFOwner.Ref,
    meta:     Meta,
    tags:     DFTags
)
enum Special.Kind: NOTHING, OPEN, CLK_FREQ
```

---

### DFVal.PortByNameSelect
```scala
final case class DFVal.PortByNameSelect(
    dfType:         DFType,
    designInstRef:  PortByNameSelect.Ref,   // → DFDesignInst
    portNamePath:   String,
    ownerRef:       DFOwner.Ref,
    meta:           Meta,
    tags:           DFTags
)
type PortByNameSelect.Ref = DFRef.TwoWay[DFDesignInst, PortByNameSelect]

portByNameSelect.getPortDcl   // resolve to actual DFVal.Dcl (via DB.dupPortsByName)

// Extractor:
DFVal.PortByNameSelect.Of(dcl)  // unapply → Option[DFVal.Dcl]
```

---

## Statement subtypes

### DFNet (assignment / connection)

```scala
final case class DFNet(
    lhsRef:   DFNet.Ref,   // DFRef.TwoWay[DFVal | DFInterfaceOwner, DFNet]
    op:       DFNet.Op,
    rhsRef:   DFNet.Ref,
    ownerRef: DFOwner.Ref,
    meta:     Meta,
    tags:     DFTags
)
enum DFNet.Op: Assignment, NBAssignment, Connection, ViaConnection, LazyConnection
```

**Extension methods:**
```scala
net.isAssignment    // Assignment | NBAssignment
net.isConnection    // Connection | ViaConnection | LazyConnection
net.isViaConnection
net.isLazyConnection
```

**Pattern extractors** (most useful in stages):
```scala
DFNet.Assignment(toVal, fromVal)   // Assignment or NBAssignment; toVal and fromVal are DFVal
DFNet.BAssignment(toVal, fromVal)  // blocking only (op == Assignment)
DFNet.NBAssignment(toVal, fromVal) // non-blocking only (op == NBAssignment)
DFNet.Connection(toVal, fromVal, swapped)
  // toVal: DFVal.Dcl | DFVal.Special | DFInterfaceOwner
  // fromVal: DFVal | DFInterfaceOwner
  // swapped: Boolean — true if lhs/rhs were physically reversed
```

---

### Wait
```scala
final case class Wait(
    triggerRef: Wait.TriggerRef,   // DFRef.TwoWay[DFVal, Wait]
    ownerRef:   DFOwner.Ref,
    meta:       Meta,
    tags:       DFTags
)
```

---

### TextOut
```scala
final case class TextOut(
    op:       TextOut.Op,
    msgParts: List[String],         // literal string segments
    msgArgs:  List[DFVal.Ref],      // interpolated value arguments
    ownerRef: DFOwner.Ref,
    meta:     Meta,
    tags:     DFTags
)
enum TextOut.Op:
  case Print, Println, Debug, Finish
  case Report(severity: Severity)
  case Assert(assertionRef: AssertionRef, severity: Severity)
enum TextOut.Severity: Info, Warning, Error, Fatal
```

---

### Goto
```scala
final case class Goto(
    stepRef:  Goto.Ref,   // DFRef.TwoWay[StepBlock | ThisStep | NextStep | FirstStep, Goto]
    ownerRef: DFOwner.Ref,
    meta:     Meta,
    tags:     DFTags
)
// Special step marker singletons:
case object ThisStep  extends DFMember.Empty
case object NextStep  extends DFMember.Empty
case object FirstStep extends DFMember.Empty
```

---

## DFBlock subtypes

### DFDesignBlock  (also aliased as `DFDesignInst`)
```scala
final case class DFDesignBlock(
    domainType: DomainType,
    dclMeta:    Meta,         // declaration-site meta (class name, position, …)
    instMode:   DFDesignBlock.InstMode,
    ownerRef:   DFOwner.Ref,
    meta:       Meta,         // instance-site meta (val name, position)
    tags:       DFTags
)
enum InstMode: Normal, Def, Simulation
enum InstMode.BlackBox: NA, Files(path), Library(libName, nameSpace), VendorIP(vendor, typeName)
```

**Extension methods:**
```scala
design.isDuplicate          // tagged DuplicateTag — has NO members in DB (ports/domains removed)
                            // Use dupPortsByName/dupDesignDomainBlockMap for synthetic members
design.isBlackBox           // instMode is BlackBox
design.isVendorIPBlackbox
design.inSimulation         // instMode is Simulation
design.isTop                // no ownerRef pointing to another design
design.dclName              // design class name (from dclMeta)
design.getCommonDesignWith(other)  // nearest common ancestor DFDesignBlock
```

**Companion extractors:**
```scala
DFDesignBlock.Top()   // matches the top-level design (isTop == true)
```

---

### DomainBlock
```scala
final case class DomainBlock(
    domainType: DomainType,
    ownerRef:   DFOwner.Ref,
    meta:       Meta,
    tags:       DFTags
)
```

---

### ProcessBlock
```scala
final case class ProcessBlock(
    sensitivity: ProcessBlock.Sensitivity,
    ownerRef:    DFOwner.Ref,
    meta:        Meta,
    tags:        DFTags
)
sealed trait ProcessBlock.Sensitivity
case object ProcessBlock.Sensitivity.All                          // process(all)
final case class ProcessBlock.Sensitivity.List(refs: List[DFVal.Ref])  // process(x, y)
```

---

### StepBlock
```scala
final case class StepBlock(
    ownerRef: DFOwner.Ref,
    meta:     Meta,
    tags:     DFTags
)
// Extension methods:
stepBlock.isOnEntry     // getName == "onEntry"
stepBlock.isOnExit      // getName == "onExit"
stepBlock.isFallThrough // getName == "fallThrough"
stepBlock.isRegular     // none of the above
```

---

### DFConditional

#### DFMatchHeader
```scala
final case class DFMatchHeader(
    dfType:      DFType,
    selectorRef: DFVal.Ref,
    ownerRef:    DFOwner.Ref,
    meta:        Meta,
    tags:        DFTags
)
```

#### DFCaseBlock
```scala
final case class DFCaseBlock(
    pattern:              DFCaseBlock.Pattern,
    guardRef:             Block.GuardRef,          // optional boolean guard
    prevBlockOrHeaderRef: DFCaseBlock.Ref,
    ownerRef:             DFOwner.Ref,
    meta:                 Meta,
    tags:                 DFTags
)
```

**`DFCaseBlock.Pattern` hierarchy:**
```scala
sealed trait Pattern
case object CatchAll                                               // case _
final case class Singleton(valueRef: DFVal.Ref)                   // case 42
final case class Alternative(list: List[Pattern])                  // case 1 | 2 | 3
final case class Struct(name: String, fieldPatterns: List[Pattern])// case MyStruct(...)
final case class Bind(ref: Bind.Ref, pattern: Pattern)             // case x @ pattern
final case class NamedArg(name: String, pattern: Pattern)          // named arg
final case class BindSI(op, parts, refs)                           // string-interpolation bind
```

#### DFIfHeader
```scala
final case class DFIfHeader(
    dfType:   DFType,
    ownerRef: DFOwner.Ref,
    meta:     Meta,
    tags:     DFTags
)
```

#### DFIfElseBlock
```scala
final case class DFIfElseBlock(
    guardRef:             Block.GuardRef,           // Some → if/else-if guard; None → else
    prevBlockOrHeaderRef: DFIfElseBlock.Ref,
    ownerRef:             DFOwner.Ref,
    meta:                 Meta,
    tags:                 DFTags
)
```

---

### DFLoop

#### DFForBlock
```scala
final case class DFForBlock(
    iteratorRef: DFForBlock.IteratorRef,  // DFRef.TwoWay[DFVal.Dcl, DFForBlock]
    rangeRef:    DFForBlock.RangeRef,     // DFRef.TwoWay[DFRange, DFForBlock]
    ownerRef:    DFOwner.Ref,
    meta:        Meta,
    tags:        DFTags
)
```

#### DFWhileBlock
```scala
final case class DFWhileBlock(
    guardRef: DFWhileBlock.GuardRef,  // DFRef.TwoWay[DFVal, DFWhileBlock]
    ownerRef: DFOwner.Ref,
    meta:     Meta,
    tags:     DFTags
)
```

#### DFLoop.Block extension methods
```scala
loop.isCombinational   // tagged CombinationalTag
loop.isFallThrough     // tagged FallThroughTag
```

---

### DFRange
```scala
final case class DFRange(
    startRef: DFRange.Ref,   // DFRef.TwoWay[DFVal, DFRange]
    endRef:   DFRange.Ref,
    op:       DFRange.Op,
    stepRef:  DFRange.Ref,
    ownerRef: DFOwner.Ref,
    meta:     Meta,
    tags:     DFTags
)
enum DFRange.Op: Until, To
```

---

## DFType hierarchy

```
DFType  (sealed)
├── DFBoolOrBit  (sealed)
│   ├── DFBool          — boolean, width = 1
│   └── DFBit           — single hardware bit, width = 1
├── DFBits(widthParamRef)          — bit vector
├── DFDecimal(signed, widthParamRef, fractionWidth, nativeType)
│   ├── DFUInt(w)        — unsigned integer
│   ├── DFSInt(w)        — signed integer
│   └── DFInt32          — standard 32-bit signed (nativeType = Int32)
├── DFEnum(name, widthParam, entries: ListMap[String, BigInt])
├── DFDouble             — 64-bit floating point
├── DFString             — unbounded string
├── DFVector(cellType, cellDimParamRefs)   — ordered collection
├── DFStruct(name, fieldMap: ListMap[String, DFType])
├── DFOpaque(name, kind, id, actualType)
├── DFUnit               — unit / void
├── DFNothing            — bottom type
├── DFTime               — physical time quantity
├── DFFreq               — physical frequency quantity
└── DFNumber             — dimensionless literal number
```

**Type marker traits:**
- `ComposedDFType` — has inner types (Vector, Struct, Opaque)
- `NamedDFType` — has a `name: String` (Struct, Opaque, Enum)

**Common DFType API:**
```scala
dfType.width              // Int (requires MemberGetSet for parameterised widths)
dfType.getRefs            // List[DFRef.TypeRef]
dfType.copyWithNewRefs    // using RefGen
dfType.isSimilarTo(that)  // structural equivalence ignoring identity
```

**DFVector helpers:**
```scala
vector.length             // Int
vector.cellType           // DFType of each element
```

**DFStruct helpers:**
```scala
struct.fieldMap           // ListMap[String, DFType]  (ordered)
struct.fieldIndex(name)   // Int — field index
struct.fieldRelBitLow(name) // Int — bit offset of field within struct
```

**DFOpaque.Kind:**
```scala
sealed trait Kind
case object General extends Kind
sealed trait Magnet extends Kind
case object Clk     extends Magnet
case object Rst     extends Magnet
case object Magnet  extends Magnet    // generic magnet

opaque.isMagnet   // kind.isInstanceOf[Magnet]
```

**`IntParamRef`** — used for widths and indices in DFBits, DFDecimal, DFVector, ApplyRange:
```scala
opaque type IntParamRef = DFRef.TypeRef | Int
paramRef.getInt             // resolve to Int (using MemberGetSet)
paramRef.isInt              // true if literal
paramRef.isRef              // true if parameterised
```

---

## DomainType

```scala
enum DomainType:
  case DF              // dataflow (timing-agnostic)
  case RT(cfg: RTDomainCfg)  // register-transfer
  case ED              // event-driven (Verilog/VHDL semantics)
```

### RTDomainCfg
```scala
enum RTDomainCfg:
  case Derived                                             // inherit from context
  case Related(relatedDomainRef: RTDomainCfg.RelatedDomainRef)
  case Explicit(name: String, clkCfg: ClkCfg, rstCfg: RstCfg)
```

### ClkCfg.Explicit
```scala
final case class ClkCfg.Explicit(
    edge:             ClkCfg.Edge,     // Rising | Falling
    rate:             RateNumber,
    portName:         String,
    inclusionPolicy:  ClkRstInclusionPolicy
)
enum ClkCfg.Edge: Rising, Falling
```

### RstCfg.Explicit
```scala
final case class RstCfg.Explicit(
    mode:             RstCfg.Mode,     // Async | Sync
    active:           RstCfg.Active,   // Low | High
    portName:         String,
    inclusionPolicy:  ClkRstInclusionPolicy
)
enum RstCfg.Mode:   Async, Sync
enum RstCfg.Active: Low, High
enum ClkRstInclusionPolicy: AsNeeded, AlwaysAtTop
```

---

## Meta

```scala
final case class Meta(
    nameOpt:     Option[String],
    position:    Position,
    docOpt:      Option[String],
    annotations: List[HWAnnotation]
)
meta.isAnonymous     // nameOpt.isEmpty
meta.name            // generated name if anonymous, else nameOpt.get
meta.anonymize()     // copy with nameOpt = None
meta.setName(name)
```

---

## DFTags

```scala
opaque type DFTags = Map[String, DFTag]
tags.tag[CT <: DFTag](t)     // add tag, returns new DFTags
tags.removeTagOf[CT]          // remove tag by type
tags.getTagOf[CT]             // Option[CT]
tags.hasTagOf[CT]             // Boolean
tags.++(that)                 // merge two DFTags
DFTags.empty
```

**Built-in tags:**
```scala
case object DuplicateTag      // duplicate design instance — NO members in DB; use dupPortsByName
case object IteratorTag       // Dcl is a for-loop iterator variable
case object IdentTag          // Alias.AsIs is a pure identity (named alias of itself)
case object BindTag           // Alias is a pattern-match bind variable
case object CombinationalTag  // loop/block is combinational (no cycles)
case object FallThroughTag    // loop/block falls through to next step
case class  DefaultRTDomainCfgTag(cfg: RTDomainCfg.Explicit)
case object ResizeTag
case class  DFHDLVersionTag(version: String)
```

---

## Reference types

```scala
sealed trait DFRef[+M <: DFMember]:
  val grpId: (Int, Int)   // group (position hash, counter)
  val id:    Int
  def get(using MemberGetSet): M
  def getOption(using MemberGetSet): Option[M]
  def copyAsNewRef(using RefGen): this.type

type DFRefAny = DFRef[DFMember]
```

**Subtypes:**
- `DFRef.OneWay[M]` — unidirectional (e.g. `ownerRef`)
- `DFRef.TwoWay[M, O]` — bidirectional; `O` is the member that owns this ref (enables reverse lookup)
- `DFRef.TypeRef` — used for `IntParamRef` (width/index parameters)
- `DFRef.DuplicationRef(owner: DFOwnerNamed)` — special `OneWay` ref for analysis-only members (not in `refTable` or `members`). Overrides `get` to return `owner` directly, bypassing `MemberGetSet` lookup. Used by `dupPortsByName` and `dupDesignDomainBlockMap` to create synthetic port Dcls and domain blocks for duplicate designs.

**Pattern extractor** (very common in stages):
```scala
DFRef(member)   // matches any DFRef and extracts the resolved member
// Example:
case DFNet(DFRef(lhs: DFVal), _, DFRef(rhs: DFVal), _, _, _) => ...
```

### RefGen

```scala
class RefGen private (magnetID, grpId, lastId):
  def genOneWay[M](): DFRef.OneWay[M]
  def genTwoWay[M, O](): DFRef.TwoWay[M, O]
  def genTypeRef(): DFRef.TypeRef
  def getGrpId: (Int, Int)
  def setGrpId(id: (Int, Int)): Unit

RefGen.initial          // fresh RefGen (grpId = (0,0))
RefGen.fromGetSet       // RefGen seeded from the current DB's existing IDs
```

---

## DB — the design database

```scala
final case class DB(
    members:    List[DFMember],             // ordered flat list (top-design first)
    refTable:   Map[DFRefAny, DFMember],
    globalTags: DFTags,
    srcFiles:   List[SourceFile]
)
```

**Key computed properties:**
```scala
db.top                    // DFDesignBlock — first member, always the root design
db.topIOs                 // List[DFVal.Dcl] — ports of the top design
db.getSet                 // MemberGetSet (immutable)
db.memberTable            // Map[DFMember, Set[DFRefAny]] — member → refs pointing to it
db.originRefTable         // Map[DFRef.TwoWayAny, DFMember] — ref → member that owns it
db.originMemberTable      // Map[DFMember, Set[DFMember]] — member → members referencing it
db.ownerMemberList        // List[(DFOwner, List[DFMember])] — members grouped by owner
db.membersNoGlobals       // members excluding global values
db.membersGlobals         // global CanBeGlobal values only
db.inSimulation           // top has no ports (simulation context)
db.inBuild                // top has a device constraint tag
```

**Design duplication properties:**

Duplicate designs (tagged `DuplicateTag`) have **no members** in the DB — their ports, domain blocks, and other members are removed during immutable DB creation. Instead, synthetic members with `DuplicationRef` owners are created on-demand:

```scala
db.dupDesignToOrigMap     // Map[DFDesignBlock, DFDesignBlock] — dup design → origin design
db.dupPortsByName         // Map[DFDesignInst, ListMap[String, DFVal.Dcl]] — design → named ports
                          //   includes both real ports (for origins) and DuplicationRef-backed
                          //   synthetic ports (for duplicates, with PBNS dfType overrides)
db.dupDesignDomainBlockMap // Map[(DFDesignBlock, DomainBlock), DomainBlock]
                          //   maps (dupDesign, origDomainBlock) → dupDomainBlock
db.dupDomainOwnerPublicMemberList  // List[(DFDomainOwner, List[DFMember])]
                          //   domain owners with their public members (ports, designs, domains),
                          //   including dup-copy entries for duplicate designs
db.dupDomainOwnerPublicMemberTable // Map form of the above
```

**Important:** `dupPortsByName` is the primary port lookup — use it instead of iterating `members` for ports. It handles duplicate designs transparently. `getPortDcl` on `PortByNameSelect` uses it internally.

**Patching:**
```scala
db.patch(patches: List[(DFMember, Patch)]): DB
```

### MemberGetSet

```scala
trait MemberGetSet:
  val isMutable: Boolean
  def designDB: DB
  def apply[M <: DFMember, M0 <: M](ref: DFRef[M]): M0   // resolve ref → member
  def getOption[M <: DFMember, M0 <: M](ref: DFRef[M]): Option[M0]
  def getOrigin(ref: DFRef.TwoWayAny): DFMember            // reverse lookup
  def set[M <: DFMember](orig: M)(f: M => M): M            // update member
  def replace[M <: DFMember](orig: M)(updated: M): M
  def remove[M <: DFMember](member: M): M
```

`db.getSet` is immutable (`isMutable = false`). In stages, it is passed as `given` automatically:
```scala
def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB = ...
```

---

## Analysis extractors (dfhdl.compiler.analysis.*)

Import with `import dfhdl.compiler.analysis.*` (already in scope via the standard stage imports).
All require `using MemberGetSet`.

### DFVal extractors

| Extractor | Matches | Extracts |
|---|---|---|
| `DclVar()` | `DFVal.Dcl` with `modifier.dir == VAR` | Boolean (use in condition) |
| `DclConst()` | Any `DFVal.CanBeExpr` that is named and constant | Boolean |
| `DclPort()` | `DFVal.Dcl` with IN/OUT/INOUT | Boolean |
| `DclIn()` | `DFVal.Dcl` with IN | Boolean |
| `DclOut()` | `DFVal.Dcl` with OUT | Boolean |
| `IteratorDcl()` | `DFVal.Dcl` tagged `IteratorTag` | Boolean |
| `Ident(underlying)` | `DFVal.Alias.AsIs` tagged `IdentTag` | `DFVal` |
| `Bind(underlying)` | `DFVal.Alias` tagged `BindTag` | `DFVal` |
| `OpaqueActual(relVal)` | `AsIs` unwrapping an opaque | `DFVal` |
| `AsOpaque(relVal)` | `AsIs` casting to an opaque | `DFVal` |
| `ClkEdge(sig, edge)` | `DFVal.Func` with `rising`/`falling` op | `(DFVal, ClkCfg.Edge)` |
| `RstActive(sig, active)` | Reset condition expression | `(DFVal, RstCfg.Active)` |
| `BlockRamVar()` | `DFVal.Dcl` VAR of DFVector with only index accesses | Boolean |
| `DefaultOfDesignParam(dp)` | A value used as default of a DesignParam | `DFVal.DesignParam` |

### DFVal extension methods (analysis)

```scala
dfVal.getReadDeps             // Set[DFValReadDep]  — things that read this value
dfVal.getPartialAliases       // Set[DFVal.Alias.Partial]  — aliases of this value
dfVal.getConnectionTo         // Option[DFNet]  — single connection driving this value
dfVal.getConnectionsFrom      // Set[DFNet]  — connections driven from this value
dfVal.getAssignmentsTo        // Set[DFVal]  — values assigned to this
dfVal.getAssignmentsFrom      // Set[DFVal]  — values assigned from this
dfVal.getPortsByNameSelectors // List[DFVal.PortByNameSelect]  (ports only)
dfVal.isReferencedByAnyDclOrDesign // Boolean — true if referenced by a Dcl, DclConst, or DFDesignBlock
dfVal.isConstVAR              // VAR never assigned/connected
dfVal.isAllowedMultipleReferences // Boolean
dfVal.isPartialNetDest        // Boolean — is a partial assignment/connection target
dfVal.flatName                // String — name derived from structure if anonymous
dfVal.suggestName             // Option[String] — inferred name from context
dfVal.collectRelMembers(includeOrig) // List[DFVal] — this + all anonymous arg deps
```

**Reverse member lookup (any DFMember):**
```scala
member.originMembers          // Set[DFMember] — members whose refs point to this
member.originMembersNoTypeRef // Set[DFMember] — same, excluding TypeRef references
member.consumesCycles         // Boolean — true for Wait, StepBlock, Goto, non-comb loops
```

### ComposedDFTypeReplacement

For recursive type transformation across Struct/Vector/Opaque nesting:

```scala
class ComposedDFTypeReplacement[H](
    preCheck:   DFType => Option[H],
    updateFunc: PartialFunction[(DFType, H), DFType]
)(using MemberGetSet):
  def unapply(dfType: DFType): Option[DFType]
```

- `preCheck` — return `Some(helper)` to trigger replacement at this node; `None` to skip (but still recurse into composed children)
- `updateFunc` — given `(composedOrOriginal DFType, helper)` → produce the replacement type
- Recurses automatically into `DFStruct.fieldMap`, `DFVector.cellType`, `DFOpaque.actualType`

```scala
// Example: replace all DFOpaque types with their actualType
object DropOpaques extends ComposedDFTypeReplacement(
  preCheck   = { case dt: DFOpaque => Some(()); case _ => None },
  updateFunc = { case (dt: DFOpaque, _) => dt.actualType }
)
// Usage in pattern match:
case dfVal @ ComposedOpaqueDFValReplacement(updatedDFVal) =>
  dfVal -> Patch.Replace(updatedDFVal, Patch.Replace.Config.FullReplacement)
```
