# DFHDL Compiler Stage Creation Guide

> **For contributors adding or modifying compiler transformation stages in DFHDL.**
> This skill is version-controlled alongside the codebase — keep it updated when stage infrastructure changes.
> After completing a stage, **update this file** with any general lessons learned (new patterns, API
> behaviours, pitfalls). See the "Keeping This Skill Up to Date" section at the bottom for guidance.

You are helping create or modify a DFHDL compiler transformation stage. Use the complete reference below to produce correct, idiomatic code.

---

## Architecture Overview

A **stage** is a single transformation pass over the design IR (`DB`). Stages are chained by the `StageRunner`, which resolves `dependencies` recursively and re-runs any `nullified` stages when needed.

```
Design (Scala frontend) → DB → Stage1 → Stage2 → ... → StagedN → backend printer
```

The `DB` is an **immutable snapshot**: each stage receives the current `DB`, computes a patch list, and returns a new `DB`.

---

## Required Stage Properties

Every stage **must** satisfy three invariants. Violating any of them causes subtle, hard-to-debug compiler bugs.

### 1. Determinism — same input → same output, every time

Given the same input `DB`, a stage must always produce bit-for-bit the same output `DB`, regardless of how many times the overall compilation is run.

**Common causes of non-determinism to avoid:**
- Iterating over `Set`, `Map`, or any unordered collection to build the patch list — iteration order is not guaranteed. Always convert to a sorted or ordered structure first, or derive order from `designDB.members` (which is a `List` and is ordered).
- Using `hashCode`-based identity anywhere in the transformation logic.
- Relying on mutable external state (counters, caches, `var`s outside the `transform` call).

**Rule:** Build patch lists by iterating `designDB.members` (ordered) or `designDB.blockMemberList` (ordered). Never collect from a `Set` or `HashMap` directly.

### 2. Idempotency — `f(f(x)) == f(x)`

Applying a stage to its own output must yield the same output again. In other words, if the transformation function is `f`, then for every input DB `x`:

```
f(f(x)) == f(x)
```

This is a **design guideline**, not something that needs to be formally proved. The intent is that a stage should recognize when the IR is already in the form it produces and leave it unchanged. A stage that keeps mutating an already-transformed DB is a sign its pattern matches are too broad.

**How to achieve idempotency in practice:**
- **Match on the source form only.** Pattern-match on the exact IR shape that the stage is responsible for transforming. The transformed shape should not match the same pattern, so a second run produces an empty patch list.
- **Structural self-consistency.** After `f(x)` is applied, the resulting DB should contain no members that satisfy the stage's match predicates.
- **Injected members must consume their trigger.** A stage may inject/clone members for a
  downstream stage ONLY if the IR shape that triggered the injection is rewritten away by the
  same run (e.g. FlattenStepBlocks clones the process prologue at the wrap-around goto — but
  keyed on the relative `NextStep` form of that goto, which the same run resolves into a
  named goto, so a re-run finds no trigger). If the trigger cannot be consumed, carry the
  provenance as a **member tag** (`DFTag`) — but only one that satisfies rule 3 below.

### 3. Printability — every stage's output must survive a print/re-elaborate round trip

Take a stage's printed output, elaborate it as DFHDL source, and run the rest of the pipeline:
the result must be identical to passing the IR along directly. Printed code is the contract
between stages, so **no stage may depend on information that its own printout does not carry.**

This is what constrains tags. A tag is safe only when re-elaborating the printed form
reconstructs it:

- `CombinationalTag` prints as the `COMB_LOOP:` block wrapper and `FallThroughTag` as the
  `FALL_THROUGH(...)` marker on the construct's own condition or range (a loop's, or a
  `waitUntil`/`waitWhile`'s), so elaboration puts them back. Safe. Note the two differ in scope,
  and the printer must match: `COMB_LOOP` marks a region (every loop under it carries the tag, so
  it prints once on the outermost) while `FALL_THROUGH` marks one construct (each tagged one
  prints its own, including a nested opt-in).
- `IteratorTag`, `BindTag`, `IdentTag` and friends are re-derived by elaboration from the
  construct itself. Safe.
- A tag marking *why a stage synthesized a member* has no printed form and nothing regenerates
  it. **Unsafe** — and the failure is silent, because the IR path keeps working.

The trap is that a tag can be perfectly fix-point-safe (re-tagging is a no-op) and still break
this. Idempotency and printability are independent requirements.

The practical test: could a user have hand-written this printout, meaning something different?
A synthesized process bootstrap step prints as `def S_0: Step = NextStep` followed by the
prologue statements — byte-identical to a real first step followed by an inter-step statement.
Nothing in the printout distinguishes them, so no downstream stage may rely on the difference.

When you hit this, the fix is not a better marker; it is to **move the decision into the stage
that consumes it**, or to resolve the ambiguity before the boundary. The process bootstrap moved
from `DropRTWaits` into `FlattenStepBlocks` for exactly this reason: `DropRTWaits` still emits
relative gotos, so a `FirstStep` in its printout is ambiguous about the bootstrap, while by
`FlattenStepBlocks` every goto is explicit and the ambiguity is gone.

---

## Core Infrastructure

### `Stage` trait
```scala
// compiler/stages/src/main/scala/dfhdl/compiler/stages/Stage.scala
trait Stage extends Product, Serializable, HasTypeName derives CanEqual:
  final lazy val depSet: Set[Stage] = dependencies.toSet
  def dependencies: List[Stage]                                         // prerequisite stages, run first
  def nullifies: Set[Stage]                                             // stages that must re-run after this one
  def runCondition(using CompilerOptions): Boolean = true               // skip stage when false
  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB  // the transformation
```

### Special stage subtypes
```scala
sealed trait SpecialControlStage extends Stage   // skips sanity checks in trace logging

trait NoCheckStage extends SpecialControlStage   // use when stage intentionally leaves dangling anon refs

abstract class BundleStage(deps: Stage*) extends NoCheckStage:  // no-op, used purely for dependency ordering
  def transform(db: DB)(...): DB = db
```

### `HasDB` — accepts DB, Design, StagedDesign, or CompiledDesign as input
```scala
trait HasDB[T]:
  def apply(t: T): DB
extension [T: HasDB](t: T) def db: DB = summon[HasDB[T]](t)
```

### `StageRunner` — recursive dependency-aware executor
```scala
object StageRunner:
  def run(stage: Stage)(designDB: DB)(using CompilerOptions, PrinterOptions): DB
```
- Traverses `dependencies` depth-first before running a stage.
- Removes `nullified` stages from the "done" set so they re-run if needed later.
- Automatically runs `sanityCheck` after each non-`NoCheckStage`.
- At `TRACE` log level, prints the code string after each stage that changed the DB.

---

## IR Data Model

### `DB` — the design database (immutable)
```scala
// compiler/ir/src/main/scala/dfhdl/compiler/ir/DB.scala
case class DB(
  members: List[DFMember],               // ordered flat list of all IR members
  refTable: Map[DFRefAny, DFMember],     // reference → member resolution
  globalTags: Map[...],
  srcFiles: List[...],
):
  def getSet: MemberGetSet               // member-lookup context (needed as `given`)
  def top: DFDesignBlock                 // root design block
  def memberTable: Map[DFMember, ...]    // reverse lookup
  def patch(patches: List[(DFMember, Patch)]): DB
```

### `DFMember` hierarchy
```
DFMember (sealed)
├── DFVal          ─ any value in the design
│   ├── DFVal.Dcl        ─ port/var/const declaration
│   ├── DFVal.Func       ─ operation / function call
│   ├── DFVal.Alias      ─ alias / cast / selection
│   │   ├── DFVal.Alias.AsIs     ─ .as(T) cast
│   │   ├── DFVal.Alias.ApplyIdx ─ vector indexing
│   │   └── DFVal.Alias.SelectField ─ struct field
│   └── DFVal.Const      ─ literal constant
├── Statement      ─ assignment, connection, net
├── DFBlock        ─ container for members
│   ├── DFDesignBlock    ─ module / design definition
│   ├── ProcessBlock     ─ always/process block
│   ├── StepBlock        ─ RT step (FSM state)
│   └── DFConditional.Block ─ if/match/while clause
├── DFConditional.Header ─ if/match/while header
└── DFRange
```

### Useful extractor patterns (already in scope via `dfhdl.compiler.analysis.*`)
```scala
DclVar()           // matches DFVal.Dcl that is a variable
DclConst()         // matches DFVal.Dcl that is a constant
IteratorDcl()      // matches for-loop iterator declarations
DclBind()          // matches pattern-match bind declarations
Ident(underlying)  // matches named alias that just wraps another value
```

### Navigation helpers on `DFMember`
```scala
m.getOwner                    // immediate parent member
m.getOwnerBlock               // nearest enclosing block
m.getOwnerDesign              // nearest enclosing design block
m.getOwnerDomain              // nearest domain-bearing block
m.isAnonymous                 // true if the value has no user-visible name
m.originMembers               // members that reference this member
```

### `MemberView.Folded` vs `MemberView.Flattened`

```scala
owner.members(MemberView.Folded)     // direct children only: m.getOwner == owner
owner.members(MemberView.Flattened)  // all descendants recursively
```

Use `Folded` when processing only immediate children (e.g. iterating steps at one nesting level).
Use `Flattened` when you need to inspect or collect all descendants (e.g. finding all `Goto`
members anywhere inside a `ProcessBlock`, or collecting all members to move with a block).

A member's **direct owner** is the block for which `m.getOwner == block`. `Flattened` includes
members owned by nested blocks too, which can be confusing: a `Goto` inside a `StepBlock` inside
a `ProcessBlock` appears in `pb.members(Flattened)` but NOT in `pb.members(Folded)`.

### Domain checks
```scala
m.isInDFDomain    // dataflow domain
m.isInRTDomain    // register-transfer domain
m.isInProcess     // inside a process block
```

---

## Patch System

All mutations are expressed as a **patch list**: `List[(DFMember, Patch)]`. Apply them at the end:

```scala
designDB.patch(patchList)
```

`patch` returns the same `DB` instance when the list is empty — do NOT wrap the call in an
`if (patchList.isEmpty)` guard.

`patch` returns the same `DB` instance when the list is empty — do NOT wrap the call in an
`if (patchList.isEmpty)` guard.

### Patch types

| Patch | Effect |
|---|---|
| `Patch.Replace(newMember, cfg)` | Replace or re-reference a member |
| `Patch.Move(member, cfg)` | Move a member to a different position |
| `Patch.Add(dsn, cfg)` | Insert new members constructed via `MetaDesign` |
| `Patch.Remove(isMoved)` | Remove a member; `isMoved=true` preserves references |
| `Patch.ChangeRef(from, to)` | Redirect a single reference |

### `Patch.Replace` configs

```scala
Patch.Replace.Config.FullReplacement        // replaces the member in the member list
Patch.Replace.Config.ChangeRefOnly          // only updates references, keeps original in list
Patch.Replace.Config.ChangeRefAndRemove     // updates refs AND removes original from list
```

Optionally scope reference changes with a `RefFilter`:
```scala
Patch.Replace.RefFilter.OfMembers(memberSet)  // only refs from specific members
// or implement custom RefFilter:
object MyFilter extends Patch.Replace.RefFilter:
  def apply(refs: Set[DFRefAny])(using MemberGetSet): Set[DFRefAny] = refs.filter(...)
```

### `Patch.Move` configs
```scala
Patch.Move.Config.Before  // insert before the anchor member
Patch.Move.Config.After
Patch.Move.Config.InsideFirst  // insert as first child of a block
Patch.Move.Config.InsideLast
```

### `Patch.Move` semantics — critical details

**Descendants are NOT moved automatically.** `Patch.Move(List(member), origOwner, cfg)` only
moves the listed members. If `member` is a `DFOwner` (e.g. a `StepBlock`), its children remain
at their original positions in the flat member list. Since the flat member list must be a valid
pre-order DFS traversal (every member's owner must appear before it), moving a block without its
children violates the ownership invariant and causes `sanityCheck` to fail.

**Always include descendants when moving a block:**
```scala
val allMembersToMove = block :: block.members(MemberView.Flattened)
anchor -> Patch.Move(allMembersToMove, block.getOwner, Patch.Move.Config.After)
```

**Anchor redirect for `DFOwner` + `Config.After`.** When the anchor is a `DFOwner` and config
is `After`, the patch system redirects the physical insertion point to
`anchor.getVeryLastMember` (deepest recursive last descendant). However, the ownership update
uses the **original anchor** to determine the new owner: `newOwner = origAnchor.getOwnerBlock`.
This means after the move, the moved members' `ownerRef`s point to `anchor.getOwnerBlock`,
not to `anchor` itself.

**Multiple Move patches on the same anchor + config are concatenated** in patchList order.
If you add several `anchor -> Patch.Move(...)` entries for the same `(anchor, Config.After)`,
all the member lists are merged and inserted together after the anchor (or its redirect target).

**Same-member patches: the patch system MERGES many combinations — prefer ONE bundled
patch list.** Each `db.patch()` call is costly (full ref-table fold + member-list rebuild),
so do not reach for sequential `db.patch()` phases just because two patches share a key.
The patch-table fold in `Patch.scala` merges these same-member combinations:
- `Add(cfg)` + `Add(cfg)` (same config) → concatenated Add
- `Add(Before)` + `Add(After)` (and the reverse) → combined `ReplaceWithMemberN`
- `Add(After)` + `ReplaceWithFirst/Last` combinations → combined Add
- `Add` + `Remove` → `Add(ReplaceWithFirst())`; `Remove` + `Add` → `Add(ReplaceWithLast())`
  (e.g. a `Before`-anchored Add plus a Remove on the same block = "replace the block with
  the added members" — no second phase needed to delete the anchor)
- `Replace` + `Add(After)` (either order) → Add with the replacement inserted
  (note: `Add(InsideLast)` on an owner with members is re-keyed to the owner's very last
  member first, so it usually doesn't even collide with a `Replace` on the owner)
- `Move` + `Move` (same config) → concatenated; `Add` + `Move` (same config) → merged Move
- `Remove` + `Remove` → single Remove

Ref-table effects are applied in **patch-list order** (the raw list, before merging), so a
`Replace(old → new)` placed *before* a MetaDesign `Add` in the list correctly redirects the
added members' references from `old` to `new`.

Combinations NOT in the list above throw
`IllegalArgumentException: Received two different patches for the same member`. Note that
`Patch.Move` internally generates `Patch.Remove` for each listed member. Common triggers:
- Using a `Goto` as a `Move.Before` anchor (so the Move internally removes it on re-insertion)
  while the same `Goto` is also in another Move's members list.
- Moving a parent block with all descendants AND separately moving one of those descendants.

Resort to **multi-phase patching** (sequential `db.patch()` calls) only when the combination
is genuinely unmergeable, or when a later patch's *computation* must observe the already
patched DB (e.g. anchoring a MetaDesign on a member that only exists after the first phase —
though often the merge rules let you anchor on the original member instead).

### Recipe: replace a member AND relocate it, in one patch

Naming (or otherwise rewriting) a member while also moving it looks unmergeable, because
`Patch.Move` emits a `Patch.Remove` per moved member and `Replace + Remove` is not in the merge
table. The naive reading is that you need two phases. You do not, and the merge could not be added
anyway: `patchMembers` inserts the Move's member instances **verbatim** (it never re-resolves them
through the patch table), so merging `Replace(r, FullReplacement) + Remove` would relocate the *old*
instance and drop `r` entirely.

Use `ChangeRefAndRemove` and put the **replacement** in the move list:

```scala
List(
  // drops the original from its old position and redirects every reader to `newMember`
  origMember -> Patch.Replace(newMember, Patch.Replace.Config.ChangeRefAndRemove),
  // inserts the *new* instance at the anchor
  anchor -> Patch.Move(movedMembers, origMember.getOwner, Patch.Move.Config.Before)
)
// where movedMembers is the relocation list with origMember swapped for newMember
```

Why it works: `ChangeRefAndRemove` is rewritten to `(origMember, Patch.Remove())` for the member
list while still redirecting refs, and the `Remove` the Move emits is keyed on `newMember`, which
is not in the walked member list and is therefore inert. The two patches never share a key. The
Move's ownership fixup still finds the right owner because `setName`-style copies share the
original's ref objects.

`NamedAliases` uses exactly this to name a value and lift it out of a conditional expression branch
atomically, which it must, since `SanityCheck` would reject the intermediate DB.

### `Patch.Add` via `MetaDesign`
Use `MetaDesign` when you need to construct new IR members using the DFHDL frontend DSL:

```scala
val dsn = new MetaDesign(
  anchorMember,                                      // where to insert
  Patch.Add.Config.Before                            // or After, InsideFirst, InsideLast
                                                     // or ReplaceWithFirst/ReplaceWithLast(replCfg)
):
  // write DFHDL frontend code here — ports, vars, assignments, etc.
  val newVar = someType.<>(VAR)
  newVar := existingVal.asValAny

dsn.patch   // returns a single (DFMember, Patch) entry for the patch list
```

`ReplaceWithLast` / `ReplaceWithFirst` also replace the anchor with the last/first generated member:
```scala
Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove)
```

See the **MetaDesign Deep Dive** section below for the full mechanics.

---

## DFC — The Implicit Design Context

`DFC` (`dfhdl.core.DFC`) is the implicit context that flows through every frontend DSL operation. It carries the state needed to register new IR members in the right place during elaboration.

```scala
// core/src/main/scala/dfhdl/core/DFC.scala
final case class DFC(
    nameOpt:    Option[String],         // name the next member will receive
    position:   Position,               // source-file position for meta/errors
    docOpt:     Option[String],         // doc-comment
    annotations: List[HWAnnotation],    // active hardware annotations
    mutableDB:  MutableDB,              // the live database being built
    refGen:     ir.RefGen,              // reference-ID generator
    tags:       ir.DFTags,              // member-level tags
    elaborationOptionsContr: () => ElaborationOptions
) extends MetaContext
```

Every `Design` class provides its own `DFC` as a `given`:

```scala
trait HasDFC:
  lazy val dfc: DFC
  protected given DFC = dfc   // makes dfc available implicitly in the design body
```

All DSL operations (declaring ports, calling `:=`, etc.) require `using DFC`. The compiler plugin
injects DFC into the right places automatically — you rarely summon it explicitly.

### Key DFC methods used in stages

```scala
dfc.setMeta(meta)          // copy DFC with updated name/position/doc/annotations
dfc.setName("foo")         // copy DFC with a specific name for the next member
dfc.anonymize              // copy DFC with nameOpt = None  (anonymous member)
dfc.setMeta(m.meta)        // copy DFC mirroring another member's metadata
dfc.enterOwner(owner)      // push a new owner onto the ownership stack
dfc.exitOwner()            // pop the owner stack
dfc.owner                  // current owner (top of stack)
dfc.ownerOrEmptyRef        // ir.DFOwner.Ref for the current owner — use this inside MetaDesign
                           // when constructing raw IR members (avoids `import dfhdl.core.*` ref conflicts)
dfc.inMetaProgramming      // true when inside a MetaDesign
dfc.mutableDB              // access the MutableDB directly
dfc.getSet                 // implicit MemberGetSet backed by mutableDB
```

### Constructing raw IR members inside `MetaDesign`

When you need to build an IR member directly (rather than through the DSL) inside a MetaDesign
body — e.g. creating a `Goto` that references an existing `StepBlock` — use this idiom:

```scala
val dsn = new MetaDesign(anchorMember, Patch.Add.Config.Before):
  import dfhdl.core.*   // brings refTW, addMember, and IR type helpers into scope
  ir.Goto(existingStep.refTW[ir.Goto], dfc.ownerOrEmptyRef, dfc.getMeta, dfc.tags).addMember
```

Two important details:
- `import dfhdl.core.*` (not `import dfhdl.compiler.ir.*`) is needed inside MetaDesign for
  `refTW` and `addMember`. Do **not** write `ir.Goto` with a qualified prefix inside the body
  when `dfhdl.compiler.ir.*` is already imported at the file level — use the unqualified `Goto`
  or the explicit `ir.Goto` form consistently, but be aware that `import dfhdl.core.*` re-exports
  what you need. In practice: inside MetaDesign, use `import dfhdl.core.*` and unqualified names.
- Use `dfc.ownerOrEmptyRef` (a direct method on `DFC`) rather than `dfc.owner.ref`. The latter
  requires extension methods that may conflict with `import dfhdl.core.*`.

### DFCG — the "global" variant

`DFCG` is an opaque subtype of `DFC`. It is auto-synthesised when no explicit `DFC` is in scope
(i.e., in global/top-level Scala scope outside any design body). Operations that only need a name
and position (e.g., `==` for constant comparison) accept `using DFCG` instead of `using DFC`.

---

## MutableDB vs DB

### DB — immutable snapshot

```scala
// compiler/ir/src/main/scala/dfhdl/compiler/ir/DB.scala
final case class DB(
    members:    List[DFMember],
    refTable:   Map[DFRefAny, DFMember],
    globalTags: DFTags,
    srcFiles:   List[SourceFile]
)
```

- A **frozen, serialisable** record of the whole design.
- The `MemberGetSet` derived from it is **read-only** (`isMutable = false`).
- Every stage receives a `DB` and returns a new `DB`; it never mutates the one it received.
- Used by: stage `transform` methods, printers, analysis passes.

### MutableDB — live workspace during elaboration

```scala
// core/src/main/scala/dfhdl/core/MutableDB.scala
final class MutableDB()
```

- A **mutable** collection of `MemberEntry` records, reference tables, and ownership state.
- Lives inside `DFC`; every frontend DSL call that registers a member touches it.
- The `MemberGetSet` derived from it is **read-write** (`isMutable = true`).
- Converted to an immutable `DB` snapshot via `.immutable` (called internally by `design.getDB`).
- Used by: `Design` subclasses during Scala elaboration, `MetaDesign` during stage patching.

| | `DB` | `MutableDB` |
|---|---|---|
| Mutability | Immutable case class | Mutable class |
| Phase | Post-elaboration (compilation) | During elaboration |
| `getSet.isMutable` | `false` | `true` |
| Member changes | None | `addMember`, `plantMember`, `newRefFor` |
| Ownership context | Static | OwnershipContext stack (enter/exit) |
| Serialisable | Yes (upickle) | No |
| Conversion | — | `.immutable` → `DB` |

### Key MutableDB operations (used inside `MetaDesign`)

```scala
mutableDB.addMember(member)                  // register a member under the current owner
mutableDB.plantMember(owner, member)         // register member, forcing a specific owner
mutableDB.newRefFor(ref, member)             // update what an existing reference points to
mutableDB.injectMetaGetSet(getSet)           // allow meta-design members to reference the parent DB
mutableDB.OwnershipContext.enter(owner)      // push owner
mutableDB.OwnershipContext.exit()            // pop owner
mutableDB.immutable                          // snapshot → DB
```

---

## MetaDesign — Deep Dive

`MetaDesign` (`core/src/main/scala/dfhdl/compiler/patching/MetaDesign.scala`) is the bridge
between the frontend DSL and stage patching. It lets you write ordinary DFHDL code (ports, vars,
assignments) inside a stage and have those members injected into the DB at a precise location.

### Signature

```scala
abstract class MetaDesign[+D <: DomainType](
    positionMember: ir.DFMember,   // anchor: where to inject relative to
    addCfg:         Patch.Add.Config,
    domainType:     D = DomainType.DF
)(using
    getSet: ir.MemberGetSet,       // parent DB access (injected automatically by the stage)
    refGen: ir.RefGen              // shared reference generator
) extends Design with reflect.Selectable
```

`MetaDesign` extends `Design`, so you can write any DFHDL DSL code in its body. It is **ephemeral**
— the MetaDesign block itself is never present in the final DB; only the members created inside it
survive.

### How the DFC is set up

```scala
final override protected def __dfc: DFC =
  DFC.emptyNoEO.copy(refGen = refGen, position = positionMember.meta.position)
```

The MetaDesign starts with a minimal DFC that:
- Reuses the **same `refGen`** as the calling stage (ensures generated reference IDs are globally
  unique and don't clash with existing members).
- Copies the **source position** of the anchor member (so synthesised members appear to originate
  at the right place in the source).

After construction, the MetaDesign calls:
```scala
dfc.mutableDB.injectMetaGetSet(getSet)  // lets members inside reference the parent DB
```
This is what makes it possible to reference existing IR members (e.g., `existingVal.asValAny`)
from within a MetaDesign body.

### Where members are injected

```scala
lazy val injectedOwner: ir.DFOwner = addCfg match
  case InsideFirst | InsideLast =>
    positionMember.asInstanceOf[ir.DFOwner]   // positionMember must be a block
  case _ if globalInjection => getSet.designDB.top
  case _                    => positionMember.getOwner   // same owner as anchor
```

For `Before` / `After` / `ReplaceWith*`, members are created inside the **same owner as the anchor
member** (a sibling). For `InsideFirst` / `InsideLast`, members are created **inside** the anchor
(the anchor must be a block/design).

### The `.patch` property

```scala
lazy val patch = positionMember -> Patch.Add(this, addCfg)
```

This is a `(DFMember, Patch)` pair ready to be added to a stage's patch list. When the DB applies
this patch it:
1. Extracts the members created inside the MetaDesign.
2. Removes the MetaDesign block itself.
3. Inserts the members at the correct position relative to `positionMember` according to `addCfg`.

### Planting existing IR members

Sometimes you want to move or clone members that **already exist in the DB** into the MetaDesign
context:

```scala
// Inject a single existing IR member, forcing it under the current owner
plantMember(existingIRMember)

// Inject a set of members, re-owning those that belonged to baseOwner
plantMembers(baseOwner, memberIterable)

// Deep-clone a list of members (new refs), re-mapping internal references
val cloneMap = plantClonedMembers(baseOwner, memberList)
// cloneMap: Map[original -> clone] for further reference remapping
```

### Temporarily switching owner during construction

```scala
applyBlock(someOwner):
  // members created here belong to someOwner, not the current owner
  val v = SomeType <> VAR
```

### Converting IR members to core types inside MetaDesign

Because MetaDesign extends Design, the following exports are always available inside its body:

```scala
existingIRVal.asValAny        // ir.DFVal  → core DFValAny  (wrap without adding to DB)
existingCoreVal.asIR          // core DFVal → ir.DFVal       (unwrap)
plantMember(irMember)         // add an existing IR member to this MetaDesign's DB
```

---

## IR Layer vs Core Layer

DFHDL uses a strict two-layer architecture:

| | IR layer (`dfhdl.compiler.ir`) | Core layer (`dfhdl.core`) |
|---|---|---|
| Purpose | Immutable AST, serialisable | Live DSL objects during elaboration |
| DFVal | `ir.DFVal` — sealed trait + case-class subtypes | `core.DFVal[T, M]` — opaque wrapper around `ir.DFVal` |
| DFType | `ir.DFType` — sealed ADT (DFBits, DFStruct, …) | Extension methods on `ir.DFType` via `.asFE[T]` |
| Design block | `ir.DFDesignBlock` — immutable case class | `core.Design` — live abstract class with DFC |
| Mutability | Immutable; updates return new instances (`.copy`) | Mutable via `MutableDB` |
| Lifetime | Persists in `DB` across all stages | Ephemeral; discarded after `design.getDB` |
| Serialisable | Yes | No |

### `ir.DFVal` — the IR representation

```scala
// compiler/ir — sealed trait with case-class subtypes
sealed trait DFVal extends DFMember.Named:
  val dfType: DFType
// Subtypes:
//  DFVal.Const        — literal constant with data
//  DFVal.Dcl          — port / var / const declaration
//  DFVal.Func         — computed expression / operator
//  DFVal.Alias.AsIs   — type cast (.as(T))
//  DFVal.Alias.ApplyIdx    — vector indexing
//  DFVal.Alias.SelectField — struct field selection
//  DFVal.Alias.History     — .prev(n)
//  DFVal.Alias.ApplyRange  — bit-range slice
//  DFVal.DesignParam  — design parameter reference
//  DFVal.Special      — NOTHING, OPEN, CLK_FREQ
```

Every IR member is a **pure data record** — a case class with no behaviour beyond `.copy()`.
When a stage needs to change a member, it creates a modified copy and patches it in.

### `core.DFVal[T, M]` — the core (frontend) representation

```scala
// core/src/main/scala/dfhdl/core/DFVal.scala
into final class DFVal[+T <: DFTypeAny, +M <: ModifierAny](val irValue: ir.DFVal | DFError)
    extends DFMember[ir.DFVal] with Selectable
```

- An **opaque value class** — zero runtime overhead, just wraps `ir.DFVal`.
- `T` is the DFHDL type (e.g., `DFBits[8]`); `M` is the modifier (IN, OUT, VAR, CONST, …).
- All DSL operations (arithmetic, assignments, `:=`, `<>`) are extension methods on `DFVal`.
- Requires `using DFC` to register the resulting IR members in the `MutableDB`.
- The `| DFError` union allows deferred error reporting without exceptions.

### Key conversion methods

```scala
// IR → Core (wrapping)
irDFVal.asValAny                      // ir.DFVal → core.DFValAny
irDFVal.asVal[MyDFType, MyModifier]   // ir.DFVal → typed core.DFVal[T, M]
irDFVal.asFE[MyCoreDFType]            // general ir member → core frontend type

// Core → IR (unwrapping)
coreDFVal.asIR                        // core.DFVal[T,M] → ir.DFVal  (throws on DFError)

// Registering in DB
irMember.addMember                    // (using DFC) adds IR member to the current MutableDB
```

### Why the split matters for stage authors

Stage `transform` methods work **entirely in the IR layer**: they receive `ir.DB`, pattern-match
on `ir.DFMember` subtypes, and return a patched `ir.DB`. No `DFC` is required.

`MetaDesign` is the **only place** where you cross from IR into Core: inside a MetaDesign body you
write Core DSL code (which needs `DFC`), and `.patch` converts the result back to a
`(ir.DFMember, Patch)` pair that the stage can use.

The conversion idiom you will see most often in stage code:
```scala
// cross from IR into core to reference an existing member inside a MetaDesign
existingIRVal.asValAny     // wrap for DSL use — does NOT add to DB
plantMember(existingIRVal) // wrap AND register under the MetaDesign owner

// cross from core back to IR to build a Patch
dsn.plantedNewVar.asIR     // get the IR DFVal that was created inside the MetaDesign
```

---

## Transformation Patterns

> **IR member hierarchy, field definitions, and pattern-match extractors → see `/ir-reference`.**

### Pattern 1 — Simple member replacement (most common)
Collect patch entries for every matching member, then apply:

```scala
def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
  val patchList: List[(DFMember, Patch)] =
    designDB.members.view.flatMap {
      case m @ SomePattern(...) =>
        Some(m -> Patch.Replace(m.copy(...), Patch.Replace.Config.FullReplacement))
      case _ => None
    }.toList
  designDB.patch(patchList)
```

### Pattern 2 — Move members (e.g. hoisting declarations)
```scala
designDB.members.view
  .collect { case m @ DclVar() => m }
  .flatMap { dcl =>
    dcl.getOwnerBlock match
      case cb: DFConditional.Block =>
        Some(cb.getTopConditionalHeader -> Patch.Move(dcl, Patch.Move.Config.Before))
      case _ => None
  }.toList
```

### Pattern 3 — Construct new members with `MetaDesign`
```scala
designDB.members.view.flatMap {
  case target @ MatchPattern() =>
    val dsn = new MetaDesign(
      target,
      Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove)
    ):
      val reg = someType.<>(VAR.REG)
      reg.din := target.asValAny
    Some(dsn.patch)
  case _ => None
}.toList
```

### Pattern 4 — Type replacement (for opaque/struct type changes)
```scala
object MyDFTypeReplacement extends ComposedDFTypeReplacement(
  preCheck  = { case dt: DFOpaque if pred(dt) => Some(()); case _ => None },
  updateFunc = { case (dt: DFOpaque, _) => dt.actualType }
)
// Then match on values and call dfVal.updateDFType(newType)
```

### Pattern 5 — Backend-conditional logic
```scala
def transform(designDB: DB)(using MemberGetSet, co: CompilerOptions): DB =
  val isVHDL = co.backend.isVHDL
  val isVerilogOld = co.backend match
    case be: dfhdl.backends.verilog =>
      be.dialect match
        case VerilogDialect.v95 | VerilogDialect.v2001 => true
        case _                                         => false
    case _ => false
  ...
```

### Pattern 6 — Recursive application (until stable)
Use when removing one member may expose additional members to remove:

```scala
def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
  val patchList = designDB.members.flatMap { ... }
  if (patchList.isEmpty) designDB
  else
    val newDB = designDB.patch(patchList)
    transform(newDB)(using newDB.getSet)
```

### Pattern 7 — Multi-phase transformation
Apply phase 1, create a new `MemberGetSet`, then apply phase 2:

```scala
val phase1DB = designDB.patch(phase1Patches)
locally {
  given MemberGetSet = phase1DB.getSet
  val phase2Patches = phase1DB.members.flatMap { ... }
  phase1DB.patch(phase2Patches)
}
```

### Pattern 8 — `runCondition` for conditional stages
```scala
override def runCondition(using co: CompilerOptions): Boolean =
  co.dropUserOpaques || co.backend match
    case be: dfhdl.backends.verilog => be.dialect == VerilogDialect.v95
    case _                          => false
```

### Pattern 9 — One-level-at-a-time repeated patching (`@tailrec`)

Use when a transformation must be applied iteratively, processing one nesting level per pass
(e.g. flattening a hierarchy depth-by-depth). This avoids duplicate entries that would arise
if you tried to simultaneously move a parent block (with its descendant as part of the moved
list) AND separately move that same descendant.

```scala
import scala.annotation.tailrec

@tailrec private def flattenRepeatedly(db: DB)(using RefGen): DB =
  given MemberGetSet = db.getSet
  val patches = db.members.view.flatMap {
    case pb: ProcessBlock if pb.isInRTDomain => collectOneLevelPatches(pb)
    case _                                    => Nil
  }.toList
  if patches.isEmpty then db
  else flattenRepeatedly(db.patch(patches))

private def collectOneLevelPatches(pb: ProcessBlock)(using MemberGetSet): List[(DFMember, Patch)] =
  // Only look at direct children of pb-level steps; lift each one level up.
  // After one pass, previously-nested blocks are now direct pb children,
  // and their children become the candidates for the next pass.
  pb.members(MemberView.Folded).flatMap {
    case parent: StepBlock if parent.isRegular =>
      parent.members(MemberView.Folded).flatMap {
        case child: StepBlock if child.isRegular =>
          val allMembersToMove = child :: child.members(MemberView.Flattened)
          List(parent -> Patch.Move(allMembersToMove, child.getOwner, Patch.Move.Config.After))
        case _ => Nil
      }
    case _ => Nil
  }
```

Key invariants:
- Include ALL descendants in `allMembersToMove` to preserve the flat-list ownership ordering.
- Process only **direct children** per pass — do not recurse into grandchildren in the same pass.
- The `@tailrec` loop terminates when `collectOneLevelPatches` returns an empty list (all blocks
  are already direct children of the `ProcessBlock`).
- `given MemberGetSet = db.getSet` must be re-established at the top of each recursive call so
  navigation helpers see the updated member structure.

**This pattern is not just for flattening — it is the general cure for "nested same-kind
rewrites".** Whenever a stage rewrites a construct via `Patch.Move` or
`ReplaceWithLast(ChangeRefAndRemove)` *plus* a satellite patch anchored on its body (an `After`
increment, a `Before` goto, a consumed-member `Remove`), and that construct can **nest inside
another of the same kind**, rewriting both in one pass conflicts: the inner one appears both inside
the outer one's moved descendants (`Flattened`) AND in its own patches → ownership-order breakage
or `Received two different patches for the same member`. Two real instances from this codebase:
- `SimplifyRTOps` rewriting nested `for` loops (`DFForBlock` → `while` + iterator + `After`
  increment).
- `FlattenStepBlocks` extracting `StepBlock`s nested in conditional branches (`Move` + inserted
  goto + consumed-goto `Remove`).

The fix in both was identical: drive the rewrite with `@tailrec ...Repeatedly(db)` and **gate each
pass to the non-nested instances** so an outer and inner are never patched together — e.g. process
only the *innermost* (no transformable descendant) or only the *outermost* (no transformable
ancestor), and let later passes pick up the rest:
```scala
// innermost-first gate (SimplifyRTOps): skip a for loop that contains another transformable one
case fb: DFLoop.DFForBlock
    if isTransformable(fb) && !fb.members(MemberView.Flattened).exists {
      case inner: DFLoop.DFForBlock => isTransformable(inner); case _ => false
    } =>

// outermost-first gate (FlattenStepBlocks): skip a step that has a transformable step ancestor
val targets = pb.members(MemberView.Flattened).collect {
  case sb: StepBlock if isCondBranchStep(sb) && !hasCondBranchStepAncestor(sb) => sb
}
```
Each pass strictly reduces the count of nested instances, so the `@tailrec` loop converges. This
keeps the multi-phase structure intact — a stage with several phases can wrap just the affected
phase(s) in their own `...Repeatedly` driver (as `FlattenStepBlocks` does for conditional
extraction and structural flattening independently).

### Pattern 10 — Replace a DFOwner while preserving its children

Use when you want to swap one owner block for another (e.g. `DFForBlock` → `DFWhileBlock`) but
keep all body members in place. `ReplaceWithLast(ChangeRefAndRemove)` redirects **all** refs to
the old owner — including every child member's `ownerRef` — to the new owner (the last member
synthesised in the MetaDesign). Body members naturally remain in the flat list at their correct
positions, now re-parented to the new block. No `plantMembers` is needed.

```scala
case forBlock: DFLoop.DFForBlock if forBlock.isInRTDomain =>
  val m1 = new MetaDesign(
    forBlock,
    Patch.Add.Config.ReplaceWithLast(Patch.Replace.Config.ChangeRefAndRemove),
    dfhdl.core.DomainType.RT
  ):
    // preamble members (e.g. iterator VAR.REG, guard expression) ...
    val whileBlock = dfhdl.core.DFWhile.Block(guard)(using dfc.setMeta(forBlock.meta))
    dfc.enterOwner(whileBlock)   // required so whileBlock is a proper owner in the DB
    dfc.exitOwner()
  // capture M1 members for use in further patches
  val newIterDclIR = m1.newIterDcl.asIR
  List(m1.patch, ...)
```

The `dfc.enterOwner` / `dfc.exitOwner` pair is required for the new owner (here `whileBlock`) to
be registered in the DB's ownership context so that child member navigation works correctly.

### Pattern 11 — Two-MetaDesign pattern: append members at end of a re-owned body

Use when you need to inject members **after the last body member** of a block that is being
replaced by Pattern 10. Anchor M2 at `bodyMembers.last` with `After`. M2's `injectedOwner` is
`bodyMembers.last.getOwner = oldBlock`. When M1's `ChangeRefAndRemove` redirects refs to the old
block → new block, M2's members' `ownerRef`s are also redirected, placing them correctly as the
last statements inside the new block.

```scala
val forBodyMembers = forBlock.members(MemberView.Folded)
// M1: replace forBlock with whileBlock (Pattern 10) ...
if forBodyMembers.nonEmpty then
  val m2 = new MetaDesign(
    forBodyMembers.last,
    Patch.Add.Config.After,
    dfhdl.core.DomainType.RT
  ):
    // members here are owned by forBodyMembers.last.getOwner = forBlock
    // after M1's ChangeRefAndRemove, their ownerRefs are redirected to whileBlock
    m1.newIterDcl.din := m1.newIterDcl + stepConst   // cross-MetaDesign ref is fine
  List(m1.patch, iterDclPatch, m2.patch)
else List(m1.patch, iterDclPatch)
```

**Cross-MetaDesign references**: Members declared with `val` in M1 body are accessible as
`m1.memberName` (MetaDesign extends `reflect.Selectable`). They can be used inside M2's body
via closure; the symbolic refs resolve correctly in the final DB because M1's members are present
after all patches are applied.

**Returning multiple patches per case**: use `.flatMap { ... }` (not `.collect { ... }.flatten`)
and return `List(...)` from the case; return `None` from the catch-all `case _ =>`. Both `Option`
and `List` are `IterableOnce`, so `flatMap` handles them uniformly.

---

## Boilerplate Template

### Stage file: `compiler/stages/src/main/scala/dfhdl/compiler/stages/MyStage.scala`

### Stage ScalaDoc guidelines

- Wrap the ScalaDoc comment with `//format: off` / `//format: on` (scalafmt reformats `{{{ }}}` blocks and `==Headings==` if left unguarded).
- Use `==Rule N: Title==` ScalaDoc sections for each distinct transformation rule.
- Use `{{{ }}}` code blocks (not ` ```scala ` fences) for before/after examples.
- Each code block should start with `// Before` and `// After` comment lines.
- Before/after examples should show the construct in context (e.g. with a preceding statement), not as the first and only statement in a process — this makes the placement of generated members unambiguous.
- Do NOT include Idempotency or Determinism sections — those are implementation invariants, not user-facing documentation.

```scala
package dfhdl.compiler.stages

import dfhdl.compiler.analysis.*
import dfhdl.compiler.ir.*
import dfhdl.compiler.patching.*
import dfhdl.options.CompilerOptions

//format: off
/** Brief description of what this stage does and which IR shapes it transforms.
  *
  * ==Rule 1: Title of first transformation==
  *
  * Prose explanation of the rule.
  * {{{
  * // Before
  * <input IR shape>
  *
  * // After
  * <output IR shape>
  * }}}
  *
  * ==Rule 2: Title of second transformation==
  * ...
  */
//format: on
case object MyStage extends Stage:
  def dependencies: List[Stage] = List(SomePriorStage)   // run these first
  def nullifies: Set[Stage]     = Set(SomeInvalidatedStage)

  def transform(designDB: DB)(using MemberGetSet, CompilerOptions): DB =
    val patchList: List[(DFMember, Patch)] =
      designDB.members.view.flatMap {
        case m @ TargetPattern(...) =>
          Some(m -> Patch.Replace(m.copy(/* changes */), Patch.Replace.Config.FullReplacement))
        case _ => None
      }.toList
    designDB.patch(patchList)
end MyStage

// Convenience extension — follows the naming convention of all other stages
extension [T: HasDB](t: T)
  def myStage(using CompilerOptions): DB =
    StageRunner.run(MyStage)(t.db)
```

---

## Debugging a Stage Failure with TRACE

When a full compilation blows up inside the stage pipeline (a `SanityCheck` failure, a patch
conflict, etc.), the fastest way to localize and reproduce it is the **TRACE log**, which prints
the full design code string after every stage that changes the DB.

### Enabling and running

Put a design in `lib/src/test/scala/Playground.scala` (or `core/.../Playground.scala`) and run the
whole pipeline for a top-level design named `Foo` via the `main` injected into its companion
object. The log level is a plain command-line option, so **no source edit is needed**:
```bash
sbtn.bat 'lib/Test/runMain Foo --nocache --log trace compile --print-backend'
```
(core equivalent: `corePlayground` + `core/Test/runMain`.)

The command line is `[design-args] [app-options] <mode> [mode-options]`, a plain subcommand
structure, so **placement around the mode is load-bearing**:

| Piece | Side | Examples |
|---|---|---|
| Design args + app options | *before* the mode | `--width 12`, `--nocache`, `--log trace` |
| Mode | — | `compile`, `commit`, `simulate` |
| Mode options | *after* the mode | `--print-backend`, `-b vhdl.v2008`, `-t questa` |

An option on the wrong side fails with `[scallop] Error: Unknown option 'backend'` — note that
`--backend`/`-b` is a **mode** option, so it goes after `compile`, not before it. The full
reference is `docs/user-guide/command-line/index.md`.

Useful combinations:
```bash
# see the generated HDL without leaving the trace run
sbtn.bat 'lib/Test/runMain Foo --nocache --log trace compile --print-backend'
# compare backends (illegal output is often backend-specific)
sbtn.bat 'lib/Test/runMain Foo --nocache compile --backend vhdl.v2008 --print-backend'
sbtn.bat 'lib/Test/runMain Foo --nocache -- simulate -t questa'
```
The older form, a `given options.CompilerOptions.LogLevel = _.TRACE` at the top of the Playground
file, still works and is what you need when the run is driven by a spec rather than `runMain`.

A design with **no ports + a `finish()`** is treated as a self-contained simulation top, so the
default (no-mode) action becomes *simulate* instead of *compile*.

### Reading the trace to localize the failing stage

The log emits `Running stage X....` / `Finished stage X` around each stage, runs a `SanityCheck`
after every non-`NoCheckStage`, and prints the code string after any stage that changed the DB. So:
- The **`SanityCheck` that throws** (`Failed ownership check!`, `Received two different patches for
  the same member`, …) fires *immediately after* the offending stage — that stage name is your
  culprit, even if the symptom (a dangling owner, a duplicate Goto) looks structural.
- **Don't assume the failing stage is the one you changed.** A stage can pass its own sanity check
  and emit a valid DB, yet a *later* stage chokes on a shape your stage newly produced. Read the
  `Running stage` sequence to find the first failure, not the first suspect.

### Localizing *illegal generated HDL* (nothing throws)

When a run succeeds end-to-end but emits HDL the downstream tool rejects, no check fires and the
trace has to be read differently. That diagnosis workflow, along with how to attribute the shape
to the stage that *introduced* it, lives in [/bugfix](bugfix.md).

### Harvesting a self-contained reproducer from the trace

The code printout emitted **immediately before** the failing stage is that stage's *input* — and
crucially it is a **valid** design (it just passed the previous stage's sanity check). Because
stages run before later lowering, the printed constructs are exactly the failing stage's input
form, so you can drop that printout almost verbatim into a self-contained `<Stage>Spec` test (see
*Test Authoring Rules* below) as the reproducer. This turns an opaque end-to-end crash into a fast,
isolated unit test in one step — write the test, watch it fail with the same error, then fix.

### Caveats

- Re-running an unchanged design may short-circuit on the on-disk cache
  (`Loading committed design from cache...`) and skip the stages (and the trace) entirely. Pass
  **`--nocache`** to disable caching — it is an app option, so it goes *before* the mode.
  (Alternatively clear `sandbox/<Top>` via `sbtn clearSandbox`, or edit the design — but `--nocache`
  is the lightweight option for repeated trace runs.)
- **`--nocache` covers both caches**: the app's step cache under `sandbox/<Top>/cache` *and* the
  sub-design elaboration cache, which lives in `*/target/scala-*/dfhdl-cache/` and would otherwise
  keep adopting cached sub-design bodies instead of elaborating them. `clearSandbox` does NOT reach
  the second one; `sbtn clearElabCache` clears it, and `sbtn clearDFHDL` clears both. You rarely
  need either, since cache entries are keyed by code digest and a stale one can never be served.
- `libPlayground` / `corePlayground` zero out other subprojects' test sources for the session. To
  run `StagesSpec` tests again afterwards, reset with a leading `;reload`.

---

## Stage-Facing Invariants Discovered by Bug Fixes

Deciding **where a rule belongs** (elaboration `DB.check` vs `SanityCheck`), how to scope it, and
how to measure its blast radius before fixing stages is bug-fixing methodology and lives in
[/bugfix](bugfix.md). What follows is only the part a stage author must obey.

### What `SanityCheck` enforces after your stage

`SanityCheck.transformSubDB` runs, per sub-DB: `refCheck`, `memberExistenceCheck`,
`ownershipCheck`, `orderCheck`, `hdlMethodCheck`, and the whole of `DB.subDBCheck` — the same
per-design set elaboration runs (`nameCheck`, the connectivity checks behind `connectionTable`,
`directRefCheck`, `initialCheck`, `condExprNamedValCheck`). So a stage's output has to satisfy
every rule a *user design* satisfies, not just the structural ones.

`orderCheck` is the one most likely to be new to you: **every member may reference only members
defined above it in the flat list** (a `Goto` is the sole exception, since it targets a later
step). It bites whenever a stage relocates a member without its dependency cone, or emits a
substitute after the value that reads it (mistake 27).

### Named values in conditional expression branches

A `DFConditional.Header` with `dfType != DFUnit` is a conditional *expression*: each branch
contributes only its resulting value. Whether a branch of one may *also* hold a named value depends
on the scope, and the boundary is not the one you would guess:

| Conditional expression sits in | Named value in a branch | Why |
|---|---|---|
| ED domain body (**concurrent**) | **illegal** | branch is not a block; the drive becomes a connection |
| ED `process` | legal | branch lowers to a procedural block |
| RT / DF domain | legal | same |
| conditional *statement* branch (`dfType == DFUnit`) | legal | branch is a scope in its own right |

`ExplicitNamedVars` chooses connection vs assignment with `named.isInEDDomain && !named.isInProcess`.
**Only the concurrent combination emits a connection**, and a connection is the only drive with
nowhere to live once `ExplicitCondExprAssign` wraps the expression into a `process(all)` — at which
point it prints as `assign` inside an `always_comb` (issue #426). `DB.condExprNamedValCheck()`
enforces the same predicate, and the comments on both say they must stay in agreement.

A named **conditional header** is exempt: `ExplicitNamedVars` drives it through `patchChains`
(`case _: DFConditional.Header => // do nothing`, then an assignment per branch), so it never
becomes a connection.

Consequences when writing a stage:
- **Never name an intermediate at its point of use when that point is a conditional expression
  branch in an ED domain body.** Name *and* relocate in the same patch — see the
  name-and-relocate-in-one-patch recipe under *Patch System*. A follow-up cleanup stage is not an
  acceptable substitute: `SanityCheck` runs after every stage, so the intermediate DB would be
  illegal. `NamedAliases` does both in one patch for this reason.
- **`isInProcess` walks owners until a `ProcessBlock` or a `DFDomainOwner`, so conditional blocks
  are transparent to it.** If you need "is this member in a scope that stays concurrent?", that is
  the wrong test.
- **`collectRelMembers` recurses only into *anonymous* values.** Mind *when* you call it. Before
  your patch is applied, a value a sibling group in the same pass is about to name is still
  anonymous, so it **is** in the cone — which is what lets one group carry another's sub-tree, and
  exactly why overlapping groups must be gated (next bullet). After the naming patch, the same
  call stops at that value, so a guard written against the post-patch cone silently passes; check
  `getRefs` directly there.
- **Two groups can want to relocate the same sub-tree.** `(if (d) p else q) + 1` produces two
  naming groups whose move sets overlap, and moving the shared members twice yields
  `More than one appearance of member in member list`. Gate each pass to the innermost group (drop
  any group whose own value another group would carry, leaving it anonymous) and drive the stage
  `@tailrec`; the outer group is picked up next pass, once what it reads sits outside the branch.

---

## Test Authoring Rules

**Every stage you touch gets a test in its own `<Stage>Spec`.** One change, one stage, one spec
test — and if a fix spans three stages, all three get one. Covering the whole fix with a single
test on the stage that happened to be easiest, or with an end-to-end backend test, leaves the other
stages' behaviour unpinned: the next person to edit one of them gets no signal, and a failure lands
on whichever stage is later in the pipeline rather than the one that broke.

A stage with no spec file yet is not an exemption. Add the `extension [T: HasDB](t: T) def
<stageName>` entry point next to its siblings in the stage's own source file (several stages in a
shared file have one and the rest do not, purely by accident), and create
`StagesSpec/<Stage>Spec.scala`. That entry point is also what the test's `import` refers to, so
reverting the stage file wholesale to prove the test fails will break the spec's compilation
instead — revert just the changed guard in place. See
[/bugfix](bugfix.md) "Prove the test fails without the fix".

When a stage's change genuinely cannot be seen in its own printout (two different IRs printing
identically — the printout is the stage contract precisely because it hides representation), say so
in a comment at the backend test that does pin it, and do not leave behind a stage test that passes
either way.

**Tests must be self-contained.** Each test should only exercise the stage under test. Do not write input designs that rely on a prior stage to produce the IR shape that the current stage expects — write that IR shape directly using the DFHDL DSL.

For example, a stage that operates on `StepBlock`s should use explicit `def MyStep: Step = …` syntax in the test design rather than `1.cy.wait` or `while (…)` constructs, because those are transformed into `StepBlock`s by `DropRTWaits`, not by the stage under test. Letting `DropRTWaits` silently run as a dependency makes a failing test ambiguous: it is unclear whether the bug is in the stage under test or in the dependency.

The same principle applies to any other prior-stage construct: if `DropFoo` normally feeds into `AddBar`, the `AddBarSpec` tests should express the output of `DropFoo` directly, without triggering `DropFoo`.

**Line-number-sensitive expected strings.** `PrintVHDLCodeSpec` and `PrintVerilogCodeSpec` contain tests (e.g. "text out printing") whose expected output embeds hardcoded source positions of the spec file itself (a `debug(...)` call prints `PrintXCodeSpec.scala:<line>:<col>`). Inserting or removing lines *anywhere above* those tests shifts the positions and fails them with an unrelated-looking diff. After editing these files, grep for `CodeSpec.scala:[0-9]` and realign the embedded line numbers with the actual call sites.

## Test File Template

### Test file: `compiler/stages/src/test/scala/StagesSpec/MyStageSpec.scala`

```scala
package StagesSpec

import dfhdl.*
import dfhdl.compiler.stages.myStage
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}

class MyStageSpec extends StageSpec:

  test("basic transformation") {
    class Top extends EDDesign:           // or RTDesign / DFDesign
      val x = UInt(8) <> IN
      val y = UInt(8) <> OUT
      process(all):
        y :== x
    val result = (new Top).myStage
    assertCodeString(
      result,
      """|class Top extends EDDesign:
         |  val x = UInt(8) <> IN
         |  val y = UInt(8) <> OUT
         |  process(all):
         |    y :== x
         |end Top
         |""".stripMargin
    )
  }

  test("backend-specific behaviour under VHDL") {
    given options.CompilerOptions.Backend = _.vhdl.v93
    class Top extends EDDesign:
      ...
    val result = (new Top).myStage
    assertCodeString(result, "...")
  }

end MyStageSpec
```

### `StageSpec` reference
```scala
// compiler/stages/src/test/scala/StageSpec.scala
abstract class StageSpec(stageCreatesUnrefAnons: Boolean = false)
    extends FunSuite, NoTopAnnotIsRequired:
  inline def assertCodeString(db: DB, cs: String): Unit =
    import db.getSet
    if (stageCreatesUnrefAnons) db.dropUnreferencedAnons.sanityCheck
    else db.sanityCheck
    assertNoDiff(DefaultPrinter.csDB, cs)
```

- Pass `stageCreatesUnrefAnons = true` when your stage intentionally leaves unreferenced anonymous members (e.g. it extends `NoCheckStage`). `assertCodeString` will then call `dropUnreferencedAnons` automatically before checking.
- `assertNoDiff` from munit gives a clear character-level diff on mismatch.
- The expected code string must reproduce DFHDL source syntax exactly (including `end DesignName` terminators, blank lines between designs, and pipe-aligned fields when using the scalafmt alignment hint at the top of the file).

---

## Common Mistakes to Avoid

1. **Writing `end process` in code examples** — Scala does not support `end` markers on method
   calls, so `process` blocks have no closing `end process`. Only named definitions (`def`, `class`,
   `object`, `val`, etc.) accept end markers. StepBlock prints as `def Name: Step = … end Name`;
   ProcessBlock prints as `process(sensitivity):\n  body` with no end marker.
2. **Iterating over `Set` or `Map` to build the patch list** — order is undefined, producing non-deterministic output. Always iterate `designDB.members` (a `List`) to drive patch collection.
3. **Overly broad pattern matches** — if a match can fire on already-transformed IR, the stage is not idempotent (`f(f(x)) ≠ f(x)`). Match on the exact source shape so that the output IR no longer satisfies the predicate.
4. **Forgetting `end transform` / `end MyStage`** — scalafmt's `insertEndMarkerMinLines = 15` rule requires end markers on blocks ≥ 15 lines.
5. **Missing `given RefGen = RefGen.fromGetSet`** inside `transform` when using `MetaDesign` with fresh reference generation.
6. **Mutating `getSet`** — always create a new `MemberGetSet` via `newDB.getSet` after patching before iterating again.
7. **Patch order matters** — patches are applied in list order; a `Move` before a `Replace` on the same member may conflict.
8. **Not extending `NoCheckStage`** when your stage produces unreferenced anonymous members as intermediate output — the automatic sanity check will fail.
9. **Nullifying too aggressively** — only nullify a stage if your transformation invalidates its invariant. Unnecessary nullification forces redundant re-runs.
10. **Confusing `ChangeRefOnly` vs `FullReplacement`** — `ChangeRefOnly` keeps the old member in the member list (useful when another stage still expects it); `FullReplacement` swaps it in place.
11. **Moving a `DFOwner` without its descendants** — `Patch.Move` only moves the listed members.
    Moving a block but leaving its children at their original positions violates the ownership
    invariant (pre-order DFS ordering). Always include `block :: block.members(MemberView.Flattened)`
    in the moved-members list. If you need to move multiple nesting levels, use the one-level-at-a-time
    `@tailrec` pattern (Pattern 9) to avoid duplicating descendants across multiple Move entries.
12. **Conflicting patches on the same member** — `Patch.Move` internally generates `Patch.Remove`
    for each moved member. If that same member also appears in an *unmergeable* patch in the same
    list, you get `IllegalArgumentException: Received two different patches for the same member`.
    The most common cause is using a `Goto` as a `Move.Before` anchor while that same `Goto` also
    appears in another Move's members list. Before splitting into sequential `db.patch()` calls
    (multi-phase — each call is costly), check the same-member merge table in the Patch System
    section: many combinations (Add+Remove, Replace+Add, Add+Add, …) merge fine in one list.
13. **Using `dfc.owner.ref` inside MetaDesign** — `import dfhdl.core.*` introduces extension
    methods that conflict with `.ref`. Use `dfc.ownerOrEmptyRef` instead to obtain the owner's
    `ir.DFOwner.Ref` when constructing raw IR members.
14. **`DFBlock` subtypes lack `isAnonymous` / `getName`** — these helpers are extension methods on
    `DFMember.Named` (value types), not on blocks. For `DFForBlock`, `DFWhileBlock`, and similar,
    access the name via `block.meta.nameOpt` directly:
    ```scala
    val iterName = forBlock.meta.nameOpt match
      case None       => iteratorDcl.getName
      case Some(name) => s"${name}_${iteratorDcl.getName}"
    ```
15. **Case-class extractor `DFForBlock()` requires all fields** — writing `case x @ DFLoop.DFForBlock()`
    fails with "wrong number of argument patterns" because `DFForBlock` has multiple fields. Use a
    type pattern instead: `case x: DFLoop.DFForBlock`. The same applies to other multi-field IR
    case classes that have no dedicated single-argument unapply.
16. **Rewriting nested same-kind constructs in one pass** — if your stage rewrites a construct that
    can nest inside another of the same kind (nested `for` loops, steps-in-conditionals nested in
    steps, etc.) via a `Move` / `ReplaceWithLast(ChangeRefAndRemove)` plus a body-anchored satellite
    patch, doing the outer and inner in one patch list conflicts: the inner appears both in the
    outer's `Flattened` moved descendants and in its own patches → ownership breakage or
    `Received two different patches for the same member`. Drive it `@tailrec` and gate each pass to
    the innermost-only or outermost-only instances (see Pattern 9). This is easy to miss because a
    single-level test passes — **always add a nested test** for any such rewrite.
17. **Resolving fresh clone refs inside MetaDesign** — refs created by `copyWithNewRefs` /
    `cloneAnonValueAndDepsHere` are registered only in the MetaDesign's own `MutableDB`. Calling
    `ref.get` on them with the stage's outer `MemberGetSet` misses (or worse, resolves stale).
    Inside code that traverses freshly cloned members, bind `given MemberGetSet = dfc.getSet` —
    the DFC's getSet chains to the parent DB via `injectMetaGetSet`, so it resolves both new and
    original members.
18. **`plantMembers` re-owning is lost when the old owner is Replaced in the same list** —
    `plantMembers(oldOwner, members)` re-owns by registering the members' ownerRefs to the new
    owner in the Add's ref table, but `Patch.Replace`'s `replaceMember` collects refs from the
    reverse `memberTable` index, which Add patches do NOT update. A same-list
    `Replace(oldOwner → x, ChangeRefAndRemove/FullReplacement)` therefore re-redirects the
    "moved" members' ownerRefs to `x`, undoing the re-own. Fix: use `plantClonedMembers` +
    `Patch.Remove()` of the originals (fresh clone refs are invisible to the Replace), or split
    into two patch phases. Relatedly, place `Replace` patches BEFORE a MetaDesign `Add` in the
    patch list when the Add's members reference the replaced instance — ref-table effects apply
    in list order, so the Add's references then resolve to the replacement.
19. **Name shadowing inside MetaDesign bodies** — `MetaDesign` extends `Design`, whose
    `export dfhdl.hdl.*` brings frontend names (`DFVal`, `StepBlock`, …) into the *class* scope,
    shadowing the file-level `import dfhdl.compiler.ir.*` wildcard for overlapping names. Inside
    a MetaDesign body, add `import dfhdl.compiler.ir` at the file top and qualify IR types as
    `ir.DFVal`, `ir.StepBlock`, etc., importing only the core names actually needed (e.g.
    `import dfhdl.core.{DFIf, DFBool, DFUnit, refTW, addMember}`).
20. **Design-block ownership is structural in flat DBs** — calling `getOwnerDesign` (or any
    owner navigation) on a `DFDesignBlock` throws `No owner found` in a `newToOld` flat DB: a
    design's `ownerRef` is the hierarchy key (`StaticRef`) and is deliberately NOT in the
    refTable. To find a design's owning design, build a map from `designMemberList` by
    collecting child `DFDesignBlock` members per design (see `UniqueDesigns.designOwnerMap`).
21. **Every read of a def-design (ED method) output creates its own `PortByNameSelect`** —
    when redirecting readers of a call result, collect ALL `designInstPBNS(inst).filter(_.isOut)`
    members, not just the first: each read site references a different PBNS member (see
    `PrepEDDefs` — the main PBNS kept via `ChangeRefOnly` + `OfMembers`, the rest
    `ChangeRefAndRemove`d).
22. **An anonymous value may be read exactly once** — `SanityCheck` reports *"An anonymous value has
    more than one reference"*. So when a stage synthesizes a *wrapper* per read (a `.din` read, a
    cast, a sampling alias), do NOT memoize one wrapper per target and share it between two readers:
    emit a fresh one per reading reference. Memoizing feels like the obvious de-duplication and
    passes every structural check except this one.
23. **You cannot redirect an existing member's references from inside a `MetaDesign`** —
    `dfc.mutableDB.newRefFor` only governs refs registered in the meta design's own DB (fresh clones
    and members it created). Calling it on a ref belonging to a member that is already in the stage's
    DB corrupts the ref table (`Failed reference check!`). To rewrite a member's reads you must
    either replace the member (`Patch.Replace` keyed on it) or rebuild it as a clone — which is also
    why a rewrite that must reach *several* members of a block is cleanest as a whole-block rebuild
    (see Pattern 15).
24. **Position within a block can be load-bearing** — before appending synthesized members, check
    whether a downstream stage reads the block positionally. A `fallThrough` block's condition is
    `members.last` in both `DropRTProcess` and `FirstStepFusion`, so a rebuild has to emit each
    synthesized read *before* the member that reads it rather than appending at the end.
25. **Naming an intermediate at its point of use may land it in a conditional expression branch** —
    inside an ED *domain body* (a concurrent scope), a branch of a `DFConditional.Header` with
    `dfType != DFUnit` is not a block, so a named value there is driven by a connection that later
    prints as `assign` inside an `always_comb` (issue #426). `DB.condExprNamedValCheck()` rejects
    it; place the name in an enclosing scope where all its operands are visible. The same shape is
    legal in an ED `process` and in RT/DF domains, where it lowers to a blocking assignment.
    Relatedly, `isInProcess` treats conditional blocks as transparent, so it is the wrong test for
    "does this member stay in a concurrent scope?". See *Stage-Facing Invariants* for the full
    story, including the conditional-header exemption.
26. **A new phase inside an existing stage is almost always wrong** — either the work is a
    self-sustained, idempotent, fix-point transformation, in which case it is its own **stage**, or
    it belongs in the **same patch** as the existing work. And when the work exists to keep the
    stage's *own output legal*, a separate stage is not an option either: `SanityCheck` runs after
    every stage, so the DB in between would be invalid. It has to be the same patch. Check the
    merge table before concluding that is impossible, and see the
    *replace AND relocate in one patch* recipe for the case that looks unmergeable but is not.
27. **Substituting into a cloned expression tree AFTER cloning it inverts the member order** —
    `cloneAnonValueAndDepsHere` builds each dependency before the value that reads it, which is the
    only order the flat member list accepts. If you then walk the finished clone and `newRefFor` a
    ref to a *freshly emitted* replacement (a forwarded value cloned here), that replacement is
    appended below its reader, and the reader points forward at a member defined under it. Pass the
    substitution in instead — `cloneAnonValueAndDepsHere(substDep)` applies it to every dependency
    while the clone is built, so anything it emits lands ahead of the reader:
    ```scala
    def substDep(dep: ir.DFVal): ir.DFVal = dep match
      case dcl: ir.DFVal.Dcl    => forwardedValueFor(dcl)   // may emit its own clone
      case _ if dep.isAnonymous => dep.cloneAnonValueAndDepsHere(substDep)
      case _                    => dep
    v.cloneAnonValueAndDepsHere(substDep)
    ```
    `SanityCheck.orderCheck` catches this (`Failed member order check!`). Note the printed output is
    typically **identical** either way, because an anonymous value prints inline at its use — so no
    code-string test will ever see it.
28. **Deferring a fix to a later stage lets an illegal DB exist in between** — a stage must never
    emit IR that violates a stated invariant, not even briefly. If you find yourself writing a
    cleanup stage for a shape an earlier stage produces, fix the producer instead. The way to
    discover this at all is to wire the invariant into `SanityCheck`; nothing else in the pipeline
    will tell you (`DB.check` runs once, at elaboration, and `SanityCheck` never calls it).

---

## Additional Transformation Patterns

### Pattern 12 — Memoize original-DB analysis across restructuring phases

A multi-phase stage can compute an analysis on its *input* DB, hold the results (member
instances or patch lists) across intermediate `db.patch()` phases, and apply/use them at the
end — because `Patch.Move` preserves member instances (only ownership refs are rewired).
`FlattenStepBlocks` uses this twice: Phase 3's `gotoPatchList` (computed on the nested input,
applied after phases 0–2 flattened everything) and Phase 4's fusion-candidate `Set[StepBlock]`
(nesting provenance is destroyed by flattening, so it must be captured up front). Prefer this
over inventing an IR tag to transport analysis between two separate stages — a tag also makes
self-contained spec inputs inexpressible when the frontend has no syntax for it.

### Pattern 13 — MetaDesign as an abortable sandbox (try/fallback transforms)

Constructing a `MetaDesign` mutates only its own private `MutableDB`; nothing touches the real
DB until its `.patch` is applied. You can therefore `throw` mid-construction to abandon a
speculative transform and fall back (e.g. `FirstStepFusion` throws `AbortFusion` when a dispatch
turns out not to be soundly inlinable, drops the offending step from its candidate set, and
rebuilds all patches from scratch in a `@tailrec` restart loop — the set shrinks each restart,
guaranteeing termination). Just never apply a partially-built design's `.patch`.

### Pattern 14 — A stage that CREATES a method def design (sub-DB forest surgery)

A stage can synthesize a whole HDL-method design (a `DFDesignBlock` with
`InstMode.Def`, e.g. a static function) and call it via `Func`/`Op.Def`. See
`DropInitialBlocks` Rule 1 (init-function conversion) for the full working recipe. The
non-obvious parts:

1. **Never `dfc.enterOwner`/`applyBlock` a design block inside a MetaDesign.**
   `OwnershipContext.exit()` on a `DFDesignBlock` calls `DesignContext.endDesign`, which
   assumes elaboration bookkeeping that meta-programming never set up. Instead, create the
   block raw (`ir.DFDesignBlock(...)` + `addMember`) and place every body member under it
   explicitly: `addMember(m)` then `newRefFor(m.ownerRef, defBlockIR)` (mirror
   `plantClonedMembers`'s mechanics with the def block as the owner fallback).
2. **Do NOT use `refTW` to reference a port of the new def design from the meta context** —
   `refTW` on a `DclPort()` whose owner design differs from the current owner mints a
   `PortByNameSelect` (and crashes on the design-inst cache in stages). Mint refs raw:
   `dfc.mutableDB.newRefFor(dfc.refGen.genTwoWay[M, O], member)`.
3. **Do not reuse an existing member's `dfType` instance in new members** — refs are
   identity objects; clone with `dfType.copyWithNewRefs` and bind each fresh type ref via
   `newRefFor` to the original target (lazyZip old/new `getRefs`).
4. **Self-containment**: a def-design member must not reference design-local values of the
   host (the `directRefCheck` rejects cross-design refs). Captured design-local constants
   become `PhantomTag`-tagged IN-port formals (redirect body refs to them; pass the
   captured values as the call's args, in formal member order). Globals are fine (shared by
   identity).
5. **The hierarchical model forbids a nested design block in the parent's member list.**
   After `subDB.patch`, extract the def block + its content (track them BY IDENTITY from
   the MetaDesign; do not use `isInsideOwner`, which detours through `designBlockInstMap`)
   into a NEW sub-DB: members = globals closure ::: defBlock :: locals, refTable = refs the
   members emit (mirror `oldToNew`'s `refsFor`), PLUS `defBlock.ownerRef -> DFMember.Empty`
   (SanityCheck's `refCheck` resolves `isTop` via `ownerRef.get`; the same ref object
   doubles as the sub-DB's `StaticRef` key). Rebuild the parent's refTable the same way
   (minus the def members). Such a stage must manage `designDB.update(subDBs = ...)` itself
   (mirror `HierarchyStage.transform`) since `HierarchyStage` cannot add sub-DB entries.
6. **Method shape expected by printers**: phantom formals first, then body, then an
   `IdentTag`-tagged ident of the return value connected to a non-phantom OUT port named
   `o` as the LAST members. `newToOld` emits the def body just before its first call, and
   the VHDL printer declares static functions between the constants and the
   signal/variable declarations (a signal's default may call one; a static function never
   reads signals).

### Pattern 15 — Rebuild a block member-by-member to rewrite what it reads

When a stage must redirect reads across *several* members of one block (mistake 23 rules out
editing them in place), rebuild the block's contents: anchor a `MetaDesign` on the block with
`Patch.Add.Config.InsideFirst`, re-emit every member as a clone with remapped ref targets, and
`Patch.Remove()` the originals. `InsideFirst` keys the Add on the block itself, so it never
collides with the per-member Removes (`InsideLast` is re-keyed onto the block's very last member,
which usually *is* one of them).

Unlike `plantClonedMembers`, resolve each member's ref targets **before** adding its clone, so any
member you synthesize for a target lands ahead of its reader (mistake 24). `getRefs` excludes
`ownerRef`, so set the owner separately:

```scala
val dsn = new MetaDesign(block, Patch.Add.Config.InsideFirst, dfhdl.core.DomainType.RT):
  given MemberGetSet = dfc.getSet   // clones resolve here, not in the stage's DB (mistake 17)
  val clonedOf = mutable.Map.empty[ir.DFMember, ir.DFMember]
  blockMembers.foreach { m =>
    val targets = m.getRefs.map { ref =>
      ref.get match
        case t: ir.DFVal => rewriteTarget(clonedOf.getOrElse(t, t).asInstanceOf[ir.DFVal])
        case other       => other
    }
    val cloned = m.copyWithNewRefs
    dfc.mutableDB.addMember(cloned)
    dfc.mutableDB.newRefFor(cloned.ownerRef, dfc.owner.asIR)
    cloned.getRefs.lazyZip(targets).foreach(dfc.mutableDB.newRefFor(_, _))
    clonedOf += m -> cloned
  }
dsn.patch :: blockMembers.map(_ -> Patch.Remove())
```

`copyWithNewRefs` preserves `meta`, so **named** members keep their names — which is the point:
`cloneAnonValueAndDepsHere` stops at named values, so a clone-the-expression-tree approach silently
leaves a named intermediate's own reads unrewritten, splitting one block's semantics in two.

When the rewrite wraps a value (rather than substituting one), mind chain-vs-reader: a partial
selection into the wrapped root is a *link* in the read chain, so wrap at its outermost consumer,
not at each link, or you produce an inside-out alias that may not even be printable.

---

## API Notes

### `getOwner` throws; walking owners needs `ownerRef.get`

`m.getOwner` raises `Exception: No owner found for member $m` whenever `ownerRef.get` is
`DFMember.Empty`, which covers **globals, the top design, and the `Goto` step placeholders**
(`NextStep`, `ThisStep`, `FirstStep`). Any pass that walks owners over *arbitrary* members, rather
than ones it already knows are in-design, must match `m.ownerRef.get` and treat a non-`DFOwner`
result as "no owning block". Getting this wrong turns one pass into hundreds of test failures whose
message names the API rather than the pass, so it reads like a broken build, not a broken walk.

### Raw `ir.DFNet` construction inside MetaDesign

Mirrors the raw `ir.Goto` idiom for emitting an assignment without going through typed core ops:

```scala
ir.DFNet(
  lhsIR.refTW[ir.DFNet], ir.DFNet.Op.Assignment, rhsIR.refTW[ir.DFNet],
  dfc.ownerOrEmptyRef, origNet.meta, origNet.tags
).addMember
```

### Cloning a single member with remapped refs

Mirror `plantClonedMembers`'s per-member mechanics when a custom per-ref remap is needed:
`val cloned = m.copyWithNewRefs` → `dfc.mutableDB.addMember(cloned)` →
`dfc.mutableDB.newRefFor(cloned.ownerRef, dfc.owner.asIR)` → zip `m.getRefs` with
`cloned.getRefs` and `newRefFor` each cloned ref to the (remapped) original target.

### Compile-time constant evaluation of values

`dfVal.getConstDataThroughParams[Any]` returns `Some(data)` when the (possibly substituted)
expression tree folds to a constant. Note the data may be `Option`-wrapped for bubble-capable
types — match both `Some(Some(b: Boolean))` and `Some(b: Boolean)` for a Bool/Bit guard.

### Full-width assignment check

`net.lhsRef.get.departialDcl` yields `(dcl, slice)`; the assignment covers the whole declaration
iff `slice.isFullOf(dcl.dfType.widthIntOpt) == Tri.Yes`.

### `dfhdl.core.DFInt32`

Available as both a type alias and a value (`ir.DFInt32.asFE[DFInt32]`). Use it inside MetaDesign
bodies to create 32-bit signed integer variables:

```scala
dfhdl.core.DFInt32.<>(VAR.REG).initForced(List(initConst))(using dfc.setName("i"))
```

This mirrors the `iterType.<>(VAR.REG)` pattern used for UInt variables.

---

## Checklist for a New Stage

- [ ] Stage `object`/`class` extends `Stage` (or `NoCheckStage` / `BundleStage` if appropriate)
- [ ] `dependencies` lists every stage that must run first
- [ ] `nullifies` lists every stage whose invariant this transformation breaks
- [ ] `runCondition` added if the stage should be skipped for some backends/options
- [ ] `transform` builds `patchList` and calls `designDB.patch(patchList)`
- [ ] Patch list is derived from `designDB.members` (ordered `List`) — never from an unordered `Set` or `Map` **[determinism]**
- [ ] Pattern matches target the *source* form only; the transformed IR should not re-match the same predicate, so `f(f(x)) == f(x)` **[idempotency]**
- [ ] Convenience `extension` method added at the bottom of the file
- [ ] Test file in `StagesSpec/` extends `StageSpec`
- [ ] **Every** stage the change touches has a test in its **own** `<Stage>Spec` (add the file and the `extension` entry point if the stage has none yet)
- [ ] At least one "basic" test and one "edge case" or "backend-specific" test
- [ ] `assertCodeString` expected strings verified manually or via a first-run snapshot
- [ ] Each new test verified to **fail** with its fix reverted, not just to pass with it
- [ ] `sbt test` passes (or `sbt quickTestSetup; test` for faster iteration via `lib/Playground.scala`)
- [ ] **Update this skill** with any general lessons learned (see below)

---

## Keeping This Skill Up to Date

While creating a new stage, you will often discover things that are not yet documented here:
new IR API behaviours, patch interaction subtleties, MetaDesign patterns, common pitfalls, etc.

**After completing a stage, review what you learned and update this file** when the lesson is
general enough to help with future stage creation. Specifically:

- **New IR / API behaviour** → add to the relevant section (IR Data Model, Patch System, MetaDesign).
- **New pitfall or gotcha** → add a numbered entry to "Common Mistakes to Avoid".
- **New reusable transformation pattern** → add a numbered "Pattern N" entry under "Transformation Patterns".
- **Corrected or outdated information** → update the existing section in place.

Do **not** add stage-specific implementation details here. Only document things that are likely
to recur in other stages. The rule of thumb: *would a future contributor hit this same issue if
they didn't already know about it?* If yes, document it.

Also update **stage-adjacent source files** when you discover missing or incorrect API documentation:
- `Patch.scala` — document `Patch.Move` / `Patch.Replace` / `Patch.Add` semantics
- `DB.scala` — document `MemberView.Folded` / `Flattened`
- `MetaDesign.scala` — document ownership and `plantMember` / `plantClonedMembers`
- `DFC.scala` — document DFC helpers used in MetaDesign

This skill and the source files it references are the authoritative guide for future stage authors.
Keep them accurate.
