# Methods

How a DFHDL `def` becomes hardware: as an inlined design instance, or as a real HDL function, task
or procedure.

DFHDL methods are ordinary Scala `def`s whose return type carries a DFHDL marker. The marker picks
the domain the body elaborates in, and, with it, what the method finally becomes in the output. This
document covers the whole path: the frontend types, the compiler plugin's recognition and capture,
the IR the harness builds, the call representation, and how the printers place and render it.

Related: [scoping.md](scoping.md) for the capability lattice the bodies live in,
[elaboration-caching.md](elaboration-caching.md) for the design load gate every method rides, and
[static-function-eval-plan.md](static-function-eval-plan.md) for the one deferred piece (evaluating
a static function's body to a constant).

## 1. Terminology

Fixed vocabulary, swept across code and docs. "Subprogram" and "design def" are retired.

| Term | Means |
|---|---|
| **method** | the umbrella term (Scala calls `def`s methods). Qualify by domain: DF method, ED method, static function |
| **function** vs **procedure** | by return value. A method returning a value is a function; a `Unit`-returning one is a procedure |
| **static function** | always say this. Static methods always return a value, so no static procedure exists by construction. API name is `isStaticFunction` |
| **method design** | the design BLOCK a method elaborates into (the artifact), as opposed to the method (the abstraction) |
| **HDL method** | ED plus static methods: the ones that stay callable methods in the HDL output. IR predicate `DFDesignBlock.isHDLMethod` |

Names kept deliberately because they are Scala-keyword-accurate: `Func.Op.Def`,
`DFDesignBlock.InstMode.Def`, and the `Func.Call` extractor.

## 2. The four kinds, and the one criterion that splits them

| Kind | Marker | Context parameters | Terminal form |
|---|---|---|---|
| DF method | `T <> DFRET` | `DFC`, `DomainType.DF` | design instance |
| RT method | `T <> RTRET` | `DFC`, `DomainType.RT` | design instance |
| ED function | `T <> EDRET` | `DFC`, `DomainType.ED`, `Scope.Function` | HDL function |
| ED procedure | `Unit <> EDRET` | `DFC`, `DomainType.ED`, `Scope.Procedural` | HDL task / procedure |
| static function | `T <> CONSTRET` | `DFCG`, `DomainType.Static`, `Scope.Function` | HDL function (`pure`) |

Defined in the `<>` match type, [DFVal.scala](../core/src/main/scala/dfhdl/core/DFVal.scala):

```scala
infix type <>[T <: DFType.Supported, M] = M match
  case DFRET    => (DFC, DomainType.DF) ?=> DFValOf[DFType.Of[T]]
  case RTRET    => (DFC, DomainType.RT) ?=> DFValOf[DFType.Of[T]]
  case EDRET    => EDRETOf[T]                 // Unit => Procedural, else Function
  case CONSTRET => (DFCG, DomainType.Static, DFC.Scope.Function) ?=> DFConstOf[DFType.Of[T]]
```

**The criterion that decides the terminal form is the printed HDL form, not the domain.**
Prints-as-a-method becomes a `Func(Op.Def)` call expression; becomes-an-instance stays a
`DFDesignInst`. DF and RT methods keep instances because a real design instance is what they lower
to, and their parameters stay parameters, which is what makes the generated modules generic.

### What each context parameter buys

The two evidence parameters do different jobs, and the split is worth internalizing.

- **Domain evidence gates the CALL SITE.** An ED method's `DomainType.ED` must be summonable where
  it is called, which confines it to ED domains. This yields a ladder: `CONSTRET` is callable
  everywhere (`Static` is the ambient given), `DFRET` in any dynamic domain (via
  `given fromRT(using RT): DF` and `fromED`, so `DF` is reachable without being ambient), and
  `RTRET`/`EDRET` only lexically in their own domain.
- **Scope evidence gates the BODY.** `Scope.Function` and `Scope.Procedural` carry the capability
  set the body may use. See [scoping.md](scoping.md).

Neither is optional for a static function. `DomainType.Static` shadows the enclosing design's domain
given, which would otherwise stay in scope inside the body (a def body is a lambda lexically nested
in its design) and bring `.reg`, `REG` variables and the rest back to life. `Scope.Function` is what
the plugin's method predicate keys on.

A static function's context is `DFCG`, not `DFC`. `DFCG` is summonable at global scope and, through
`given DFCG(using DFC)`, inside any design too, so a static call compiles everywhere while a bare
`DFC` stays unsummonable globally.

Procedural arguments carry direction through a type ANNOTATION the plugin reads, not through a
distinct type: `IN` is a readable value, `OUT` a copy-out assignable output, `OUT.NB` a non-blocking
live output. The argument type stays generic so ordinary actuals conform at the call site.

## 3. The plugin

[MethodsPhase.scala](../plugin/src/main/scala/plugin/MethodsPhase.scala), with capture discovery
shared from [CapturePhase.scala](../plugin/src/main/scala/plugin/CapturePhase.scala).

**Recognition is on the domain evidence, not the scope.** A static def and an ED function both carry
a `Scope.Function` parameter, so the scope predicate recognizes both as methods, which is what we
want. What separates them is the other context parameter, `DomainType.Static` versus
`DomainType.ED`. These are opaque types in core, so from outside `object DomainType` they are
distinct and a `<:<` test discriminates cleanly.

**Capture lift.** Out-of-scope value references in a body become explicit: DFHDL constants and DFHDL
values become phantom formals, plain Scala values become cache-key extensions (`scalaArgs`). The
body IS the `designFromDef` call, so captures are evaluated inside the def at call time, in the
caller's elaboration context. Call sites are never transformed, and a def compiled in another file
needs no signature knowledge.

Capture discovery is **transitive**. A call to another method does not run the callee's body in the
caller's scope, but it DOES evaluate the callee's captures there, so a capture the caller cannot
reach either is a capture of the caller as well. Every def between the captured value's design and
the call site materializes the capture as its own phantom. Limitation: propagation needs the
callee's tree, so it covers the defs of the unit being compiled; a nested call into a def compiled
in an earlier run does not propagate.

**Body content checks** (`checkHDLMethodContent`) are the PRIMARY enforcement for constructs that
have no type-level twin: a `process` carries no scope guard (a positive one would leak, see
[scoping.md](scoping.md) §3), and an ED-method call site summons `DomainType.ED` directly, which
reaches past a static body's `Static` given to the enclosing design's.

## 4. Elaboration: what a method becomes

All kinds share one harness, `designFromDefImpl` in
[r__For_Plugin.scala](../core/src/main/scala/dfhdl/core/r__For_Plugin.scala), differing by the
`domain` argument. Every caching and purity property of DF methods therefore holds for the others.

A method design is a **`DFDesignBlock` with `instMode = InstMode.Def`** and the method's domain.
The predicates live in [DFMember.scala](../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala):

```scala
def isEDMethod       = instMode == Def && domainType == DomainType.ED
def isStaticFunction = instMode == Def && domainType == DomainType.Static
def isHDLMethod      = isEDMethod || isStaticFunction
```

Inside the method design:

- **Formals are `DFVal.Dcl` IN ports**, non-phantom first, then phantom ones tagged `PhantomTag`.
  This holds uniformly for ED methods and static functions; a static function's const args are
  ordinary static IN ports.
- **The return is a `DFNet.Connection`** into a single OUT port named `o`, emitted after the body
  elaborates. That connection is what `methodReturnPort`
  ([DFValAnalysis.scala](../compiler/ir/src/main/scala/dfhdl/compiler/analysis/DFValAnalysis.scala))
  recognizes, and it is how function-versus-procedure is derived structurally: a procedure has no
  return port. An `<> OUT` argument port is also an output, but it is driven by ASSIGNMENT, so the
  return port is what distinguishes the two. On a cache hit the body never runs, so `DB.subDesignRetDFType`
  reconstructs the return type from the loaded sub-DB by the SAME rule (the connection-driven OUT port,
  DFUnit when there is none); reading it as merely the first output-class port would misread an OUT
  argument as the return of a Unit procedure and drop the call.
- **Locals** are ordinary `Dcl` VAR members with their assignment nets, interleaved in body order.

Every instantiation rides the **design load gate**, so a method unifies across call sites when its
key matches. `DesignLoadKey` is `dclMeta + inputTypes + scalaArgs + impureData`, and it is
owner-class independent, so a pure method used from several designs is ONE method design. See
[elaboration-caching.md](elaboration-caching.md).

## 5. The call: `Func(Op.Def)`

An HDL method application is a first-class expression, not an instance:

```scala
DFVal.Func(retType, Func.Op.Def(staticRef), actuals)
```

`Op.Def` carries the same design-block key `DFDesignInst.designRef` uses. A procedure's application
is a `DFUnit`-typed `Func` in statement position, which is exactly how `if`/`match` STATEMENTS are
already modeled.

This replaced an older model of `DFDesignInst` + `PortByNameSelect`, and the reason is worth
recording because it explains several things that are now absent. `Func.args` are plain refs, with
no nets at all, and `Func` extends `CanBeGlobal`. That buys global callability directly, and it
removes cross-context resolution through a port select, which was the source of a whole crash class
(a static call parameterizing a sub-design). `PortByNameSelect` returns to its single original
purpose: port reads on real design instances.

### Key mechanics

The `staticRef` is minted at the call site pointing at the CANONICAL method design (the load gate
already knows the canonical when the call is created), in the `ownerRef`-key form. It is a **stable
identity token**: never unified, never freshened by `copyWithNewRefs`, compared with plain `==`, and
deliberately excluded from `getRefs` and refTable enumeration.

Resolution is **structural**, through `MemberGetSet.getDesignBlockByKey`: the mutable run's design
registry, the root's `subDBs`, or a flat DB's `designBlockByKey`. It must never resolve through the
refTable, where the same token maps to the design's OWNER. `StaticRef.getDesignBlock` is the shared
path for both `DFDesignInst.designRef` and `Op.Def`.

The key is rewritten in exactly three places, all ordinary member rewrites: `UniqueDesigns`
(duplicate retarget), `ReduplicateDesign` (clone re-anchoring), and `SubDesignEntry.cloneForAdoption`
(the serialization boundary, keyed by `childTokens`).

### What has to count `Func.Call`

A method design may have NO instance referencing it, so everything that discovers or keeps
sub-designs alive through instances must also count calls: `childDesignsOf` (the forest walk and
cache-entry children), `designBlockOwnershipMap` and its domain twin, `usesClkRst`,
`edMethodCallSiteCheck`, `newToOld` and `canonicalForm` first-reference emission, `methodPrinters`
discovery, and `ReduplicateDesign`'s DFS.

Anonymous `DFUnit`-typed calls needed two exemptions beyond the printer's: `DropUnreferencedAnons`
and `SanityCheck.refCheck` both treat one as a statement, referenced by nothing by design.

Named calls need no special stage: a named call is a named `Func`, and `ExplicitNamedVars` converts
named expressions to variables on its standard path. (A dedicated `PrepEDDefs` stage used to do
this and was deleted; its spec was absorbed.)

## 6. Phantoms

A phantom is a formal the user never wrote, materializing a captured outer value. It is tagged
`PhantomTag`, appended after the explicit formals, and paired with its actual **positionally** (the
arg at the phantom formal's index).

Printers hide phantoms from the signature and from the actual list, and a body reference to one
prints as **the actual's code string in the owning design's scope**, not as the phantom's own name.
`AbstractPrinter.methodPrinterAt` builds that substitution at the call site, where the host's getSet
resolves the actual, and hands it to the printer rendering the body. Printing the phantom's own name
instead was a real defect: a multi-step capture path (`sub.o`) named the phantom after the leaf
while the value lived in `sub_o`, emitting a dangling identifier.

Substitution composes for free through nested calls: the inner call's actual is the caller's own
phantom, which the caller's printer substitutes again, so the body names the value exactly as the
host does.

Static functions may capture constants only. A captured non-constant would be a non-constant input,
which contradicts staticness, and the plugin rejects it.

## 7. Static functions

A static function is the method form of the **static domain**, the degenerate bottom of the domain
lattice ([DomainType.scala](../compiler/ir/src/main/scala/dfhdl/compiler/ir/DomainType.scala)):

```
DomainType
├── Static
└── Dynamic
    ├── DF
    ├── RT
    └── ED
```

Two axes are kept separate and separately named. **Constness is a VALUE property** (`<> CONST`,
`isConst`, `ConstData`); **staticness is a REGION property** (does time advance here at all). A
`CONST` value can live in any domain; a `Static` region is one where every value is constant.
`Static` is not a fourth level of abstraction: DF, RT and ED remain the three levels.

`Dynamic` is a real sealed layer, not a marker, and that is load-bearing. Nearly every existing
`DomainType` site asks a timing question and must stay exhaustive over the three dynamic cases. A
flat fourth case would let `case _` fall-throughs swallow `Static` silently. The same hazard exists
at the type level: guards phrased negatively (`NotGiven[A <:< DomainType.ED]` for `.prev`) admit
`Static` for free, so they carry a positive `A <:< DomainType.Dynamic` conjunct.

### Static ports carry constant data, and the body is data-blind

A `Dcl` whose owning design block is a static function resolves `UnknownConst`: the static domain is
timeless, so every value in it is a constant, of unknown value. That rule is what keeps body reads
of formals const-typed.

The owner walk implementing it must NOT force `getOwnerDesign`. `getConstData` runs on values whose
owner chain is transiently out of scope during meta-programming, and forcing it reproducibly crashes
the RT-loop stages. The guarded `ownerRef.getOption` walk is inlined in `Dcl.protGetConstData`; a
plain `getOwnerDesign` has been tried twice and reverted twice.

**Data-blindness is a feature, not a limitation.** A formal port carries no applied-data snapshot, so
a static body CANNOT observe an argument's applied value at elaboration: forcing it (`n.toScalaInt`,
a Scala-level branch on `n`) fails with an unknown-constant error rather than silently specializing
the body. Bodies therefore never diverge per call site, and one printed body per load key is sound.
An earlier model made const args design parameters, which are visible to elaboration, and that
required a structural body-dedup stage to stay correct; the port model closes the hole by
construction and the dedup stage is not needed.

The consequence is that a static function call folds to `UnknownConst`, never to a value. Evaluating
the body is deferred; see [static-function-eval-plan.md](static-function-eval-plan.md).

### Purity is enforced, not opted into

A static function is pure by definition, and `PureCheckPhase` already computes the verdict for every
def. The only change is that for a static def an impure verdict is an ERROR rather than the advisory
"never cache" it is elsewhere.

The trap: `pure(true, impureParams*)` means the def IS pure. The names only say that a parameter's
applied data was forced into elaboration and must enter the cache key. A static function with
phantom constants is legal and pure. Only `pure(false)` is an error.

`PureCheck` reasons about Scala-level effects only (DFHDL's core is on its trusted list), so the
DFHDL-level half is covered separately, and the conjunction is what "pure" means here:

| Effect | Caught by |
|---|---|
| randomness, IO, time, outer `var`, impure callee | `PureCheck`, fatal for static defs |
| reading a captured DFHDL signal | the captured-non-constants plugin check |
| writing anything outside the body | no ports beyond the return |
| assertions, printing | `Scope.Function` grants no `HasTextOut` |

`HasTextOut` staying off `Scope.Function` is therefore load-bearing and must not be relaxed for
convenience. VHDL's `pure` is weaker than ours (it only forbids referencing outer signals and shared
variables, and permits `report`), so emitting the `pure function` keyword is always sound.

Purity does not bear on recursion. Recursion is rejected for methods generally, for an ELABORATION
reason: the Scala body re-runs per call site, so a recursive def would not terminate. A pure function
may legally recurse in both VHDL and Scala, so do not cite purity when explaining the rejection.

## 8. Printing and emission placement

The formal list is a method's non-phantom IN ports, minus the return port, uniform across kinds.
Call sites print `call.args.take(visibleFormalCountOf(design))`. Call rendering is per-backend
(`csMethodCall`): DFHDL always parenthesizes; VHDL prints parameterless calls bare and procedural
calls with `;`; Verilog keeps the task and parenless rules plus the v95/v2001 dummy-`0` rule.

Backend forms:

| | Function | Procedure |
|---|---|---|
| VHDL | `pure function` (static) / `impure function` iff phantom INs exist (ED) | `procedure` (no purity keyword) |
| SystemVerilog | `function automatic` | `task automatic` |
| v95/v2001 | plain `function`, `logic` stripped, `__dummy__` input plus literal-`0` actual when the printed input list is empty | static task (no `automatic`) |

Verilog methods are static by default (concurrent calls share one set of arg and local storage), so
printers emit `automatic` unconditionally where available. Naming collision worth flagging to anyone
reading the SV printer: SystemVerilog's `static` means variable LIFETIME and is the OPPOSITE of
`automatic`, so a Static-domain function emits an `automatic` function.

### Local, or shared in the globals area

A method used by exactly one design prints inside that design's declarative region. A method used by
MORE THAN ONE design is emitted ONCE in the shared globals area, like a named type or a global
constant. This is a placement decision computed from existing IR, with no IR or forest change:

- `hdlMethodDesignUsers` maps each method design to the non-method designs that reach it, resolving
  method-to-method calls transitively.
- `globalHDLMethods` (an overridable `def` on `AbstractPrinter`) is the blocks used by more than one
  design AND package-eligible, expanded to their transitive method callees so a package function
  never calls an architecture-local one.

Unification is free: a pure method already unifies across call sites through the load gate, so the
"same method, different key" hazard never arises. Eligibility resolves each phantom's actual at every
call site: a phantom materializing a GLOBAL is package-safe, one materializing a design-local capture
is not (an actual that cannot be lined up with the formals is treated as design-local, the
conservative answer).

**VHDL adds one rule.** A static function read by a PORT DECLARATION (its init or its parametric
width) must be visible at the entity level, because the port clause elaborates before the
architecture. `VHDLPrinter` overrides `globalHDLMethods` to add the static functions reachable from
any port's `dfType.getRefs ++ initRefList`, even when a single design uses them. Verilog does not
override and correctly keeps such a function module-local, since it lowers a port init to an
`initial` block inside the module body where the function is already visible.

**Globals are emitted in dependency order.** Between a global constant and a global HDL method the
dependency runs both ways: a constant's value may CALL a method, and a method's body may READ a
constant, so a fixed bucket order is wrong in one direction. Emission is a stable topological sort
over the actual references: each declaration follows everything it references, independent
declarations keep source order, and a reference cycle falls back to declaration order. Global TYPE
declarations stay first and are not part of the sort.

**Local declarations use the same sort.** A design's own constants, static functions, signals, and
ED methods have the same both-way dependencies (a constant may call a static function; a signal's
default may call one; an ED method reads signals), so both backends emit them with one stable
topological sort (`Printer.localDeclsOrdered`) rather than a fixed const/static-function/signal/ED-method
split. The seed order groups the kinds, so independent declarations still print in that layout. The
local TYPE declarations and the constants a type uses as a width (`typeReferencedConsts`, found by
following width EXPRESSIONS like `clog2(N)` transitively, not just bare-constant widths) lead ahead
of the sort; a `TypeRef` is dropped from the dependency walk since that leading block already
satisfies it. Verilog keeps its non-ANSI port-direction declarations and output `initial` blocks in
a separate port block, outside this ordering.

### Sensitivity lists

`always_comb` is sensitive to signals read inside a called function's body; `always @*` is sensitive
only to the function's arguments. A function with hidden phantom reads called from a combinational
process would silently break under `@*`-era dialects and VHDL-93 explicit lists, so explicit-list
rendering adds the phantom-read signals of transitively called functions. SystemVerilog needs
nothing.

Under VHDL-2008 and later, `process(all)` misses impure-function hidden reads, so phantom-carrying
processes are force-converted to explicit lists.

## 9. Where each rule is enforced

| Rule | Enforced at |
|---|---|
| call-site domain legality (ED method only in ED, etc.) | type level, domain evidence |
| body capabilities (no ports, no processes, no text output in a function) | type level, scope evidence |
| all-const args for a static function; no `Unit` `CONSTRET`; no const args on ED methods | plugin, `MethodsPhase` |
| captured non-constants in a static body | plugin |
| direct recursion | plugin |
| purity of a static def | plugin, `PureCheck` verdict promoted to an error |
| processes, ED calls from a static body, and the rest of the body content | plugin `checkHDLMethodContent` (primary), `SanityCheck.hdlMethodCheck` (backstop) |
| a waiting method called outside a forever or initial process | `edMethodCallSiteCheck`, on the root DB |

`hdlMethodCheck` runs in the SanityCheck stage (debug mode), NOT on the elaboration `DB.check` path.
It is the backstop for scope evidence laundered through helper `def`s, whose bodies the plugin's
syntactic check cannot see. What it rejects: `Wait` (except in procedures), non-blocking assignment
(except to an `<> OUT.NB` argument), `Goto`, `ProcessBlock`, step/fork/domain blocks, design
instances, and ED-method calls from a static body.

## 10. Global-scope static calls

A static function called at global scope (`val W: Int <> CONST = clog2(N)`) elaborates, homes and
prints. Its def block is built in a detached global `DFCG` mutableDB; its BODY lives in the def's own
design context, so it does not ride the global member injection. `endDesign` builds the def's
self-contained sub-DB and carries it on the global `DesignContext` (`globalDefSubDBs`), `inject`
merges those alongside members and refTable, and `hierarchical` appends them to the forest like
adopted sub-DBs. A def called from INSIDE a global-scope call is descended in `endDesign`, where the
elaborating context still holds the whole nest; descending at assembly time cannot work, because the
referencing run receives only `globalDefSubDBs`.

Global-scope calls are **never cached**. Such a call runs in a `DFCG` that uses default elaboration
options, and the cross-run adopt path does not model the loaded global sub-DB (a cache hit re-keyed
the def and broke `newToOld`, non-deterministically across an sbt-server session through the
process-wide store). `designFromDefImpl` detects the global-scope call and forces `cacheEnable=false`.
Re-elaboration per call is cheap and the printed HDL is unchanged.

One trap this created, now fixed: a global-scope call precedes the top design in the member list, and
first-reference emission pulled the call's target def block in with it, leaving that block at the HEAD
of the flat member list. `DB.top` is the first non-global member and `membersNoGlobals` filters only
`DFVal.CanBeGlobal`, so a design block is not filtered out, and the static function took over the top
name, the defs-header name and the output folder. Such a block is now deferred while the top is still
pending and flushed once it is emitted.

## 11. Open issues

### Correctness gaps

1. **`staticRef.getDesignBlock` can throw a `ClassCastException` instead of resolving.** When the
   registry lookup misses, `MutableDB.DesignContext.designAt` falls back to the live refTable
   (`self.getMember(design.asRef)`), which is the load-bearing path for a pre-unification
   `DFDesignInst.designRef`. But the ref's declared type is `OneWay[DFDesignBlock]`, so that call
   site carries a compiler-inserted cast: when the refTable holds the `DFMember.Empty` placeholder
   of an unplanted forward reference, the cast throws rather than the lookup failing cleanly.
   Observed from `Func.protGetConstData`'s `Op.Def` case, reached through
   `getConstDataThroughParams`, on the VHDL static-function port-width path.

   A guarded `getDesignBlockByKeyOption` was tried once and reverted, on the grounds that "a design
   block is always reachable from its key" is the right invariant and that tolerating a violation
   hides the real defect. That reasoning stands, so the fix is to make the failure diagnosable (an
   unplanted forward reference is a real state during elaboration, and a `ClassCastException` names
   neither the key nor the caller), not to swallow it. Whoever writes an `Option`-returning variant
   must widen the `getMemberOption` result to `Option[DFMember]` before
   `collect { case d: DFDesignBlock => d }`: matching on the ref's DECLARED static type elides the
   runtime type test, so `Empty` passes straight through and the cast throws anyway. That is what
   sank the reverted attempt on its first cut.
2. **A `<> VAR` in the static domain is typed as a non-constant.** `Modifier`'s fourth axis is
   parametricity (`P`, `CONST` or `NOTCONST`), and `VAR` is unconditionally
   `Modifier[Assignable, Connectable, Initializable, NOTCONST]` regardless of the domain it is
   declared in. In a static body every value is constant, so the declaration should carry
   `P = CONST` and `:=` on it should accept only constants. The visible symptom is that a static
   variable cannot be returned from a `CONSTRET` method without an unchecked cast, which is exactly
   the `acc.asInstanceOf[UInt[8] <> CONST]` in `StaticFunctionSpec`'s `sum3`. Note the const-only
   assignment rule itself holds today by construction (a static body has no ports, its formals are
   const, and non-constant captures are rejected), so this is a typing gap rather than an open hole:
   the type does not say what is already true, and the cast is what fills in for it.
3. **`OUT.NB` and `OUT` formals accept each other's actuals.** In the `<>` match type both `OUT` and
   `OUT.NB` map to `DFVarOf[...]` and differ only by the type annotation the plugin reads, so a call
   site binds any assignable variable to either. An `<> OUT.NB` formal will therefore take an actual
   that is only blocking-assignable, and an `<> OUT` formal will take one that should be driven
   non-blocking. The root fix is on the declaration side, and it shares its shape with item 2: a
   `<> VAR` or `<> OUT` should be typed by its DECLARATION SCOPE on the access axis, non-blocking
   outside a process or procedure and blocking within one, unless it is explicitly declared
   `OUT.NB` in a procedure. With the capability recorded on the declaration, the formal's type can
   demand it and the mismatch becomes a compile error rather than a silently wrong lowering.
4. **v95/v2001 dialect gates are missing.** Struct and opaque and unpacked-array args, and
   non-integral returns, print unchecked under legacy dialects. Fix: verify or flatten in a backend
   prep stage, or `printer.unsupported`.
5. **v95 waiting tasks are static** (no `automatic` before v2001), so concurrent calls of a
   wait-containing task share one arg and local storage. UNDECIDED: forbid wait-containing methods
   under v95, or document the hazard.
6. **Mutual recursion is not detected** (direct recursion is a plugin error). It surfaces as an
   elaboration stack overflow. UNDECIDED: detect it (needs cross-def call-graph knowledge the plugin
   does not have per-unit) or keep it documented-only.

### Undecided design questions

7. **Phantom-OUT lowering for VHDL.** Either declare the procedure in the calling process's
   declarative part (phantoms stay hidden, body duplicated per calling process), or promote
   phantom-OUTs to real signal-class formals per call site (one body, phantoms become visible args).
   Blocks phantom-OUT capture entirely.
8. **Explicit `<> IN`/`<> OUT` procedural args.** The semantic rules are drafted (`VAL` = copied
   value, `IN` = live signal-class formal, OUT class inferred from `:=` versus `:==`, mixing is an
   error, no explicit OUT on waiting methods, multi-driver check for phantom-OUT from two processes)
   but the direction-aware lowering is not implemented. This needs an "args with directions" helper:
   `Func.args` carry no direction, so read and write analyses must classify args by zipping them
   with the method design's formal directions through the `staticRef`. One centralized helper,
   consulted by `getReadDeps`, driver and multi-driver checks, sensitivity and `SanityCheck`.
   `DFDesignInst` plus nets gave drivers-analysis this for free; under `Func` it simply must not be
   forgotten, or an OUT actual silently reads as a READ.
9. **Const args for regular ED methods.** Rejected today ("Constant arguments are not supported for
   ED methods") because a const arg is visible to elaboration and there is no dedup step to honor a
   body that diverges per call site. A future implementation is tool-verified: VHDL needs no
   monomorphization (a constant formal can even size a local), while SystemVerilog has no method
   parameters, so a type-shaping param must be monomorphized there. Classify each param as
   value-only or type-shaping, lower value-only ones to formals in both backends, and monomorphize
   type-shaping ones for Verilog. The printers must also stop dropping `DesignParam` from method
   local declarations.

### Pending work

10. **Unused-phantom pruning.** Static over-capture from meta-dead branches is harmless in printed
   bodies but pollutes explicit sensitivity lists.
11. **Simulation validation.** No `testApps` case exercises ED methods or static functions yet.
   Target: a design using an ED function with phantom capture plus a waiting task, across tools and
   dialects.
12. **IR reference coverage.** `.claude/commands/ir-reference.md` lists `InstMode.Def` in the enum but
   documents neither `PhantomTag` nor the `Func.Op.Def` call form. (The user-facing side is covered:
   `docs/user-guide/methods/index.md` documents all four kinds.)
13. **`<>` and `:=` on `DFCG`**, for a nicer scope-guard message on true-global declarations. Blocked
    by a dotty `LambdaLift` proxy issue on the multi-`val` pattern (`val a, b, c = Bit <> IN`).
14. **Cross-run caching of global-scope static calls**, currently disabled (§10). Would need the
    adopt path to model the loaded global sub-DB.
15. **The folding interpreter**: evaluating a static function body to a `KnownConst`. Planned in
    [static-function-eval-plan.md](static-function-eval-plan.md).

### Known hazards, documented rather than fixed

- **The helper-def hole.** A plain Scala helper `def` carrying a `DFC`, called from a method body,
  can reference outer members from ITS ast, which the plugin's lexical scan of the method body
  cannot see. Such references surface at elaboration as cross-boundary references.
- **Plain-Scala captures.** `Int` or `Boolean` fields that are not `<> CONST` are silent closure
  captures. They are joined to the load key through `scalaArgs`, but a plugin warning on non-DFHDL
  free references of non-singleton types would be a useful hardening.
- **An explicit user `@pure` is a trust override.** `PureCheck` does not analyze such definitions, so
  it cannot mark them impure and the static-def error cannot fire. Deliberate, and consistent with
  the rest of the phase, but it means a user can assert purity the analysis cannot prove.
- **Multi-read anonymous calls print the call per read site.** Accepted behavior, not a gap: the
  methods are pure, so it is semantically sound.

### Out of scope

VHDL unconstrained-parameter lowering (replacing monomorphization); SV `ref` relaxation for waiting
task OUT args; INOUT args; `$time`, `$stop`, randomness and file I/O companions; string-typed args
across the method boundary; DPI and VHPI-backed methods through `EDBlackBox.Foreign`; a user-facing
`StaticDomain { ... }` block; static procedures; retrofitting globals into a real `Static` domain
owner (which would make `getOwnerDomain` total, replacing the ownerless-member sentinel).
