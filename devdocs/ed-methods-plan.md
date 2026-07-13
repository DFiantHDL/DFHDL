# ED Methods (HDL Functions / Tasks / Procedures) Plan

> Status: **Phases 1A+1B+1C + Phase 2 (procedural core) IMPLEMENTED**

## OPEN ISSUES — the single authoritative list (2026-07-11, updated 2026-07-14)

Everything open/undecided/unresolved lives HERE. Scattered "deferred" notes elsewhere in this
document point to this section. Categories: correctness gaps can produce wrong or invalid HDL
today; undecided questions block their features; pending work is decided and just not done.

> **2026-07-14 merge into `pure-checks`.** ED methods now ride the general design-def
> machinery of the elaboration-caching branch (see `devdocs/elaboration-caching.md`): the
> design load gate (intra-run + cross-run service), pure-by-default with the `PureCheck`
> plugin analysis, harness-owned parameters ("params ride the call", implemented as
> `designFromDefGetParam`), generalized phantom capture for ALL design defs (values,
> constants, and plain-Scala captures keyed via `scalaArgs`), and the compile-time code
> digest. `designFromDefED` shares the `designFromDef` harness verbatim (one `domain`
> argument apart), so every caching/purity property of DF design defs holds for ED methods.
> Consequences for the list below: item 1(c) and the first half of item 5 are FIXED; the
> `toScalaXYZ` half of item 5 is handled by PureCheck data-impurity attribution
> (`impureParams` keying), with the residual approximations documented in
> `elaboration-caching.md` (open issue 3 there).

### Correctness gaps (can produce wrong/invalid HDL today)

1. **Explicit `<> CONST` args on ED methods: REJECTED BY THE PLUGIN (2026-07-14).**
   `DesignDefsPhase` now errors on any `<> CONST` argument of an ED method ("Constant
   arguments are not supported for ED methods."). Captured outer constants are unaffected:
   they become phantom parameters, which print correctly at the enclosing design's scope
   (verified: `localparam logic [7:0] k = 8'd5;` in the module, body references `k`).

   Why (investigation, 2026-07-14): a `<> CONST` param produced INVALID HDL in every case,
   not just when call sites applied different values — with a single call site and a single
   value, both backends printed one body with a DANGLING `k` (the subprogram printers drop
   ALL `DesignParam`s from local decls, and unlike a captured constant there is no
   enclosing-scope binding to name). Symptom (c) of the old item 1 (the DFHDL re-emitter
   losing the second call's applied value under a pure-cache hit) is FIXED by the
   pure-checks merge: applied params now ride the call (`designFromDefGetParam`), and the
   DFHDL view roundtrips correctly (`addK(k = d"8'5")(a)` / `addK(k = d"8'7")(a)`).

   What a future implementation must do (tool-verified 2026-07-14, GHDL/NVC/Verilator):
   * VHDL needs NO monomorphization. A subprogram formal defaults to class `constant`, and
     subprogram locals elaborate per call, so a constant formal can even SIZE a local
     (`variable tmp : unsigned(w - 1 downto 0);` analyzes clean on GHDL and NVC). Lower each
     `<> CONST` param to a trailing constant-class formal; pass the applied value per call.
     Caveat: a constant formal is not LOCALLY STATIC, so it cannot be a case choice.
   * SystemVerilog has no subprogram parameters and no generic subprograms. An argument
     works as a plain VALUE (verified clean under Verilator), but is not a constant
     expression: `logic [w-1:0] tmp;` fails with "Expecting expression to be constant, but
     variable isn't const: 'w'". A `localparam` inside a function body is legal but cannot
     be fed from an argument.
   * So the tiered lowering (home: `PrepEDDefs`, which is dialect-aware through
     `CompilerOptions`): classify each non-phantom `DesignParam` as VALUE-ONLY or
     TYPE-SHAPING (does any DFType in the method design reference it?); lower value-only
     params to subprogram formals in BOTH backends; lower type-shaping params to formals in
     VHDL; MONOMORPHIZE type-shaping params for Verilog (one copy per applied-value group,
     param emitted as a `localparam` inside each copy). In every path the printers must stop
     dropping `DesignParam` from subprogram local decls (the one line causing the dangling
     reference). Monomorphization by GROUPING is cheap: making ED method params part of the
     design load key (as `impureParams = "*"` does) already yields correctly-wired per-value
     designs with `UniqueDesigns` renaming them (`addK_0`/`addK_1`) — but doing it at
     ELABORATION also splits the DFHDL view, so prefer the backend stage.
2. **Phantom body references assume the captured value is nameable at the owner's scope**
   (demo-verified 2026-07-14; supersedes the old "per-module phantom-actual grouping" framing,
   whose stated trigger turned out to be a NON-issue — see below).

   The printers hide a phantom port and its call-site wiring, and print the body's reference to
   it as the PHANTOM PORT'S OWN NAME, which is the captured path's LEAF name
   (`captureName = path.head`). That silently assumes the leaf name denotes the captured value
   at the owning design's scope. It does when the capture is a direct member of the owner (a
   port, var, or constant), which is every case in the current tests. It BREAKS for a
   multi-step capture path, because the actual has a different name at module scope:

   ```scala
   class FooSub extends EDDesign:
     val sub = Inner()                                    // Inner has an `o` OUT port
     def addSub(l: UInt[8] <> VAL): UInt[8] <> EDRET = l + sub.o   // capture path: sub.o
     y <> addSub(a)
   ```
   emits (SystemVerilog):
   ```systemverilog
   logic [7:0] sub_o;                                     // the actual, at module scope
   function automatic logic [7:0] addSub(input logic [7:0] l);
   begin
     addSub = l + o_0;                                    // `o_0` is declared NOWHERE
   end
   endfunction
   ```
   The phantom is named after the leaf (`o`, uniquified to `o_0`) while the value lives in
   `sub_o` → dangling identifier → invalid HDL (LOUD: fails at tool compile, not silent).

   NON-issue (verified): an ED method declared on a SHARED TRAIT and mixed into two designs is
   fine. The gate unifies it into ONE method design across both owners, but each owner prints
   its own copy of the subprogram and the body's `b` resolves to that module's own `b` member.
   Name-based resolution is exactly what makes it work.

   **FIXED (2026-07-14).** A phantom body reference now prints the ACTUAL's code string in the
   owning design's scope, instead of the phantom's own name. `AbstractPrinter.defPrinterAt`
   builds, at the call site (where the host's getSet resolves the actual), a substitution map
   from each phantom member's name INSIDE the def to `csDFValRef(actual, inst.getOwner)`, and
   hands it to the printer that renders the def body; `csDFValRef` consults it for any
   `PhantomTag` member. Both places that render a def body go through it (the ED-method
   declarations of `edMethodPrinters`, and the inline declaration of a phantom-carrying DF
   design def in `csDFMembers`), so all three printers (DFHDL, Verilog, VHDL) are covered.
   This also makes the shared-trait case correct by construction rather than by luck.

   Implementation notes: phantom PORTS are paired with their call-site connections BY ORDER,
   not by name — the PBNS records the port's name as of the connection, while the def's port
   may be uniquified afterwards (the `sub.o` capture becomes `o_0` once it collides with the
   def's `o` return port), so `pbns.portNamePath` is stale. The harness appends phantom ports
   and connects them in the same order, which is what the pairing relies on. Phantom PARAMS are
   matched by name (the `paramMap` key IS the parameter's name).

   Grouping (the old framing) is only needed if one module ever holds two calls of the same
   method bound to different actuals — not currently reachable, since a def's capture paths are
   fixed at its declaration.
2b. **Nested ED method calls (a method calling another method)**: two separate PRE-EXISTING
   defects, found 2026-07-14 while probing item 2. This contradicts the Phase 2 note claiming
   nested calls work: the tested cases call several methods from a PROCESS, never from another
   method's body.
   * **Declaration discovery — FIXED (2026-07-14).** `Printer.edMethodPrinters(design)` scanned
     only the OWNER's own members, so a call made inside another METHOD's body was never
     discovered and the callee never declared: `def inner(l) = l + 1; def outer(l) = inner(l) +
     1; y <> outer(a)` emitted a module declaring only `outer`, whose body called an undeclared
     `inner` (invalid HDL, loud). Discovery now recurses through the method bodies themselves,
     and the result is POST-ORDER (a method follows the methods it calls), since an HDL
     subprogram must be declared before it is used. Each method binds to its FIRST call site's
     printer, which is what resolves its phantom actuals (item 2). Emitted HDL verified to
     analyze on Verilator, GHDL and NVC.
   * **Elaboration crash — STILL OPEN.** If the INNER method captures anything (a phantom),
     elaborating a nested call throws `NoSuchElementException: None.get` at
     `DFDesignBlock.getCachedDesignInst` (via `r__For_Plugin.exitAndConnectInputs` ->
     `connect` -> `refTW`): the phantom's call-site connection is made in the OUTER method's
     design scope, where the referenced host value has no design inst to route through. Until
     this is fixed, a nested call is only usable when the callee captures nothing.

3. **v95/v2001 dialect gates are missing** (planned in 1C, not implemented): struct/opaque and
   unpacked-array args and non-integral returns print unchecked under legacy dialects (the S6
   limits). Fix: verify/flatten in `PrepEDDefs` or `printer.unsupported`.
4. **v95 waiting tasks are static** (no `automatic` before v2001): concurrent calls of a
   wait-containing task share one arg/local storage. UNDECIDED: forbid wait-containing methods
   under v95 (elaboration/backend check) or document the hazard.
5. **FIXED (2026-07-14, pure-checks merge)** — both halves:
   plain-Scala captures are discovered by the general capture rigging and joined to the
   design load key via `scalaArgs`, so two instances differing in a captured Scala field
   no longer share one body; and `toScalaXYZ` extraction is handled by the `PureCheck`
   analysis (data-impurity attribution: forcing a constant parameter's or capture's data
   records it on the `pure(impureParams = ...)` annotation, and the applied data joins the
   key; unattributable forcing escalates the design to `pure(false)`, which makes it
   keyless). Residual approximations (conservative escalation on body-locals, static
   dispatch) are tracked in `elaboration-caching.md`, not here.

### Undecided design questions (block their features)

6. **Phantom-OUT lowering for VHDL** (§S2): (a) declare the procedure in the calling process's
   declarative part (phantoms stay hidden; body duplicated per calling process) vs (b) promote
   phantom-OUTs to real signal-class formals per call site (one body; phantoms become visible
   args). Blocks phantom-OUT capture entirely.
7. **Explicit `<> IN`/`<> OUT` procedural args**: semantic rules are drafted in §S2 (`VAL` =
   copied value, `IN` = live signal-class formal, OUT class inferred from `:=` vs `:==`,
   mixing = error, no explicit OUT on waiting methods, multi-driver check for phantom-OUT from
   two processes) but NOTHING is implemented — the `<> IN`/`<> OUT` match-type cases for def
   args do not exist yet. Until then procedural methods are read+wait+report only, and `:==`
   is rejected in all method bodies.
8. **Mutual recursion** is not detected (direct recursion is a plugin error): it surfaces as an
   elaboration stack overflow. UNDECIDED: detect (needs cross-def call-graph knowledge the
   plugin doesn't have per-unit) or keep documented-only.

### Pending work (decided, not started)

9. **Unused-phantom pruning** (static over-capture from meta-dead branches, §S8): harmless in
   printed bodies but pollutes explicit sensitivity lists. Planned home: `PrepEDDefs`.
10. **Named `Unit <> EDRET` calls in `PrepEDDefs`**: a named procedural call (`val x = show(a)`)
    is currently left as-is (prints as a plain call statement; the name is dropped in HDL).
    Verify roundtrip behavior and either normalize (anonymize the inst) or accept-and-test.
11. **Phase 1D sim validation**: no `testApps` simulation exercises ED methods yet (target: a
    design using an ED function with phantom capture + a waiting task, across tools/dialects).
12. **Documentation**: no user-guide page for ED methods (`docs/`); `ir-reference.md` lacks
    `PhantomTag`/ED-`InstMode.Def` semantics; the helper-def hole and the plain-Scala capture
    hazard (items 5, §S8) need prominent user-facing documentation.

### Out of scope for now (Phase 3+ backlog, recorded in "Later phases")

Package/shared emission for phantom-free methods; VHDL unconstrained-parameter lowering
(replacing monomorphization); SV `ref` relaxation for waiting-task OUT args; INOUT args;
`$time`/`$stop`/random/file-I/O companions; string-typed args across the def boundary;
DPI/VHPI-backed methods via `EDBlackBox.Foreign`.

### Related pre-existing gap (not ED-specific)

`:=` on an OUT port under VHDL prints invalid `z := ...` (VHDL-targeted user code uses `:==`);
discovered during 1C, tracked independently of this feature.

## Implementation history

> **[PHASE 2 IMPLEMENTED — procedural core]** (2026-07-11): `Unit <> EDRET` is live —
> procedural ED methods (Verilog `task` / VHDL `procedure`) with `<> VAL` args (+ phantom-IN
> / phantom-param capture), `wait`, `TextOut`, local `VAR`s + `:=`, and nested method calls.
> Implementation notes:
> - `Scope.Procedural extends Local` (NOT `Process` — steps stay unavailable in bodies) with
>   a CONDITIONAL given `given (using Scope.Process): Scope.Procedural` in `object Scope` —
>   call sites are gated to processes/other procedural bodies, and being conditional (not
>   ambient like `Scope.Function`'s) it poisons nothing. `Scope.Procedural` carries an
>   `@implicitNotFound` message ("A procedural ED method (`Unit <> EDRET`) can only be
>   invoked inside a process or another procedural ED method body"); the compiler appends
>   its implicit-chain explanation after it. `DomainType.ED` also carries one ("An ED
>   method can only be invoked inside an event-driven (ED) domain") — safe because the ONLY
>   direct `DomainType.ED` context-parameter summons are the two `EDRETOf` context-function
>   types (the ambient `Scope.Function` given never fails to summon, so its own
>   `@implicitNotFound` is effectively dormant).
> - `InsideProcess:=` union gained `DFC.Scope.Procedural` (safe: only summonable where
>   Process exists or inside bodies). `:==` remains rejected (no outer writes yet).
> - Function-vs-procedural is structurally derived: a procedural method has NO output port
>   (`DclOut`). `edMethodCheck` allows `Wait` in procedural bodies only; NBAssignment is
>   rejected in both (with a procedural-specific message).
> - **`edMethodCallSiteCheck` (new root-level check, next to `waitCheck`)**: a method that
>   TRANSITIVELY contains a wait (memoized walk through nested method sub-DBs) may only be
>   called inside `process.forever` (`Sensitivity.List(Nil)`) or an `initial` block; nested
>   calls are covered by the outermost call site (method-owned call sites are skipped).
> - Printers: Verilog `task automatic name(input ...); begin ... end endtask` (v95:
>   non-ANSI; parameterless tasks print as `task automatic name;` and are CALLED without
>   parentheses — no `__dummy__` needed, the ≥1-input rule is functions-only). Style: the
>   `begin`/`end` keywords are NOT indented (they align with the `function`/`task` header;
>   only the body inside them gets one indent level); VHDL
>   `procedure name(l : <type>) is ... end procedure;` (no purity keyword — a
>   phantom-reading procedure is simply legal VHDL). Procedural calls print as statements
>   (with `;`), flowing through the existing `csDFMembers` Unit-return-inst path — which
>   needed a `getOrElse` fix: an argument-less Unit call has NO PBNS entry at all.
> - Deferred items from this phase are consolidated in **OPEN ISSUES** above (items 6, 7, 10).
>
> Phase 1 status: **Phases 1A+1B+1C IMPLEMENTED** (frontend, plugin ED path + phantom capture,
> `designFromDefED`, `edMethodCheck`, DFHDL re-emitter with local in-class def placement and
> phantom hiding, `UniqueDesigns` local scoping, `PrepEDDefs`, Verilog/VHDL function
> printing incl. dialect forms + `__dummy__` + sensitivity fixes; specs green:
> `CoreSpec.EDMethodSpec`, `PrintCodeStringSpec`, `ElaborationChecksSpec`,
> `PrepEDDefsSpec`, `PrintVerilogCodeSpec` (sv2005/v95/v2001), `PrintVHDLCodeSpec`
> (v2008/v93) — 481 stage tests green). Phase 1D (sim validation via testApps) pending.
> Implementation findings are recorded inline below marked **[1A/1B FINDING]** and in the
> **[1C FINDINGS]** section.
>
> **[1C FINDINGS]** (backend phase):
> - `DropDesignDefs` was ALREADY DF-only (its match is `domainType = DF, instMode = Def`) —
>   no change needed; ED defs flow through the prep pipeline untouched.
> - **Call inlining came almost free**: the shared printing machinery predates DropDesignDefs
>   and still handles def designs — `csDFValRef`'s `PortOfDesignDef(OUT, inst)` case inlines
>   anonymous call results via `csDFDesignDefInst`, and `csDFMembers` already filters def
>   input-connect nets and read-output anonymous insts. The HDL work reduced to implementing
>   `csDFDesignDefDcl`/`csDFDesignDefInst` per printer + module/architecture decl injection.
> - **Phantom connects/PBNS carry `PhantomTag`** (tagged DFC at `designFromDefED` connect
>   time): phantom detection is local everywhere (printers, DropProcessAll) — no cross-sub-DB
>   member resolution needed.
> - **`PrepEDDefs` (new stage, after ExplicitNamedVars)**: named calls (`val x = f(a)`) have
>   no HDL equivalent — rewritten to VAR + assignment/connection with the inst anonymized.
>   Key discovery: EVERY read of a call result has its own `PortByNameSelect` member —
>   all out-PBNS must be redirected (main kept for the new net; the rest
>   `ChangeRefAndRemove`d).
> - **`ViaConnection` skips ED-method insts** (calls must not be rewired through vias).
> - **`DropProcessAll` fixes**: (a) its dependency walk only collected `Assignment` nets —
>   ED call arguments (explicit AND phantom) are read through input `Connection` nets, now
>   collected via `PortOfDesignDef(IN, _)`; (b) ED call PBNS are excluded from the computed
>   list (a call expression is not a signal); (c) under VHDL-2008/2019 `process(all)` misses
>   impure-function hidden reads, so phantom-carrying processes (detected via tagged nets)
>   are force-converted to explicit lists — v93/v95 processes get captures in their lists
>   automatically via (a). SystemVerilog needs nothing (`always_comb` sees function bodies).
> - **`UniqueDesigns` flat-DB ownership**: `getOwnerDesign` on a design block THROWS in the
>   flat (`newToOld`) DB (design ownership is structural, not in refTable) — owner lookup is
>   built from `designMemberList` child-block scanning.
> - **Verilog forms**: ANSI header + `automatic` for v2001+; v95 non-ANSI input decls; the
>   `logic` keyword stripped for v95/v2001 (same treatment as module params); `__dummy__`
>   input + literal-`0` actual when the printed input list is empty under v95/v2001; return
>   via function-name assignment (portable, no `return`).
> - **VHDL forms**: `impure` iff phantom INs exist; return type prints as a type MARK
>   (constraint stripped); parameterless functions/calls have no parentheses; function
>   locals print as `variable` (via an `isEDMethod` case in the sigOrVar decision); body
>   `return <ret>;` replaces the output-port connect. NOTE: `:=` on an out PORT under VHDL
>   prints invalid `z := ...` — a PRE-EXISTING gap independent of ED methods (VHDL-targeted
>   user code uses `:==`); ED tests follow that idiom.
> - `Printer.getCurrentDesign` is mutated by `csFile` — locally-placed def decls are printed
>   via `csDFDesignDefDcl` directly (never nest `csFile` during a design's own rendering).
> - Deferred items from this phase are consolidated in **OPEN ISSUES** above (items 1, 3, 9).
>   Note (accepted behavior, not a gap): multi-read ANONYMOUS calls print the call per read
>   site — pure, so semantically sound.
> Decisions locked:
> 1. **(REVISED 2026-07-11)** Zero-arg ED functions are **allowed**, but must declare an
>    explicit empty `()` parameter block (a parameterless `def f: T <> EDRET` is rejected —
>    call sites must read as calls). Driving use-case: functions for initialization code
>    (calls in initial positions, mainly meaningful under VHDL) — and every DFHDL feature
>    must compile across all backends, so legacy Verilog (v95/v2001, which require ≥1
>    function input) gets a **printer workaround**: a single dummy input `__dummy__` in the
>    printed declaration and a literal `0` actual at printed call sites (leading underscores
>    are legal Verilog identifiers; VHDL forbids them, but the workaround is Verilog-only).
> 2. **Recursion is forbidden** (direct recursion rejected in the plugin; mutual recursion
>    surfaces as an elaboration stack overflow — documented, not detected).
> 3. New scopes **`Scope.Function`** and **`Scope.Procedural`**, selected by the `<> EDRET`
>    return type (`Unit` ⇒ Procedural, otherwise Function), for the ED domain only.
> 4. ED methods are **`@hw.pure` by default** (meta-programming is not expected in these
>    compatibility-oriented bodies; saves redundant elaboration).
> 5. Outer-scope access is modeled via **phantom members** (`PhantomTag`): the **compiler
>    plugin** captures them — `DesignDefsPhase` statically lifts free DFHDL-typed references
>    in the body into extra phantom arguments of the `designFromDef` call, which constructs
>    tagged `DesignParam`/`Dcl` members and connects them; printers hide them. The method
>    subDB is self-contained from the moment it elaborates, and `@hw.pure` memoization works
>    unchanged (phantoms are ordinary inputs — see §S8).
> 6. Argument modifiers: **functions take `<> VAL`** (+ `<> CONST`) — the same surface as
>    DF/RT design defs; **`<> IN`/`<> OUT` are procedural-only** (Phase 2), where `<> VAL` =
>    copied value (VHDL constant/variable class), `<> IN` = live signal-class formal
>    (waitable, `.rising`/`.falling`), `<> OUT` = signal-class output. Functions remain
>    input-only in any case (portable intersection of all backends).

## Motivation

Verilog/SystemVerilog functions and tasks, and VHDL functions and procedures, are the natural
target for reusable sequential-code helpers (printing/reporting, stimulus, waits, small
combinational computations usable in expressions). DFHDL design defs (`<> DFRET`) already model
the DF equivalent as `InstMode.Def` design blocks; this effort extends the same modeling to the
ED domain with `T <> EDRET`, printed as real HDL subprograms rather than module instances.

## Frontend design

The `<>` match type (`core/src/main/scala/dfhdl/core/DFVal.scala:100-104`) gains a nested match
on `T` for `EDRET`:

```scala
infix type <>[T <: DFType.Supported, M] = M match
  case EDRET => T match
    case Unit => (DFC, DomainType.ED, DFC.Scope.Procedural) ?=> Unit
    case _    => (DFC, DomainType.ED, DFC.Scope.Function) ?=> DFValOf[DFType.Of[T]]
  ...
```

- **Call-site gating** comes from the context-function given demands:
  - `Scope.Procedural` is summonable inside processes (derivation `given (using
    DFC.Scope.Process): DFC.Scope.Procedural`) and inside procedural bodies (the context
    parameter itself) — so tasks/procedures are callable only from processes or other
    procedural methods. NOT summonable from `Scope.Design` — matching HDL (no task calls in
    concurrent context).
  - `Scope.Function` is summonable in every ED-legal context (derivations from
    `Scope.Design`, `Scope.Domain`, `Scope.Process`, `Scope.Initial`, `Scope.Procedural`,
    plus the body's own parameter) — HDL functions are legal in expressions, continuous
    assignments, and processes alike.
- **Type-level scope restriction inside bodies is best-effort only** — as established with
  `Scope.Initial`, scope evidence can be laundered through helper `def`s, and derivation
  givens make `NotGiven`-style guards unreliable. **Elaboration checks are the authoritative
  enforcement** (see checks section). Given-priority behavior of the derivation chain must be
  verified in Phase 1A (ambiguity between e.g. `Scope.Process` given and derived
  `Scope.Function`).
- Both marker traits: `sealed trait Function extends Local` / `sealed trait Procedural extends
  Local` in `DFC.Scope` (`core/src/main/scala/dfhdl/core/DFC.scala:130-149`).

**[1A/1B FINDINGS] — implementation notes:**

- **Ambient scope given mechanics**: `Scope.Function` gets a low-priority ambient given in a
  `ScopeLP` base trait of `object Scope` (call sites summon it anywhere; givens declared
  directly in `object Scope`, e.g. `Global`, win generic `Scope` summons). Because it is
  ambient, `Scope.Function` must NEVER appear in a positive `AssertGiven` union or a
  `NotGiven` guard — it would trivially pass/fail everywhere. It is only usable as (a) the
  context-function parameter (lexically innermost inside bodies) and (b) an `A <:<
  DFC.Scope.Function` *declaration-site* check — this is how `InsideProcess:=` admits `:=` on
  function-body locals (the var's access type gets `Scope.Function` intersected at
  declaration). For the same reason `Function` does NOT extend `Local` (an ambient `Local`
  would break the "declarations cannot be global" guard). Consequence: `print/assert` inside
  a *top-level* ED function def fall back to Scala's Predef (exactly like top-level DF design
  defs today); inside design-class defs they work via the class's `Scope.Design` given.
- **Plugin-reported errors are untestable via `assertCompileError`** (`typeCheckErrors` stops
  at the typer; plugin phases never run). The three DesignDefsPhase errors (missing `()`
  block, recursion, procedural-unsupported) were verified manually via a scratch compile.
  Also, a `Unit <> EDRET` def whose body uses `:=` hits the typer error first (`:=` not
  allowed under `Scope.Procedural` yet), so the plugin's procedural error only surfaces for
  bodies that type-check.
- **Const captures ARE plugin-lifted into explicit phantom `DesignParam`s (user-corrected,
  2026-07-11)**: an early implementation relied on the elaboration auto-created-param
  mechanism (`getReachableNamedValue`/`cloneUnreachable`) for captured constants. That was
  wrong — implicit parameter access is a reachability *by-product*: the auto param is
  untagged (it leaked into printed def signatures/call sites as if it were an explicit
  `<> CONST` arg, breaking hiding and roundtrip), and its applied-value recovery relies on a
  creation entry that does not exist on a `@hw.pure` cache hit. The plugin now lifts
  captured constants explicitly: `genContainerPhantomParam` creates a `PhantomTag`-tagged
  `DesignParam`, the applied value flows per call through `constArgs`/`paramMap` exactly
  like explicit const args (memoization-sound), and printers hide it (DFHDL: filtered from
  the def signature and inst param list; Verilog/VHDL: no print — body references resolve to
  the captured constant's name at module/architecture scope).
  Plugin lessons from this fix: (a) a member with an EXPLICIT `<> ...` type annotation
  carries the unreduced match-type alias on its TermRef — `widen` before the
  `dfValTpeOpt`/`isDFConst` tests, or the capture silently misses it (inferred-type members
  resolve concretely, masking the bug); (b) captured-reference meta positions must come from
  `t.symbol.srcPos`, not `t.srcPos` (references may originate from inlined library code,
  e.g. Exact conversions); (c) printer collects over def-design members must exclude
  `DesignParam` BEFORE `DclConst()` — the extractor also matches design params (the module
  printers all do this).
- **Def calls inside processes work as-is**: the def design block + input connects + inst
  elaborate inside the process block and pass all existing checks — no check relaxation was
  needed.
- **Capture keying**: captures are keyed by full stable path (`List[Symbol]`), roots: `this`
  (enclosing container member) captured; statics (globals) never captured (reachable
  everywhere); the def's own params/body-locals excluded by owner-chain test.

**Plugin** (`plugin/src/main/scala/plugin/DesignDefsPhase.scala`):

- Extend the trigger beyond the current DF path: recognize the ED context-lambda shapes above.
  Note the current trigger requires `dfValArgs.nonEmpty` **and** a DFHDL-value return — both
  conditions need widening for ED methods: zero-DFHDL-arg functions/tasks are legitimate
  (decision 1), and procedural (`Unit`) methods have no DFHDL-value return. ED methods must
  declare an explicit (possibly empty `()`) term parameter block — a parameterless
  `def f: T <> EDRET` (empty `paramss`, no `()` clause) is a plugin error.
- `<> CONST` args → `DesignParam` (unchanged mechanics); `<> VAL` args → `IN` ports —
  functions reuse the DF-def argument surface and `prepareForValDef` path unchanged.
  Type-level `<> IN`/`<> OUT` match-type cases do **not** exist in Phase 1 (using them on a
  def arg fails to compile naturally); Phase 2 adds them for procedural methods only —
  `<> IN` expanding like `VAL` but marking a live signal-class formal, `<> OUT` expanding to
  an assignable `DFVarOf[DFType.Of[T]]`.
- **Phantom capture (decision 5)**: collect the body's free DFHDL-typed references — idents
  and *stable* selects (`this.sig`, `outer.inner.x`) whose symbol is declared outside the def
  — dedup by symbol/path, and append them to the arg lists passed to `designFromDef` as
  separate phantom lists. Key property: the def body IS the `designFromDef` call, so the
  captured refs are evaluated *inside the def at call time*, in the caller's elaboration
  context — **call sites are never transformed**, and defs compiled in other files/libraries
  need no signature knowledge. Body references to captures are rewritten to
  `designFromDefGetInput(i)` exactly like explicit args. Split by type shape as with explicit
  args: `ISCONST[true]`-typed captures → phantom `DesignParam`; other `DFValOf` captures →
  phantom `IN` `Dcl` (Phase 2 adds LHS-position analysis to classify captured assignment
  targets as phantom OUT). Phantom names derive from the captured val's name, uniquified
  against explicit args.
- Reject **direct recursion** (body calls its own symbol) with a clear error (decision 2).
- Apply `@hw.pure` by default for ED methods (decision 4) — i.e. the generated
  `designFromDef` path behaves as if `annotation.Pure` is present unless the user opts out
  (an explicit `@hw.pure(false)`).

## IR modeling

- An ED method is a **`DFDesignBlock` with `instMode = InstMode.Def` and
  `domainType = DomainType.ED`** — no new `InstMode` variant. The function/procedural flavor
  is structurally derivable: a function def has the single output port `o` (the existing
  `Unit`-return path in `designFromDef` already omits it), so
  `isDefFunction = instMode == Def && domainType == ED && hasOutPort`.
- **`PhantomTag`**: new `case object PhantomTag extends DFTag` in
  `compiler/ir/src/main/scala/dfhdl/compiler/ir/DFTags.scala`. Attached to auto-constructed
  `Dcl` ports and `DesignParam`s that materialize captured outer members. The caller-side
  connect `DFNet` needs no tag — printers derive hiding from the phantom-tagged endpoint.
- `designFromDef` (`core/src/main/scala/dfhdl/core/r__For_Plugin.scala:135-178`) gains an ED
  variant: `domain = ir.DomainType.ED`, same input-port/output-port/paramMap mechanics, plus
  separate phantom arg lists (from the plugin's capture lift) that produce
  `PhantomTag`-tagged `Dcl`s/`DesignParam`s.

## Language survey (what the backends can and cannot express)

Verified capability matrix:

| Capability | Verilog func | SV func | VHDL func | Verilog/SV task | VHDL procedure |
|---|---|---|---|---|---|
| Consume time (`wait`, `#`, `@`) | ✗ | ✗ | ✗ | ✓ | ✓ |
| OUT/INOUT args | ✗ | ✓ (then banned from expression contexts) | ✗ (in-mode only) | ✓ | ✓ |
| Assign signals outside itself | ✓ (legal, ugly) | ✓ | ✗ (**never**, even visible ones) | ✓ | ✓ (driver rules below) |
| Call the other kind | ✗ (func can't call task) | ✗ | ✗ (nor a wait-ing procedure) | ✓ | ✓ |
| Usable in expressions / concurrent assigns | ✓ | ✓ | ✓ | ✗ | ✗ (VHDL has concurrent procedure *calls*) |
| Zero args | ✗ (≥1 input) | ✓ | ✓ | ✓ | ✓ |
| Recursion | automatic only | automatic only | ✓ | automatic only | ✓ |

### S1 — The function/procedural split is a three-way semantic contract

- Non-`Unit` `EDRET` must be **IN-only**: Verilog-2005 and VHDL functions disallow out-mode
  params; SV allows them but bans such functions from expression/continuous-assign contexts.
  Want outputs + return? Return a struct, or use a procedural method with an OUT arg.
- Function bodies must be **side-effect-free on outer state**: VHDL functions cannot contain
  signal assignment at all — so phantom members on a *function* are IN-only (phantom reads are
  what makes a VHDL function `impure`). Enforce the VHDL rule as the DFHDL rule.
- Functions must not call procedural methods (both language families forbid it) — enforced by
  the scope givens (no `Scope.Procedural` derivable from `Scope.Function`) + elaboration
  backstop.
- `Scope.Procedural` alone is not fine-grained enough for `wait`: VHDL hard-errors on calling
  a wait-containing procedure from a process with a sensitivity list. Elaboration check
  (Phase 2): a method whose subDB transitively contains `ir.Wait` may only be called from
  forever/initial-style processes (`Sensitivity.List(Nil)` / `Sensitivity.Initial`).

### S2 — Argument-passing semantics diverge once time passes

- Verilog/SV task args are **copy-in/copy-out** (outputs visible to the caller only at
  return); VHDL **signal-class** formals are *live* (the formal's driver IS the caller's
  driver — assignment takes effect at the `<=`, across waits); VHDL variable-class formals are
  copy-like; SV `ref` (automatic only, sv2005+) gives live semantics.
- The phantom connect-and-hide model is inherently *live* — matches VHDL signal params, and
  matches Verilog too **when phantoms print as hidden side-band accesses to module-scope
  signals** (legal in tasks). Phantoms are therefore semantically safe.
- The divergence bites on **explicit OUT args of a waiting task** (Verilog copy-out-at-return
  vs VHDL live). For non-waiting tasks the two converge observably. Phase-2 rule: *methods
  containing `wait` may not have explicit OUT args* (outer writes go through `:==`/phantoms).
  Possible later relaxation: SV `ref` on sv-dialects.
- **VHDL parameter class is declared for inputs, inferred for outputs** (decision 6): input
  class comes from the frontend modifier — `<> VAL` = constant/variable-class copied value,
  `<> IN` = signal-class live formal (so `.rising`/`.falling`/waiting on a `<> VAL` arg is an
  elaboration error, pointing the user to `<> IN`). OUT class is inferred from the assignment
  op: `:==` → signal class; `:=` → variable class (whose actual must be a variable). Mixing
  `:=` and `:==` on the same OUT arg = elaboration error. Verilog doesn't care about classes,
  but the declared/inferred class still picks the printed lowering.
- VHDL non-formal signal assignment from a procedure is only well-defined when the procedure
  is **declared inside the calling process** (the driver belongs to that process);
  architecture-level procedures must take signals as formals. Gives the VHDL printer two
  lowerings for phantom-OUT: promote phantoms to real signal formals per call site, or
  declare the procedure in the process declarative part (keeps phantoms hidden, duplicates
  the body per process).

### S3 — Sensitivity lists don't see hidden reads inside functions

IEEE-documented: `always_comb` is sensitive to signals read *inside* a called function's body;
`always @*` is sensitive only to the function's *arguments*. A function with hidden phantom-IN
reads called from a combinational process silently breaks under `v95`/`v2001` `@*` and under
VHDL-93 explicit lists. Since the compiler knows the phantoms: **when printing explicit
sensitivity lists (and `@*`-era dialects), add the phantom-read signals of every transitively
called function**. Must be tested the moment functions land (Phase 1C).

Relatedly: the VHDL printer must emit **`impure function`** exactly when phantom INs exist.
Three unrelated "purity" notions are now in play — `@hw.pure` (elaboration memoization), VHDL
`pure/impure` (reads beyond params), and side-effect-freedom of function bodies. Keep internal
naming distinct.

### S4 — Lifetime: always emit `automatic`

Verilog subprograms are **static by default** (concurrent calls share one set of arg/local
storage; recursion broken). Printers emit `automatic` unconditionally (available v2001+).
`v95` has no `automatic`: harmless for functions (no waits ⇒ calls complete atomically within
a timestep in practice), but Phase 2 must forbid (or document) reentrancy of waiting tasks in
v95. Local `VAR`s inside method bodies get **fresh-per-call, re-initialized** semantics — the
only portable choice (VHDL variables and SV automatic locals both behave this way).

### S5 — Genericity and emission placement: monomorphize for Verilog

- Verilog/SV have **no parameterized subprograms** (outside classes). Width-generic methods
  are either monomorphized (one printed function per distinct param combination, mangled
  names) or emitted **inside each calling module**, where phantom `DesignParam`s resolve to
  that module's parameters — the latter falls out of the phantom design and is Verilog's
  native idiom. Package-level sharing only for phantom-free, fixed-width methods (later).
- VHDL: unconstrained array parameters (since '93) cover width-genericity without
  monomorphization; VHDL-2008 generic subprograms exist but tool support is patchy (GHDL
  gaps) — unconstrained params are the safer lowering. Phase 1 monomorphizes for both
  backends (widths baked per instantiation); unconstrained-param sharing is a later
  optimization.
- **Name mangling is mandatory for Verilog** (no overloading) and used for VHDL too (Scala
  monomorphization already yields distinct bodies). Scala provides overloads/default/named
  args in the frontend for free; printers expand defaults at call sites (v95/v2001 lack
  default-arg support).
- **Name-uniqueness scope is the owning design (user decision, 2026-07-11)**: ED methods are
  locally scoped — printed inside their owning design — so `UniqueDesigns` must NOT apply its
  global dclName collision renaming across designs (same-named functions in different designs
  would get pointlessly renamed). Implemented (Phase 1B): `UniqueDesigns.scopedDclNameKey`
  keys ED methods by `ownerDclName::dclName` — cross-design same-name methods never meet
  (no rename, no cross-owner structural unification — harmless since each owner prints its
  own copy), while same-scope overloads still get suffixed. The DFHDL re-emitter likewise
  prints ED method defs at the top of the owning design class body (`Printer.designPrinters`
  excludes them from standalone emission; `Printer.edMethodPrinters` + `csDFDesignBlockDcl`
  place them locally), which also makes phantom-hidden body references (`l + b`) valid Scala
  on roundtrip.

### S6 — Dialect-specific paper cuts

- Zero-arg functions illegal in v95/v2001 (≥1 input) — printer workaround per decision 1:
  emit `input __dummy__;` in the function declaration and pass a literal `0` at every call
  site. Applies whenever the *printed* input list is empty — i.e. no explicit args
  (phantoms are hidden and don't count, per the phantom-hiding rule).
- v95/v2001 function **return types** limited to integral/real/vector — struct/vector returns
  need the existing flattening treatment there; same for unpacked-array *arguments* (v2001 ✗,
  SV/VHDL ✓).
- `return`/early exit: SV & VHDL have it, v95 needs `disable` hacks — not supported (matches
  the project's no-early-return Scala convention; Scala last-expression is the return).
- Recursion: forbidden (decision 2) — also sidesteps subDB self-reference and v95.

### S7 — Procedural whitelist (existing coverage + gaps)

Already modeled: `TextOut` (`print/println/report(severity)/assert(severity)/debug/finish`,
mapping to `$info/$warning/$error/$fatal` ↔ `report/assert severity`), `Wait`
(delay/condition/endless). Note **VHDL functions may contain `report`/`assert`** and Verilog
functions may `$display` — TextOut is legal inside *function* bodies too.

Gaps worth adding later as companion features (all sim-only): `$time`/`now` as a readable
value; `$stop` (breakpoint) alongside `Finish`; `$random`/`$urandom` ↔ VHDL `uniform`; file
I/O (`$fopen/$fdisplay/$readmemh` ↔ `textio`); string-typed values crossing the def boundary
as args (needed for reusable report helpers). Out of scope permanently (for this feature):
`fork/join`, named events, class/DPI features (a DPI/VHPI-backed ED method could later reuse
`EDBlackBox.Foreign`), VHDL resolution functions, constant functions (Scala meta owns
elaboration-time computation), VHDL concurrent procedure calls (auto-wrap in a process if
ever needed).

### S8 — Interactions with existing machinery

- **`@hw.pure` memoization vs phantom capture — SOLVED by plugin capture (decision 5).**
  The cache in `MutableDB.runFuncWithInputs`
  (`core/src/main/scala/dfhdl/core/MutableDB.scala:243-262`) is keyed by
  `(source position, input DFTypes)` — identity-blind. Had captures been discovered at
  *elaboration* time (by running the body), a memoized (skipped) body could never reveal what
  call site #2 captured. Plugin lifting makes captures **ordinary inputs**: port creation and
  caller-side connection happen per call *before/outside* the memoized body run, so a cache
  hit still connects call site #2's own captured members, and phantom DFTypes participate in
  the cache key for free. No capturing/re-run carve-out; memoization is uniform.
  Residual hazards to document/enforce:
  - **Helper-def hole**: a plain Scala helper `def` (declared outside the ED method,
    `using DFC`) called in the body can reference outer members from *its* AST — invisible to
    the plugin's lexical scan of the ED def body. Such refs surface at elaboration as
    cross-boundary refs → **elaboration error** (same as DF design defs today; message should
    say "reference it directly in the ED method body or pass it as an argument").
  - **Plain-Scala captures** (`Int`, `Boolean` fields that are not `<> CONST`) remain silent
    closure captures — under default purity, two instances differing only in such a field
    would wrongly share one memoized body. Pre-existing DF-design-def hazard, now
    default-on: document prominently; a plugin warning on non-DFHDL free refs of
    non-singleton types is a possible future hardening.
  - **Static over-capture**: a lexical ref in a meta-dead branch (Scala-level `if` on an
    elaboration constant) still produces a phantom port + connect even though the elaborated
    body never reads it. Structure stays consistent across instances (good for dedup);
    unused phantom ports are pruned in `PrepEDDefs` (Phase 1C).
- **Elaboration checks** (authoritative, `DB.check` style like `initialCheck`): method subDBs
  must contain no sub-design instantiation (`InstMode.Normal` etc. — a hard printing wall),
  no process blocks, no `Step`/goto members; function subDBs additionally: no `ir.Wait`, no
  writes to phantoms/outer state, no procedural-method instances. Multi-driver check when a
  phantom-OUT signal is written by a method called from two processes (each call materializes
  a driver in its caller) — Phase 2.
- **`DropDesignDefs`** (`compiler/stages/.../DropDesignDefs.scala`) currently erases *every*
  `InstMode.Def` design into a `Normal` design + instances. It must be restricted to
  `domainType == DF` defs; ED defs flow through to the backends on a new printing path
  (backends today never print user subprograms — only internal helper functions).
- **Call-site printing**: an ED function call elaborates as design-inst + input connects +
  output-port read; HDL needs a call *expression*. The DFHDL re-emitter already renders def
  instances as calls (`csDFDesignDefInst`, `compiler/ir/.../printing/DFOwnerPrinter.scala`)
  — mirror that mechanism in the Verilog/VHDL printers: inline `f_name(actual1, ...)` at the
  (single) output-port reference; suppress the inst and connects.
- **Per-module phantom-actual grouping**: hiding a phantom means the printed body references
  the *actual's* name directly — so one printed function can serve only calls sharing the
  same phantom-actual binding. Within one module this is the norm (a def on the design class
  captures `this.sig` identically at every call in that design); the printer must still group
  method instances by (structure, phantom-binding) per module and uniquify names when
  multiple groups exist.

Survey sources: Accellera on `always_comb` vs `@*` function sensitivity
(<https://accellera.org/images/eda/vlog-pp/0385.html>), Sigasi wildcard sensitivity
(<https://www.sigasi.com/tech/wildcard_sensitivity/>), VHDL signal-parameter driver rules
(<https://peterfab.com/ref/vhdl/vhdl_renerta/mobile/source/vhd00063.htm>), Doulos VHDL-2008
enhancements (<https://www.doulos.com/knowhow/vhdl/vhdl-2008-major-enhancements/>), GHDL
generic-subprogram support gaps (<https://github.com/ghdl/ghdl/issues/2383>).

## DFHDL semantics (chosen rules)

**Functions** (non-`Unit` `<> EDRET`):

- Zero or more explicit `<> VAL` DFHDL-value arguments, with a mandatory (possibly empty
  `()`) parameter block (decision 1); `<> CONST` params allowed; no `<> IN`/`<> OUT`
  (procedural-only, and OUT is impossible for functions anyway).
- Body: local `VAR`s (fresh per call) with `:=`, control flow, TextOut, calls to other
  functions. No `wait`, no `:==`, no writes to outer/phantom state, no processes, no
  sub-designs, no procedural calls, no recursion.
- Phantom **IN** capture allowed (⇒ VHDL `impure`, sensitivity-list participation per S3).
- Callable anywhere in the ED domain (design/concurrent scope, processes, initial blocks,
  other method bodies).
- `@hw.pure` by default; capture-free calls memoize, capturing calls re-elaborate (S8).

**Procedural methods** (`Unit <> EDRET`) — Phase 2, semantics recorded now:

- Explicit `<> VAL` (copied value), `<> IN` (live signal-class formal — waitable,
  `.rising`/`.falling`), and `<> OUT` args; zero-arg allowed; phantom IN and OUT capture
  allowed.
- Body: everything a process body allows minus steps/gotos — waits, `:==`, TextOut, function
  and procedural calls. No processes, no sub-designs, no recursion.
- Callable only under `Scope.Procedural` (processes / other procedural bodies).
- Waiting methods: no explicit OUT args (S2); callable only from forever/initial processes
  (S1).
- VHDL parameter-class inference per argument usage (S2).

## Phase 1 — function support

### 1A — Frontend + plugin + IR + elaboration (no phantoms yet)

- `Scope.Function` + `Scope.Procedural` traits and derivation givens in `DFC.Scope`
  (Procedural declared but unused until Phase 2; verify given-priority/ambiguity behavior of
  the derivation chain with a compile test).
- `<> EDRET` match-type nesting per the frontend design above. `Unit <> EDRET` compiles but
  the plugin errors "procedural ED methods are not yet supported" (keeps the surface honest
  until Phase 2).
- `DesignDefsPhase`: ED trigger recognition (widened for zero-DFHDL-arg defs); error on a
  missing term parameter block (parameterless `def f: T <> EDRET` — empty `()` is required,
  decision 1); direct recursion error; default-`Pure` annotation injection for ED defs.
- `designFromDef` ED variant (`domain = ED`, `instMode = Def`); `isDefFunction` analysis
  helper (e.g. in `ProcessBlockAnalysis.scala` or a new `DesignDefAnalysis`).
- Elaboration checks (new `edMethodCheck()` in `DB.scala` `subDBCheck`, `initialCheck` as the
  template): function-subDB content rules from the semantics section (waits, `:==`,
  processes, sub-designs, outer writes, procedural members). Recursion backstop: a def design
  transitively containing an instance of its own structure is unreachable if the plugin check
  holds — document, don't detect.
- DFHDL re-emitter: `csDFDesignDefDcl`/`csDFDesignDefInst` extended for ED defs (`<> EDRET`
  modifier rendering).
- Tests: `core/CoreSpec` compile-guard errors (missing-`()`-param-block, recursion,
  wait-in-function, `:==`-in-function, procedural-not-supported, calling from non-ED domain)
  plus a zero-arg `def f(): T <> EDRET` positive case,
  `StagesSpec.PrintCodeStringSpec` roundtrip, `lib/ElaborationChecksSpec` for the laundering
  backstops.

### 1B — Phantom IN capture (plugin-side)

- `PhantomTag` in `DFTags.scala` (IR format bump ⇒ DiskCache invalidation via version tag).
- **`DesignDefsPhase` capture lift** (the frontend-design section has the mechanics): free
  DFHDL-typed stable references → extra phantom arg lists on the `designFromDef` call;
  body refs rewritten via the existing `inputMap` mechanism; const-typed captures → phantom
  `DesignParam` entries. No call-site transformation; cross-unit safe by construction.
- **`designFromDef` ED variant** accepts the phantom lists: declares `PhantomTag`-tagged `IN`
  `Dcl`s / `DesignParam`s (names from the captured vals, uniquified), connects them in the
  caller's scope like explicit args. `runFuncWithInputs` needs no changes — phantoms ride the
  existing input path and the pure cache key picks up their DFTypes automatically (S8).
- Elaboration backstops: **residual cross-boundary refs = error** (the helper-def hole, S8 —
  matches today's DF-def behavior, improve the message); a body *write* to a phantom on a
  function = error (the "VHDL functions can't assign signals" rule).
- Tests: capture of a design signal / a design param / stable-select capture
  (`this.io.x`-shaped); two instances of the same enclosing design (identical method subDBs +
  per-instance connects + **pure-cache hit despite different captured members** —
  elaboration-count assertion if feasible); meta-dead-branch over-capture (phantom exists,
  pruned later in 1C); helper-def cross-boundary error; capture-write error.

### 1C — Backends

- `DropDesignDefs`: restrict to `domainType == DF` (one-line predicate + spec).
- New backend-prep stage (use `/new-stage`; working name `PrepEDDefs`): monomorphization
  grouping — per printing module/architecture, group ED-def instances by (design structure,
  phantom-actual binding, applied param values); assign mangled unique names; **prune
  phantom ports/params with no body references** (static over-capture, S8); verify/flatten
  dialect constraints (v95/v2001: no unpacked-array args, integral/vector return only — reuse
  the existing flattening treatment or `printer.unsupported`).
- **Verilog printer** (`VerilogOwnerPrinter`/`VerilogValPrinter`): emit
  `function automatic <ret> <name>(input ...);` (plain `function` for v95) in the module
  declarative region, one per group; body statements reuse the process-body printing path;
  phantom Dcls/params print nowhere — body refs render the *actual's* name (module signal /
  module parameter); call sites render inline `name(actuals...)` at the output-port
  reference, inst + connects suppressed. Zero-input functions under v95/v2001 get the
  `__dummy__` input + literal-`0` actual workaround (decision 1, S6) — dedicated
  `PrintVerilogCodeSpec` v2001 test.
- **VHDL printer** (`VHDLOwnerPrinter`): emit in the architecture declarative part;
  `impure function` iff phantoms exist; monomorphized widths (no unconstrained params in
  Phase 1); same call-site inlining.
- **Sensitivity lists** (S3): explicit-list processes and `@*`-dialect rendering add the
  phantom-read signals of transitively called functions. Dedicated
  `PrintVerilogCodeSpec` v2001 test.
- Tests: `PrepEDDefsSpec` (grouping, mangling, dialect gates), `PrintVerilogCodeSpec`
  (sv2009 + v95 end-to-end incl. an impure-style capture), `PrintVHDLCodeSpec` (v93 + v2008,
  `impure` keyword, sensitivity), docExample + `docExamplesRefUpdate`.

### 1D — Validation + docs

- `testApps`-level simulation check: a design using an ED function (with a phantom capture)
  simulated across available tools/dialects.
- Update `.claude/commands/ir-reference.md` (PhantomTag, ED `InstMode.Def` semantics,
  `isDefFunction`) and `.claude/commands/new-stage.md` with any general lessons.
- Verification ladder throughout: individual specs → `testOnly StagesSpec.*` → full `test`.

## Later phases (outline only)

- **Phase 2 — procedural methods**: `Unit <> EDRET` enabled; `Scope.Procedural` call-site
  gating live; `<> IN`/`<> OUT` match-type cases added (OUT expands to an assignable
  `DFVarOf`); phantom OUT; wait-containing-method call-site check
  (forever/initial only); no-OUT-args-on-waiting-tasks rule; declared VAL-vs-IN input classes
  + OUT class inference (`:=` vs `:==`) per S2; VHDL emission placement choice (process-declared for
  hidden phantom-OUT vs promoted signal formals); Verilog `task automatic`; multi-driver
  phantom-OUT elaboration check; v95 reentrancy documentation.
- **Phase 3+ (unordered)**: package/shared emission for phantom-free methods; VHDL
  unconstrained-param lowering instead of monomorphization; SV `ref` relaxation for waiting
  tasks; INOUT args; `$time`/`$stop`/random/file-I/O companion features; string args across
  the def boundary; DPI/VHPI-backed methods via `EDBlackBox.Foreign`.

## Sequencing

| Phase | Content | Gate |
|---|---|---|
| 1A | Scopes, `EDRET` match type, plugin ED path, `designFromDef` ED, elaboration checks, DFHDL printer | core compile-guard tests + PrintCodeString + elab checks |
| 1B | `PhantomTag`, plugin capture lift, `designFromDef` phantom lists, elaboration backstops | capture specs + pure-cache-hit-across-instances regression |
| 1C | `DropDesignDefs` restriction, `PrepEDDefs`, Verilog/VHDL function printing, sensitivity fix | stage spec + printer specs (sv2009/v95/v93/v2008) + ref update |
| 1D | sim validation + docs/skill updates | testApps case |
| 2 | procedural methods (tasks/procedures) | — |
| 3+ | sharing/genericity/companions | — |
