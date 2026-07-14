# The Static Domain and Static Functions

Status: plan, not yet implemented.

## 1. Motivation

DFHDL models constants in several disconnected places today:

1. The **global scope** allows only constants (no variables). It is enforced by a bespoke
   `DFC.Scope.Global` guard, and a global member is modeled as an *ownerless* member: `isGlobal`
   is literally "my `ownerRef` is `Empty`" ([DFMember.scala:418]).
2. **For-loop iterators** are scope-limited variables that are treated as constants.
3. **Initial blocks** restrict assignments to constant sources, but could reasonably host
   variables that mutate only from constants.

What is missing is the ability to model a **function that returns a constant**, including a
function whose body mutates local variables, as long as every value flowing through it is
constant. The mutation is bounded by the function body, so from the outside the result is a
constant.

The unifying observation: all four cases are the same thing, a region in which **nothing depends
on time**. Constness is a *value* property; what these cases share is a *region* property. We
call that region property **static**.

## 2. The two axes

Keep them separate, and keep them separately named:

| Axis | Question | Vocabulary |
|---|---|---|
| Value | is this value's data known (or at least fixed) at elaboration? | `<> CONST`, `isConst`, `ConstData` |
| Region | does time advance here at all? | `DomainType.Static` vs `DomainType.Dynamic` |

A `CONST` value can live in any domain. A `Static` domain is a region where *every* value is
constant. Naming both `CONST` would collapse a distinction we rely on.

`Static` is not a fourth level of abstraction. DF, RT, and ED remain "the three levels" (the
dynamic domains, where values change over time). `Static` is the degenerate bottom of the lattice,
the domain that globals have always been in without our having said so.

VHDL precedent: "locally static" and "globally static" expressions mean exactly "evaluable at
analysis/elaboration time", so the word reads correctly to HDL users on first contact.

## 3. Locked decisions

1. **IR**: `DomainType` becomes a lattice, `Static` and `Dynamic.{DF, RT, ED}`.
2. **Core**: `DomainType` gains `Static` *and* a `Dynamic` supertype (see 5.1: the `Dynamic` layer
   is required, not cosmetic). `Static` becomes the ambient default given, replacing the current
   ambient `given DF`.
3. **Scope**: no new scope type. A static function body is a `DFC.Scope.Function` body, exactly like
   an ED function. The *function-region* restrictions come from the scope; the *static-region*
   restrictions come from the domain. `DomainType.Static` is what discriminates the two.
4. **Declaration**: a static function is declared by `T <> CONSTRET`, which expands to
   `(DFC, DomainType.Static, DFC.Scope.Function) ?=> DFValOf[DFType.Of[T]]`. A `Unit` return type
   (the task/procedure indicator) is disallowed with `CONSTRET`, enforced in the plugin.
5. **Arguments**: a static function's DFHDL arguments must all be `<> CONST`, enforced in the
   plugin. They become **design parameters** of the def design, *not* input ports (§5.6a: a port arg
   is wired by a net at the call site, and a static function must be callable from global scope,
   where there is no `Concurrent` capability and no block to own a net). Captured outer constants
   remain **phantom design parameters**, as they are for ED methods today, so *every* constant input
   to a static function is a design parameter and the phantom/explicit split is only about
   visibility in the printed signature. Captured non-constants are rejected.
5a. **Printing**: for a def design, non-phantom **design parameters and input ports print
   identically, as subprogram formals**, in one formal list. A def design never prints generics (a
   VHDL or Verilog subprogram has no generics). So a static function prints as a subprogram whose
   formals are its params, and an ED method as one whose formals are its ports; the two paths differ
   only in which member kind supplies the list. See §5.6a for the body-sharing rule this requires.
6. **Static ports**: inside a static function's def design, ports and variables carry constant
   data, known or unknown. This is what makes the return value constant at the call site.
7. **Body scope**: arithmetic and logic operations, `if`/`match`, `for`/`while` loops, `<> VAR`
   declarations (non-`REG`), `<> CONST` declarations, reads of captured constants and globals, and
   calls to other static functions.
8. **Purity**: a static function is **pure by definition**, not by opt-in. This is *enforced*, and it
   needs no new analysis: the existing [PureCheckPhase.scala] already computes the verdict for every
   def. The only change is that for a static def an impure verdict is an **error** rather than the
   advisory "never cache" marking it is today. Phantom constants do **not** make a function impure
   (§8.1). See §6.5.

## 4. IR changes (`compiler/ir`)

### 4.1 `DomainType` becomes a sealed lattice

[DomainType.scala] is currently a flat `enum` of `DF | RT | ED` deriving `CanEqual` and upickle's
`ReadWriter`. Replace with:

```
DomainType
├── Static
└── Dynamic
    ├── DF
    ├── RT
    └── ED
```

`Dynamic` must be a real sealed subtype, not a marker. Every existing `DomainType` site is asking a
*timing* question (does this owner need clk/rst, is `.prev` or `.reg` the right history operator,
is a process legal here), and there are around 90 of them across 34 files. Those sites should take
`Dynamic` and stay exhaustive over three cases without change. Only the sites that legitimately
take a bare `DomainType` are forced to decide what `Static` means, and the compiler will point at
them. Adding a flat fourth case would instead let existing `case _ =>` fall-throughs swallow
`Static` silently, which is the bug class to avoid.

Serialization note: the DB is pickled for the elaboration cache, so the `ReadWriter` must be
hand-written for the sealed form, and existing caches invalidate. Not a blocker, but do not
discover it mid-migration.

### 4.2 Domain predicates

[DFMember.scala:132-140] gains `isInStaticDomain` / `isInDynamicDomain` alongside `isInDFDomain`,
`isInRTDomain`, `isInEDDomain`.

Do **not** name the predicate a bare `isStatic`: the plugin already uses dotty's `Symbol.isStatic`
with an entirely different meaning ("top-level Scala symbol", relied on by the capture-root test in
[CapturePhase.scala]). Same word, two meanings, one codebase.

### 4.3 Globals move into the Static domain (phase 2, see §10)

A global today is an ownerless member ([DFMember.scala:418]), so `getOwnerDomain` is undefined for
it. Retrofitting globals into a real `Static` domain owner makes `getOwnerDomain` total and lets the
global scope inherit the static domain's legality rules instead of maintaining its own. This is the
largest single chunk of the work (a couple of dozen sites in `DB.scala` key on the empty-owner
sentinel), so it is deliberately sequenced *after* static functions, which do not depend on it.

### 4.4 Static ports carry constant data

`DFVal.Dcl` is currently opaque to the const lattice: a port resolves to `NotConst`. In a `Static`
domain, a `Dcl` must resolve constant data the way `DesignParam` already does
([DFMember.scala:507-562]):

- through the design instance's connection to the port, giving `KnownConst(data)` when the actual is
  known;
- otherwise `UnknownConst(this)`, because a subprogram formal is a constant of unknown value.

Consequence to audit: `isConst` becomes **true** for a static function's formal ports. Any stage
that folds, hoists, or drops const `Dcl`s must be checked against that, or a static function's
formals will be constant-folded out from under it.

Folding a static function *call* to a known value means evaluating the body with formals bound to
actuals, which needs a small interpreter over the static body (straight-line statements, static
variables, statically bounded loops). **This is deliberately deferred.** DFHDL types are sized by
Scala, so a static function cannot size a type in any case, and `UnknownConst` is sufficient to keep
the generated HDL parametric, which is the primary payoff. See §10.

### 4.5 Subprogram predicate

[DFMember.scala:1756] defines `isEDMethod` as `instMode == Def && domainType == ED`. Add
`isStaticFunction` as `instMode == Def && domainType == Static`, and introduce
`isHDLSubprogram = isEDMethod || isStaticFunction` for the many sites that mean "prints as a
subprogram rather than an instance" ([DFOwnerPrinter.scala:236], [Printer.scala:169,218],
[PrepEDDefs.scala], [DropDesignDefs.scala]). Audit each `isEDMethod` use and decide which of the two
it meant.

## 5. Core changes (`core`)

### 5.1 `DomainType` opaque hierarchy, and the ambient given

[DomainType.scala] currently has `opaque type DF | RT | ED <: DomainType`, with **`given DF`** as the
only ambient given (contrary to earlier belief that there was none). Add:

```scala
opaque type Dynamic <: DomainType = DomainType
opaque type DF <: Dynamic = Dynamic   // and RT, ED
opaque type Static <: DomainType = DomainType
given Static = ir.DomainType.Static   // replaces `given DF`
```

Flipping the ambient given from `DF` to `Static` is safe on the `DFRET` side, because `DFRET` already
injects `DomainType.DF` explicitly ([DFVal.scala:101]) rather than leaning on the ambient. Design
bodies get their domain given from `DomainContainer` ([Design.scala:371-375],
[Container.scala:29]), which is lexical and therefore wins over the companion-scope ambient. The
ambient's only real consumer is the global scope, which is exactly where `Static` is wanted.

### 5.2 The negative guards, which will silently break

This is the highest-risk item in the plan. Several domain guards are phrased *negatively*, and with
`Static` in existence they admit it:

- `.prev` / history: `util.NotGiven[A <:< DomainType.ED]` ([DFVal.scala:1700, 1713, 1718])
- `Fork`: `NotGiven[A <:< DomainType.DF]` ([Fork.scala:28])
- `Process`: `NotGiven[A <:< DomainType.DF]` ([Process.scala:42, 54])

With `A = Static`, `NotGiven[Static <:< ED]` **succeeds**, so `x.prev` would become legal inside a
static function body. Each of these needs a positive `A <:< DomainType.Dynamic` conjunct. This is the
type-level twin of the flat-enum fall-through hazard, and it is the whole reason core needs the
`Dynamic` layer.

Guards that are already *positive* need no change and reject `Static` for free: `REG` and `SHARED`
([Modifier.scala:28,32]), `din` ([DFVal.scala:1761]), `Process` ([Process.scala:38]), and the ED
method call-site requirement.

`while` currently *requires* RT ([DFWhile.scala:50,59]). Widen to `RT | Static`.

### 5.3 `CONSTRET`

Add to the `<>` match type ([DFVal.scala:100-105]):

```scala
case CONSTRET => (DFC, DomainType.Static, DFC.Scope.Function) ?=> DFValOf[DFType.Of[T]]
```

The `DomainType.Static` context parameter is **not optional**. A static function's body is a lambda
lexically nested inside its enclosing design, so without it the enclosing `DomainContainer`'s given
(say `DomainType.RT`) stays in scope inside the body, and `.reg`, `REG` variables, and the rest come
back to life. The inner context parameter shadows the outer given, exactly as `EDRETOf` does with
`DomainType.ED` ([DFVal.scala:111-113]).

It also gives the call-site rule for free: `DomainType.Static` is summonable everywhere (the
companion-scope ambient given), so **static functions are callable from any domain and from the
global scope**, unlike ED methods, whose `DomainType.ED` context parameter is what restricts them to
ED domains.

### 5.4 No new scope: reuse `Scope.Function`

A static function body is a `Scope.Function` body. Nothing in `DFC.Scope` changes. The division of
labor is:

- the **scope** carries the function-region restrictions (no processes, no step blocks, and so on),
  and it is what the plugin's design-def predicate keys on ([CapturePhase.scala:61-79]), so a
  `CONSTRET` def is recognized as a design def, with capture discovery and phantom rigging, at zero
  cost;
- the **domain** (`Static`) carries the static-region restrictions, and is what discriminates a
  static function from an ED method everywhere it matters (in the plugin, in the printers, and in
  the type-level guards).

Two things come for free from reusing `Scope.Function`:

- blocking assignments inside the body, via the existing `A <:< DFC.Scope.Function` alternative in
  ``InsideProcess:=`` ([DFVal.scala:1711-1716]), which is what static variables need;
- **a static function cannot call a non-static ED method**, and it enforces itself. An ED method call
  site must summon `DomainType.ED`; inside a static body the domain given is `Static`, so the call
  does not compile, with the existing `implicitNotFound` message. No plugin check needed.

### 5.5 The declaration guard: RESOLVED by the scope lattice

This was the second high-risk item: a **top-level** static function has no enclosing container, and
the old declaration guard summoned `Scope.Local`, which is not summonable there, so `Bits(8) <> VAR`
in its body would have been rejected as a global declaration. That collided with two locked items at
once, variables in static function bodies (decision 7) and global-only static functions (§9).

**The scope lattice ([scoping.md](scoping.md)) has since landed and dissolves this.** No work is
needed here. The declaration guard now takes the INNERMOST scope as a type parameter and subtype-tests
it, and `Scope.Function` has the `HasVars` capability:

```scala
protected type DclScope[S <: DFC.Scope] = AssertGiven[
  S <:< DFC.Scope.HasVars | DFC.Scope.HasPorts,
  "Port/Variable declarations cannot be global"
]
```

which discriminates every site correctly, with no reference to the domain at all:

| Site | innermost `Scope` | declarations |
|---|---|---|
| true global scope | `Global` | rejected (no `HasVars`) |
| a plain Scala helper carrying only a `DFC` | `Global` | rejected (the ambient `Function` given loses to `Global`) |
| top-level static function body | `Function` | **allowed** |
| ED function body | `Function` | allowed (already relied on: ED methods declare local variables) |
| static function inside a design | `Function` | allowed |

This rests on the given-priority invariant that `Global` (declared directly in `object Scope`) beats
the ambient `Function` (declared in the `ScopeLP` base trait) for a bare `Scope` summon, while a
context-function parameter, being lexically nested, beats both inside the body. That invariant is no
longer a trusting comment: it is pinned by the paired tests in
[GlobalsSpec](../core/src/test/scala/CoreSpec/GlobalsSpec.scala) (a `DFC`-carrying helper in no DFHDL
scope cannot declare) and [ScopeChecksSpec](../core/src/test/scala/CoreSpec/ScopeChecksSpec.scala).

### 5.6 `designFromDefImpl`

[r__For_Plugin.scala:152-179] has `designFromDFDef` (domain `DF`) and `designFromEDDef` (domain `ED`).
Add `designFromStaticDef` (domain `Static`).

For a static function, the declared const arguments are routed to `genContainerParam` (design params),
alongside the captured constants (`phantomConstArgs`), and **nothing** is routed to `inputs`
([r__For_Plugin.scala:217-231]). A static function's def design therefore has no input ports at all:
its interface is design parameters plus the return port.

### 5.6a Why params, not ports: the global call site, and the body-sharing rule it costs

A def design's **input ports are wired by a net at the call site**. The DFHDL printer recovers an
arg's value by reading the connection back out ([DFOwnerPrinter.scala:314]), and the HDL backends do
the same. A net needs the `Concurrent` capability and a block to own it. **A static function must be
callable from global scope**, where there is neither, so a port arg is not merely awkward there, it is
unrepresentable. A design parameter carries its value in the member itself and prints from the
instance's `paramMap` ([DFOwnerPrinter.scala:317]) with no net. Hence params.

That fixes the call site, but it re-opens something the port model was quietly holding shut, and the
plan must say so:

**One printed body can serve all call sites only when the argument values are invisible to
elaboration.** An ED method's port args are DFVals, so the body cannot branch on them, so every call
site necessarily elaborates a structurally identical body, so a single subprogram is sound. That is
the real content of `DesignDefsPhase`'s "one printed body serves all its calls" rejection of const
args ([DesignDefsPhase.scala:95-119]). A static function's args are const *by definition*, hence
visible to elaboration, hence `if (n > 4)`, a `for` bound over `n`, or a width derived from `n` makes
two call sites elaborate genuinely different bodies. Moving the arg from a port to a param does not
change that: it is a property of const-ness, not of the IR category.

It resolves cleanly, and better than the ED rule does, because **the body references the
`DesignParam` member, not its value**. Unless the Scala code actually branched on the value, every
call site's body IR is structurally identical and differs only in the param's const data. So:

- print non-phantom design params as subprogram formals (decision 5a), and the call site's `paramMap`
  as the actual-argument list;
- **dedup def-design blocks by structural body equality and emit one subprogram per equivalence
  class.**

The common case then prints exactly one function taking formals, which is what the port model was
trying to buy. A body that genuinely diverged on a param value is monomorphized into its own
subprogram automatically, with no error to raise. This is strictly better than the ED-method rule,
which had to *forbid* const args only because it had no dedup step to fall back on. The dedup is the
one piece of new machinery this decision buys us, and §7 owns it.

## 6. Plugin changes (`plugin`)

All in [DesignDefsPhase.scala] plus a shared symbol in [CapturePhase.scala]:

1. **Discriminate on the domain evidence, not the scope.** A static def and an ED method both carry a
   `Scope.Function` parameter, so `CapturePhase.isEDAnonDef` ([CapturePhase.scala:61-79]) recognizes
   both as design defs, which is what we want, and needs no change. What separates them is the
   *other* context parameter: `DomainType.Static` versus `DomainType.ED`. Add
   `domainTypeStaticCls` / `domainTypeEDCls` to the phase's symbol initialization and test the anon
   def's parameters against them.
   These are opaque types in core, so from outside `object DomainType` they are distinct and mutually
   unrelated, and a `<:<` test distinguishes them cleanly. (Inside `object DomainType` itself the
   opacity is lifted, but no design def is ever written there.)
2. Route recognized static defs to `designFromStaticDef`.
3. **Narrow the existing const-argument rejection.** [DesignDefsPhase.scala:111-119] currently
   rejects `<> CONST` arguments for all ED-family defs, and a static def is one by the `Scope.Function`
   predicate, so every static function would error on its first argument. The check must apply only to
   non-static ED methods (that is, to defs whose domain evidence is `ED`).
   The comment above it ([DesignDefsPhase.scala:105-110]) needs rewriting rather than just
   narrowing. Its second rationale ("differing applied values across call sites cannot share one
   body") is the *true* one and §5.6a now answers it directly: static functions make const args design
   params, print them as formals, and dedup divergent bodies. Its first rationale ("a Verilog function
   cannot take a constant formal at all") is a red herring, since the formal is an ordinary input
   formal. Rewritten, it should say: an ED method rejects a const arg because it has no dedup step, so
   it cannot honor a body that diverges per call site; a static function accepts one because it does.
   If the dedup lands cleanly, the ED-method rejection becomes removable too (§10).
4. **New enforcement for static functions**:
   - all DFHDL arguments must be `<> CONST` (the inverse of the ED rule);
   - a `Unit` return type is disallowed with `CONSTRET`;
   - captured non-constant values are rejected (an existing phantom *port* capture would be a
     non-constant input, which contradicts staticness). `CapturePhase.discoverDesignDefCaptures`
     already partitions captures into `phantomConsts` and `phantomVals`; for a static def,
     `phantomVals` must be empty.
5. **Purity is enforced by promoting the existing `PureCheck` verdict to an error** (decision 8,
   §8.1). [PureCheckPhase.scala] already computes it for every def, and it already knows what a design
   def is: it mixes in `CapturePhase` and carries its own `isEDAnonDef` ([PureCheckPhase.scala:306-317]),
   which §6.1's domain-evidence test replaces in both phases alike. So the change is small and local:
   at the end of `analyze`, for each root that is a **static** def, if its verdict is impure (it is in
   `verdictImpure`, or the user wrote an explicit `@pure(false)`), report an error at the def's
   position instead of only synthesizing the marking.
   - **Do not treat `pure(true, impureParams*)` as impure.** That annotation says the def *is* pure and
     names the parameters (including phantom constants) whose applied data must enter the cache key.
     A static function with phantom constants is legal and pure. Only `pure(false)` is an error.
   - `PureCheck` runs after `TopAnnot` and before `MetaContextPlacer`, and `DesignDefs` acts later, so
     the error surfaces before any static-def rigging is built. No phase reordering is needed.
6. Recursion is **already** rejected for design defs generally ([DesignDefsPhase.scala:95-104]), so
   static functions are covered with no new check. Only the message wording needs to read sensibly for
   a static function. Note the reason is elaboration termination, not purity (§8.1).

Whether relaxing the const-argument rejection for *regular* ED methods (making a const arg a design
param there too, printed as a formal) is worth doing is deferred; nothing in this plan depends on it,
but §5.6a's dedup is exactly the machinery that would unblock it.

## 7. Stage and printer changes

- **Def-design params print as formals, never as generics.** A regular design's `DesignParam`s print
  as a VHDL `generic` block ([VHDLOwnerPrinter.scala:54-56]) with a `generic map` at instantiation
  ([VHDLOwnerPrinter.scala:200-205]), and equivalently as Verilog parameters. A subprogram has no
  generics in either language. So the def-design printing path must put non-phantom design params
  into the **same formal list as the input ports** (decision 5a), and the instance's `paramMap` into
  the actual-argument list. For a static function the formal list is params only; for an ED method it
  is ports only; the printer should not care which. Phantom params stay hidden from the signature, as
  today ([DFOwnerPrinter.scala:266]).
- **New: structural dedup of def-design blocks** (§5.6a). Each call site elaborates its own def-design
  block. Group them by structural body equality and emit one subprogram per class; a body that
  diverged on a param value gets its own monomorphized copy and a distinct name. Without this, two
  call sites with different const args would silently share a body that is only correct for one of
  them. This is the one genuinely new stage the params decision costs us, and it must land with it,
  not after.
- Everything keyed on `isEDMethod` must be triaged against `isHDLSubprogram` (§4.5), in particular
  [PrepEDDefs.scala] (named calls become variables), [DropDesignDefs.scala], and the design-def
  printing paths in [Printer.scala:169,218] and [DFOwnerPrinter.scala].
- `AddClkRst`, `ExplicitClkRstCfg`, and friends match on `DomainType.RT`, so a `Static` domain never
  gets clk/rst for free. Assert it rather than assume it.
- [DFValPrinter.scala:393-396] has `case DomainType.ED => ??? // impossible!` for the history
  operator. It gains a `Static` twin, also impossible once the `.prev` guard in §5.2 is fixed.
- The DFHDL-code printer ([DFOwnerPrinter.scala:276, 363, 545]) needs `CONSTRET` in the return-modifier
  match and a name for the static domain in the design/domain class matches.
- VHDL backend: a static function emits as a `pure function`. Verilog/SV backend: it emits as a
  `function automatic`. Note the naming collision to anyone reading the SV printer: SystemVerilog's
  `static` means variable *lifetime* and is the opposite of `automatic`, so a "Static domain" emits an
  "automatic function". Comment it where the two meet, or someone will "fix" it.

## 8. Static function body scope

Allowed:

- arithmetic and logic operations on constants
- `if` / `match` (as expressions, since the function must produce a value)
- `for` and `while` loops
- `<> VAR` declarations (static variables: mutate within the body, bounded by it)
- `<> CONST` declarations
- reads of captured constants and globals (materialize as phantom design parameters)
- calls to other static functions

Rejected, and by what mechanism:

| Construct | Mechanism |
|---|---|
| ports, signals | declaration guard (§5.5). A static function's def design has **no input ports at all**: its interface is design parameters plus the return port (§5.6a) |
| `REG`, `SHARED` variables | existing positive guards ([Modifier.scala:28,32]) |
| `.prev` / `.reg` | needs the §5.2 fix; otherwise silently allowed |
| processes, `wait`, events | positive `A <:< DomainType.ED` guards |
| design instantiation | new guard |
| calls to non-static ED methods | free, via the missing `DomainType.ED` given (§5.4) |
| assertions, printing | `Scope.Function` grants no `TextOut` capability. Load-bearing for purity (§8.1), so do not relax it |
| recursion | already rejected for design defs generally ([DesignDefsPhase.scala:95-104]). An *elaboration* limit, not a purity one (§8.1) |
| captured non-constants | new plugin check (§6.4): the DFHDL-level half of purity (§8.1) |
| impure elaboration (randomness, IO, time, outer `var`, impure callee) | existing `PureCheck` verdict, promoted to an error for static defs (§6.5, §8.1) |

Blocking assignment (`:=`) to a static variable is already permitted by the `A <:< DFC.Scope.Function`
alternative in ``InsideProcess:=`` ([DFVal.scala:1711-1716]), so it needs no work.

### 8.1 Purity (decision 8): reuse `PureCheck`, and promote its verdict to an error

**No new analysis.** [PureCheckPhase.scala] already answers exactly this question for every def in the
run. It synthesizes `@hw.annotation.pure(false)` on anything whose elaboration detectably depends on
an effect: a reference to an already-impure symbol (transitively, and across compilations via TASTy),
a blacklisted API (randomness, IO, time, system state), or a read or write of a `var` declared outside
the definition. Today that verdict is **advisory**: `pure(false)` merely means "never cache this
design's elaboration".

For a static function the verdict becomes **fatal**. A static def whose `PureCheck` verdict is impure
is an error, not a cache opt-out. That is the whole enforcement, and it costs one condition at the end
of the phase (§6.5).

**Phantom constants do not make a function impure.** This is the trap, so it is worth being precise.
`PureCheck` records *data-impure parameters* by name on the def's own annotation as
`pure(true, impureParams*)`, and a captured constant that the DesignDefs rigging turns into a phantom
design parameter is recorded there by its predicted name ([PureCheckPhase.scala:445-470]). The `true`
is the point: the def **is** pure. The names only say "this parameter's applied *data* was forced into
elaboration, so the cache key must include it". A static function may freely have phantom constants,
and it stays pure. Only `pure(false)` is an error.

**The two purity notions are complementary, and both are needed.** `PureCheck` reasons about
*Scala-level* elaboration effects; DFHDL's own core is on its trusted list, so it will never flag a
DFHDL-level effect. Those are covered separately, and decision 8 is the conjunction:

| Effect | Caught by |
|---|---|
| randomness, IO, time, outer `var`, impure callee | `PureCheck`, now fatal (§6.5) |
| reading a captured DFHDL signal (a non-constant) | the captured-non-constants check (§6.4) |
| writing anything outside the body | no ports beyond the return (§5.6a) |
| assertions, printing | `Scope.Function` grants no `HasTextOut` (already enforced) |

`HasTextOut` staying off `Scope.Function` is therefore load-bearing, not incidental
([scoping.md](scoping.md) §1), and must not be relaxed later for convenience. This half of purity is
already enforced at compile time by the landed scope lattice.

**What purity then buys.** Two things the plan already needed:

- **It makes the §5.6a body dedup mandatory, not an optimization.** A pure function must satisfy its
  signature for *every* actual its formals admit. A body that elaboration specialized to `n = 3`
  (because the Scala branched on `n`) does not: calling it with `n = 5` would be wrong. Monomorphizing
  divergent bodies is the only sound answer, and purity is the reason.
- **The call memoization is already built.** A pure static call is a deterministic function of its
  argument data, which is precisely what the elaboration cache keys on, with `pure(true, names*)`
  naming the data that must enter the key. So "memoize a static call on its args" is not new
  machinery, it is [elaboration caching](elaboration-caching.md) doing its existing job, and the
  phantom-constant names are how a captured constant reaches the key.

**VHDL's `pure` is weaker than ours** (it only forbids referencing signals and shared variables
declared outside the subprogram, and it permits `report`), so our rule implies it, and emitting the
`pure function` keyword (§7) is always sound.

**Purity does not bear on recursion.** Recursion is already rejected for design defs generally
([DesignDefsPhase.scala:95-104]) and static functions are no exception. The reason is *elaboration*,
not purity: the Scala body is re-run per call site, so a recursive def would not terminate at
elaboration. A pure function may legally recurse in both VHDL and Scala, so nobody should read
decision 8 as licensing it, nor cite purity when explaining the rejection.

**The escape hatch stays.** An explicit user `@pure` (or `@pure(true)`) is a trust override:
`PureCheck` does not analyze such definitions ([PureCheckPhase.scala:61-64]), so it cannot mark them
impure and the static-def error cannot fire. That is deliberate and consistent with the rest of the
phase, but it means a user can assert purity the analysis cannot prove. Document it as such rather
than trying to close it.

## 9. Emission location

A design-local static function is emitted in that design's declarative region, so it can be called
from the design body but **not** from that design's own generic map or port declarations. A
global (top-level) static function is emitted in a package and is callable from anywhere, including
generic maps.

This matters for the headline use case. If the point is to compute a design parameter's value while
keeping the generated HDL parametric (emitting `parameter W = clog2(N)` rather than a folded
literal), the function must be visible at the parent's generic map, therefore global.

Decide before implementing: are static functions **global-only** for now, or design-local with the
restriction above spelled out? Global-only is the smaller surface and covers the motivating case.

### 9.1 A global call site needs a global def-design block

The params decision (§5.6a) removes the *net* that a global call site could not own, but the call
still instantiates a def design, and that instance and its block need a home. At global scope there is
no owning block. Members at global scope are simply **ownerless**: `isGlobal` is "my `ownerRef` is
`Empty`" ([DFMember.scala:418]), so an ownerless `DFDesignBlock` is representable in the IR as it
stands. What is not yet true is that the rest of the compiler expects one:

- `DB.designMemberList` and the OML generators are rooted at `top` ([DB.scala:336-344]), so a
  design block outside `top` needs to be threaded through deliberately, not discovered by accident.
- `SanityCheck` must learn that an ownerless design block is legal, and only for a static function.
- The VHDL backend already emits a globals package; a global static function belongs in it, which is
  also exactly what §9 wants for the generic-map use case. The Verilog side needs the equivalent.

This is the substantive open item the params decision leaves behind. It is smaller than the net
problem it replaces (no `Concurrent` capability to invent, no block to conjure), but it is not free,
and it should be settled together with the global-only question above: **if static functions are
global-only, this is the only place a def-design block ever lives, which is the simplest world.**

## 9a. What the scope lattice already did for this plan

The scope-lattice refactor has **landed** (see [scoping.md](scoping.md)). `DFC.Scope` is now a lattice
of fine-grained capabilities, and three items this plan worried about are settled:

- **§5.5 is dissolved.** A top-level static function body can declare variables, because the
  declaration guard subtype-tests the innermost scope and `Scope.Function` has `HasVars`. No widened
  guard, and no domain involvement.
- **The plugin does NOT inject the body's scope given pre-typer.** That was proposed and rejected.
  `CONSTRET` keeps its `Scope.Function` context parameter, exactly as `EDRET` does, and the ambient
  `Scope.Function` given stays. It costs nothing, because the capabilities are fine-grained: the
  ambient given only reaches `HasVars`/`HasAssign`/`HasLoops`, and those are the guards that take the
  innermost scope as a type parameter.
- **`Scope.Function` has no `HasTextOut`,** so text output inside a function body is already a compile
  error. That is decision 8's purity, enforced by the scope rather than by a new check.

What this plan still owes, unchanged: `CapturePhase.isEDAnonDef` must move from a scope test to a
**domain** test (§6.1), since a static def and an ED method both carry a `Scope.Function` parameter and
only the domain evidence separates them.

The `while` item in §5.2 also stands: `while` is still gated on `DomainType.RT`, not on the `HasLoops`
capability (see [scoping.md](scoping.md) §7), so widening it to admit `Static` is still this plan's job.

## 10. Deferred

- **The folding interpreter** (§4.4): evaluating a static body to a `KnownConst`. Not needed for
  parametric HDL emission, and DFHDL types are sized by Scala regardless, so a static function can
  never size a type. Revisit when a stage actually needs the number.
- **Globals retrofitted into the Static domain** (§4.3): the model-completing change, but static
  functions do not depend on it.
- **Static variables in initial blocks** (motivation item 3): falls out of the folding interpreter
  plus the static-scope rules, so it follows both.
- **A user-facing `StaticDomain { ... }` block**, alongside `DFDomain`/`RTDomain`/`EDDomain`. Coherent
  but widens the surface. Static domain owners are, for now, exactly two: the global scope and
  `CONSTRET` def designs. Additive later if wanted.
- **Const arguments for regular ED methods** (§6).
- **Static procedures** (`Unit <> CONSTRET` with `out` formals, as VHDL procedures allow). Explicitly
  rejected for now.

## 11. Implementation order

1. IR `DomainType` lattice plus `ReadWriter`, `isInStaticDomain`, `isStaticFunction`,
   `isHDLSubprogram`. Nothing produces a `Static` domain yet, so the tree stays green.
2. Core: `Dynamic` opaque layer, fix the negative guards (§5.2), widen `while`. Still no `Static`
   producer, and the guards are now sound in advance.
3. Core: `Static` opaque type, flip the ambient given, `CONSTRET`, the widened declaration guard
   (§5.5) plus its test of the `Global`-beats-ambient-`Function` invariant, `designFromStaticDef`.
4. Plugin: recognition on the domain evidence, routing, the enforcement rules, narrowed const-arg
   rejection, and the `PureCheck` verdict promoted to an error for static defs (§6.5).
5. Static-port `ConstData` (§4.4), plus the audit of const-`Dcl` folding stages.
6. Printers: def-design params as formals (never generics, §7), VHDL `pure function`, SV
   `function automatic`, DFHDL `CONSTRET` code printing.
7. The def-design structural body dedup (§5.6a). It must land **with** step 6, not after: without it,
   two call sites with different const args silently share a body correct for only one of them.
8. Reference HDL updates (`sbt docExamplesRefUpdate`).

Steps 1 and 2 are independently mergeable and de-risk the rest.

## 12. Testing

- `StagesSpec`: a static function called from DF, RT, and ED domains, and from the global scope;
  nested static calls; a captured constant becoming a phantom parameter; a `for`-loop-mutated static
  variable, declared in a **top-level** static function (the §5.5 case, which fails today).
- **Purity** (§8.1), in [PureCheckSpec] alongside the existing purity tests:
  - a static function using randomness, IO, time, or an outer `var` is a compile **error**, whereas the
    same body in a regular design def only gets marked `pure(false)`. Include the transitive case (an
    impure helper `def`) since that is where the marking, not the body, carries the verdict.
  - a static function with a captured constant is **accepted** and annotated `pure(true, <name>)`. This
    is the regression test for the trap: a phantom constant must never read as impure.
  - an explicit `@pure` on an otherwise-impure static function suppresses the error (the documented
    trust override).
- **Body dedup** (§5.6a): two call sites with different const args whose body does *not* branch on them
  emit **one** subprogram with formals; two whose body *does* branch (`if (n > 4)`) emit **two**
  monomorphized subprograms. The second is the one that silently miscompiles if the dedup is missing.
- The §5.5 invariant, directly: a `<> VAR` declaration at true global scope is still rejected, while
  the same declaration inside a top-level static function body is accepted. These two must be tested
  as a pair, since the whole guard rests on `Scope.Global` beating the ambient `Scope.Function` for a
  bare `Scope` summon.
- Negative elaboration tests (`assertElaborationErrors` / compile-time errors): a non-const argument;
  a `Unit` return; a captured non-constant; `.prev` in a static body; a `REG` variable; a call to a
  non-static ED method; recursion.
- Backend: VHDL, SystemVerilog, and Verilog reference output for a static function used to compute a
  design parameter, checking that the generated HDL stays parametric rather than folded.
- `testApps`: the existing simulation matrix, to confirm the emitted subprograms actually elaborate
  in ghdl/nvc/verilator/iverilog.

[DomainType.scala]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DomainType.scala
[DFMember.scala:418]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DB.scala:336-344]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DB.scala
[DFMember.scala:132-140]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:507-562]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:1756]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFVal.scala:100-105]: ../core/src/main/scala/dfhdl/core/DFVal.scala
[DFVal.scala:101]: ../core/src/main/scala/dfhdl/core/DFVal.scala
[DFVal.scala:111-113]: ../core/src/main/scala/dfhdl/core/DFVal.scala
[DFVal.scala:1700, 1713, 1718]: ../core/src/main/scala/dfhdl/core/DFVal.scala
[DFVal.scala:1761]: ../core/src/main/scala/dfhdl/core/DFVal.scala
[DFC.scala:140-178]: ../core/src/main/scala/dfhdl/core/DFC.scala
[Fork.scala:28]: ../core/src/main/scala/dfhdl/core/Fork.scala
[Process.scala:38]: ../core/src/main/scala/dfhdl/core/Process.scala
[Process.scala:42, 54]: ../core/src/main/scala/dfhdl/core/Process.scala
[Modifier.scala:28,32]: ../core/src/main/scala/dfhdl/core/Modifier.scala
[DFWhile.scala:50,59]: ../core/src/main/scala/dfhdl/core/DFWhile.scala
[Design.scala:371-375]: ../core/src/main/scala/dfhdl/core/Design.scala
[Container.scala:29]: ../core/src/main/scala/dfhdl/core/Container.scala
[r__For_Plugin.scala:152-179]: ../core/src/main/scala/dfhdl/core/r__For_Plugin.scala
[r__For_Plugin.scala:217-231]: ../core/src/main/scala/dfhdl/core/r__For_Plugin.scala
[CapturePhase.scala]: ../plugin/src/main/scala/plugin/CapturePhase.scala
[CapturePhase.scala:61-79]: ../plugin/src/main/scala/plugin/CapturePhase.scala
[DesignDefsPhase.scala]: ../plugin/src/main/scala/plugin/DesignDefsPhase.scala
[PureCheckPhase.scala]: ../plugin/src/main/scala/plugin/PureCheckPhase.scala
[PureCheckPhase.scala:61-64]: ../plugin/src/main/scala/plugin/PureCheckPhase.scala
[PureCheckPhase.scala:306-317]: ../plugin/src/main/scala/plugin/PureCheckPhase.scala
[PureCheckPhase.scala:445-470]: ../plugin/src/main/scala/plugin/PureCheckPhase.scala
[PureCheckSpec]: ../compiler/stages/src/test/scala/StagesSpec/PureCheckSpec.scala
[scoping.md]: scoping.md
[DesignDefsPhase.scala:95-104]: ../plugin/src/main/scala/plugin/DesignDefsPhase.scala
[DesignDefsPhase.scala:105-110]: ../plugin/src/main/scala/plugin/DesignDefsPhase.scala
[DesignDefsPhase.scala:111-119]: ../plugin/src/main/scala/plugin/DesignDefsPhase.scala
[PrepEDDefs.scala]: ../compiler/stages/src/main/scala/dfhdl/compiler/stages/PrepEDDefs.scala
[DropDesignDefs.scala]: ../compiler/stages/src/main/scala/dfhdl/compiler/stages/DropDesignDefs.scala
[Printer.scala:169,218]: ../compiler/ir/src/main/scala/dfhdl/compiler/printing/Printer.scala
[DFOwnerPrinter.scala]: ../compiler/ir/src/main/scala/dfhdl/compiler/printing/DFOwnerPrinter.scala
[DFOwnerPrinter.scala:236]: ../compiler/ir/src/main/scala/dfhdl/compiler/printing/DFOwnerPrinter.scala
[DFOwnerPrinter.scala:266]: ../compiler/ir/src/main/scala/dfhdl/compiler/printing/DFOwnerPrinter.scala
[DFOwnerPrinter.scala:276, 363, 545]: ../compiler/ir/src/main/scala/dfhdl/compiler/printing/DFOwnerPrinter.scala
[DFOwnerPrinter.scala:314]: ../compiler/ir/src/main/scala/dfhdl/compiler/printing/DFOwnerPrinter.scala
[DFOwnerPrinter.scala:317]: ../compiler/ir/src/main/scala/dfhdl/compiler/printing/DFOwnerPrinter.scala
[DFValPrinter.scala:393-396]: ../compiler/ir/src/main/scala/dfhdl/compiler/printing/DFValPrinter.scala
[VHDLOwnerPrinter.scala:54-56]: ../compiler/stages/src/main/scala/dfhdl/compiler/stages/vhdl/VHDLOwnerPrinter.scala
[VHDLOwnerPrinter.scala:200-205]: ../compiler/stages/src/main/scala/dfhdl/compiler/stages/vhdl/VHDLOwnerPrinter.scala
