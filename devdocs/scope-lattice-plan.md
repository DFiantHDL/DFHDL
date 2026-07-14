# The `DFC.Scope` Capability Lattice

Status: plan, not yet implemented. Companion to [static-domain-plan.md](static-domain-plan.md),
which depends on it (see §7 there and §5 here).

## 1. Why

`DFC.Scope` today is a flat set of places (`Global`, and `Local` with leaves `Design`, `Domain`,
`Process`, `Initial`, `Interface`, `Procedural`, plus `Function` sitting outside `Local`). Because the
scope carries no capability structure, every construct that needs to ask "am I allowed here?"
hand-writes a union of places. The worst offender is ``InsideProcess:=`` ([DFVal.scala:1711-1716]),
a six-way union of scopes and domains, but `LocalOrNonED`, `NotLocalVar`, `Fork`, `Process`, and the
declaration guard are all variations on the theme. Adding a place means auditing every union, and
nothing tells you which ones you missed.

Restructure `Scope` so that **each scope adds to the capabilities of the ones it extends**, and every
guard becomes a single subtype test against a capability rather than an enumeration of places.

This also removes the awkwardness that blocks the static-function work: `Scope.Function` currently
sits *outside* `Local`, so a function body can only declare variables by accident of being lexically
nested inside a design (§5).

## 2. The lattice

Two kinds of trait, and the distinction is a hard rule:

- **Capabilities** are mixins. They grant constructs. They **never** have a given.
- **Places** are where the user's code actually is. They mix capabilities together, and they are the
  only traits with a given.

```
Scope                                      <> CONST declarations; arithmetic/logic/conversion on constants
├── Global (place)                         adds nothing; the marker for "no enclosing container"
├── Local                                  <> VAR (modifier variants per domain); := (per domain)
│   ├── Concurrent                         <> connections; process/initial declarations;
│   │                                      .reg/.prev/.rising/.falling (per domain); generate blocks
│   ├── Sequence                           for/while loops
│   │   └── TimedSequence                  wait statements; calling tasks
│   └── TextOut                            assertions and printing
├── PublicDcl                              <> IN/OUT/INOUT ports; interface and view instantiation
│
├── Design    (place)  = PublicDcl + Concurrent + domain declaration
├── Domain    (place)  = PublicDcl + Concurrent + domain declaration
├── Interface (place)  = PublicDcl
├── Generate  (place)  = Concurrent + constant-only if/for
├── Process   (place)  = Procedural + :== (per domain) + step blocks
├── Procedural(place)  = TimedSequence + TextOut
├── Initial   (place)  = Sequence + TextOut
└── Function  (place)  = Sequence + calling other functions
```

Deltas from the first sketch, with reasons:

- **The base `Scope` carries the constant capabilities, not `Global`.** As sketched, `Local extends
  Scope` (not `Global`), so `<> CONST` declarations and constant arithmetic would have been legal only
  at global scope and illegal inside a design. `Global` now adds nothing: it is purely the "no
  enclosing container" marker that the ambient given attaches to.
- **`Initial` is a `Sequence` + `TextOut`, deliberately NOT a `TimedSequence`.** No `wait` in an
  initial block, and by the same token no task calls. This is not a new restriction: `initialCheck`
  ([DB.scala:1368-1369]) already rejects every `Wait` inside an `initial` block, unconditionally,
  with a test at [ElaborationChecksSpec.scala:656]. The lattice simply promotes that rule from an
  elaboration error to a compile-time one. `TextOut` stays, because printing from an initial block is
  a primary use (an ED-domain one: the RT domain forbids it separately, at [DB.scala:1351-1358]).
  Keep the elaboration check anyway, since a helper `def` can launder scope evidence past a
  type-level guard, which is exactly why the ED-method checks are duplicated in the IR today
  ([ElaborationChecksSpec.scala:713-714]).
- **`Process extends Procedural`.** As sketched the two had identical capability sets. Process is
  Procedural plus `:==` plus step blocks. This also makes the current
  `given (using Process): Procedural` redundant: subtyping supplies it.
- **Domain declaration moved off `PublicDcl`.** `Interface extends PublicDcl`, so granting domain
  declaration there would let a domain be declared inside an interface. It belongs to `Design` and
  `Domain`.
- **`Interface` is no longer `Local`.** It is structural only, which is consistent with the existing
  `NotInsideInterface` guard ([DFVal.scala:406-409]) that already forbids initialization there. This
  is a behavior change from today's `Interface extends Local`: confirm that no interface body relies
  on `<> VAR`.

## 3. The rule that makes it work: guards test the innermost scope

Core currently mixes two guard forms:

- **summon the capability**: `AssertGiven[DFC.Scope.Local, ...]` ([Modifier.scala:60]),
  `AssertGiven[Scope.Process | Scope.Initial | ...]` ([DFVal.scala:1712]), [Fork.scala:32]
- **subtype-test the summoned scope**: `A <:< DFC.Scope.Process` ([DFVal.scala:1700, 1704])

Summoning a capability finds **any enclosing scope's given** that satisfies it, not the innermost one.
That is harmless today only because the lattice is flat: every `Local` place is a leaf, so "some
`Local` given is in scope" really does mean "I am in a local scope".

Under the lattice it breaks immediately. Inside a process, `AssertGiven[Scope.Concurrent]` would find
the **enclosing design's** given, so `<>` connections and port declarations would become legal inside
a process body.

**Therefore: every scope guard must summon the innermost bare `Scope` and subtype-test it. A
capability must never be summoned directly.** Follow the existing `REG`/`SHARED` shape
([Modifier.scala:28,32]):

```scala
inline def foo(using s: DFC.Scope)(using AssertGiven[s.type <:< DFC.Scope.Concurrent, "..."]) = ...
```

Converting every guard in core to this form is the actual work of this refactor. It is mechanical, but
it is not optional, and it is the step where capability leaks would be introduced if rushed.

## 3a. Guard implementation: keep `AssertGiven`, do not use the plugin

**Decision (2026-07-14): the guards stay on `AssertGiven`.** The measurements below were taken to
answer "type-level assertion or compiler plugin?", and they settle that question (assertion), but they
also tempted a second change, moving off the macro onto plain implicits, that is *not* worth making.
The data is kept here so nobody re-runs it.

Measured on 2026-07-14, N = 1000 guarded call sites in `core/src/test/scala/Playground.scala`
(chunked 200 per design class, since ~2000 statements overflow the JVM method-size limit), best of 3
forced recompiles of `core/Test/compile`. Baseline is the same file with one statement, 3050 ms.

| Variant | Wall | Per site | Guard cost |
|---|---|---|---|
| no guard (what a plugin check costs the typer) | 3718 ms | 0.67 ms | 0 |
| plain implicit + `@implicitNotFound` | 3637 ms | 0.59 ms | ~0, within noise |
| `AssertGiven`, single leaf | 4396 ms | 1.35 ms | 0.68 ms |
| `AssertGiven`, 6-leaf union | 5206 ms | 2.16 ms | 1.49 ms |
| real `Bits(8) <> VAR` (contains one `AssertGiven`) | 12705 ms | 9.66 ms | n/a |

Each extra failing union leaf costs about 0.16 ms. A single-leaf `AssertGiven` doubles the cost of a
trivial call site; the 6-leaf union form triples it.

The cost is **entirely the macro**, not the type-level check. `AssertGiven` ([helpers.scala:257-282])
expands a quote/splice at every use site and runs `Expr.summon` per union leaf. The same constraint
expressed as an ordinary implicit parameter with an `@implicitNotFound` message is free, and it is not
vacuous: verified to accept inside a design and reject at global scope with the custom message.

### But in aggregate the macro is a MINOR cost, which is why we keep it

Measured by replacing the whole definition with `type AssertGiven[G, M <: String] = DummyImplicit` and
rebuilding (all guards then vacuously satisfied). Method: warm everything upstream, then
`<proj>/Test/clean` and time only `<proj>/Test/compile`.

| Module | real `AssertGiven` (s) | stubbed (s) | delta |
|---|---|---|---|
| `compiler_stages/Test` | 187.9, 207.8, 191.1 | 181.0, 181.8, 180.7 | ~7 to 10 s, **4 to 5%** |
| `lib/Test` | 25.2, 25.1, 25.3 | 24.9, 25.3, 25.3 | **none** |

A full clean `Test/compile` of the whole build could NOT resolve the difference at all: the box has a
±20 s band plus a session-long warming trend, so a real run taken late (293 s) beat most stubbed runs
(280 to 325 s). Do not attempt an A/B on full clean builds; isolate a module.

So the entire macro is worth roughly 8 seconds on a 5-minute build, concentrated in the guard-heaviest
module and invisible everywhere else. That is not enough to buy a downgrade in error quality:
`@implicitNotFound` appends Scala's "I found: ..." trailer to the message, whereas `AssertGiven`
reports the message alone, and the guards are a front-line user-facing diagnostic. **Keep
`AssertGiven`.**

Two consequences for the rest of this plan:

- **Justify the lattice on correctness and maintainability** (§3's capability-leak rule, and `Function`
  moving under `Local` in §5), **never on compile time.** The saving was the one speed argument
  available and it does not hold.
- §4's single subtype tests still eliminate most `OrType` disjunctions. That is a readability win in
  its own right, and it makes each `AssertGiven` cheaper (a 6-leaf union costs 1.49 ms, a single leaf
  0.68 ms), but the guard keeps its current form.

**And do NOT move the guards to the plugin.** A plugin cannot report a type error more precisely than
the assertion already does, it moves the guard away from the API it guards into a phase keyed on symbol
names that rots when the API moves, and the measurement shows there is no speed to win by it: a plugin
check costs the typer the same as no guard at all only because it does its work *elsewhere*, and that
elsewhere is not free either. Checks the type system genuinely cannot express (a static function's
captured non-constants, recursion) stay in the plugin, as [static-domain-plan.md] §6 already plans.
This section is about the *scope guards*, not about all checks.

Unrelated but surfaced by the same measurement: a real `Bits(8) <> VAR` costs 9.7 ms to compile, of
which the guard is ~7%. If compile time is the concern, the declaration machinery is a far larger lever
than the guards. See the `/compile-perf` skill.

## 4. What the guards become

| Construct | Today | After |
|---|---|---|
| `<> VAR` declaration | `AssertGiven[Scope.Local]` ([Modifier.scala:60]) | `s <:< Local` |
| `<> IN/OUT/INOUT` | same guard as VAR | `s <:< PublicDcl` |
| `:=` | ``InsideProcess:=``, a six-way union ([DFVal.scala:1711-1716]) | `s <:< Local` plus the domain rule |
| `:==` | ``InsideProcess:==`` + `EDDomainOnly` ([DFVal.scala:1717-1720]) | `s <:< Process` plus domain `ED` |
| `<>` connection | (implicit, unguarded) | `s <:< Concurrent` |
| `while` | `DomainType.RT` only ([DFWhile.scala:50,59]) | `s <:< Sequence` (see §6) |
| `wait` | scope-specific | `s <:< TimedSequence` |
| assertions / printing | scope-specific | `s <:< TextOut` |
| `.REG` / `.SHARED` | domain guards ([Modifier.scala:28,32]) | unchanged (domain, not scope) |

## 5. `Function` under `Local`: the ambient given is not actually poison

This is the item that ties the refactor to the static-domain work, and it resolves in the lattice's
favor for free.

The lattice puts `Function extends Sequence extends Local`. The apparent obstacle is that
`Scope.Function`'s given is **ambient** (declared in `ScopeLP`, [DFC.scala:140-141]), so making it a
`Local` looks like it would make `Scope.Local` summonable everywhere, including at global scope,
collapsing the "Port/Variable declarations cannot be global" guard. [DFC.scala:131-139] documents this
hazard, and it is why `Function` sits outside `Local` today.

**Decision (the user's, 2026-07-14): the plugin does NOT inject the scope given pre-typer.** The
`EDRET` / `CONSTRET` context-parameter lists keep their scope parameter. An earlier draft of this
section proposed dropping the scope from those lists and having `PreTyperPhase` inject
`given DFC.Scope.Function` into the def's rhs instead. Rejected, and it turns out to cost nothing.

### 5.1 Why it costs nothing: §3's rule already closes the leak

The poison is a property of the **guard form**, not of the lattice. Read the given-priority structure
([DFC.scala:140-145]): `given Scope.Function` lives in `ScopeLP`, a *base trait* of `object Scope`,
while `given Global` lives directly in `object Scope`. A given in the subclass wins over one in the
base trait, so a bare `Scope` summon at global scope yields `Global`, never the ambient `Function`.
And inside a function body, the `EDRET`/`CONSTRET` context parameter is a **lexical** given, which
beats anything from the implicit scope. The existing comment says exactly this ("which lexically
shadows this given inside ED function bodies").

So a guard that summons the innermost bare `Scope` and subtype-tests it, per §3, sees:

| Where | `summon[DFC.Scope]` yields | `s <:< Local`? |
|---|---|---|
| true global scope | `Global` (`object Scope` beats `ScopeLP`) | no, so `<> VAR` is rejected |
| top-level static function body | `Function` (context param is lexical) | **yes**, so `<> VAR` is allowed |
| design / domain / process body | `Design` / `Domain` / `Process` (class member is lexical) | yes |

That is precisely the behavior we want, with `Function extends Local` and the ambient given both left
in place. The leak exists today only because [Modifier.scala:60] uses the *summon-the-capability* form
(`AssertGiven[DFC.Scope.Local, ...]`), which searches for **any** enclosing given satisfying `Local`
and would therefore find the ambient one. Converting it to the subtype-test form fixes it.

**Consequence: §3 is now load-bearing, not merely tidy.** It is the only thing standing between the
ambient `Function` given and a legal `<> VAR` at global scope. Two hard rules follow, and the second
is already written into [DFC.scala:136-139]:

- **Never summon a capability.** `AssertGiven[Scope.Local]`, `AssertGiven[Scope.Concurrent]`, and the
  like are banned outright once `Function` is `Local`.
- **Never put a scope in a `NotGiven` guard.** An ambient given makes `NotGiven[Scope.X]` false
  everywhere, so these fail the same way. Audit for them alongside the positive summons.

Both forms must be gone before `Function` moves under `Local`; that ordering is not optional (§8).

### 5.2 The payoff

A **top-level** static function body gets a real `Function` (hence `Local`) scope and can declare
variables. So [static-domain-plan.md] §5.5's widened declaration guard is **not needed** once this
lands: the plain `s <:< Local` test does the job. If the static-domain work lands first, §5.5 stays as
the interim guard and this refactor removes it (§9a there).

## 6. RT does not fit the Concurrent/Sequence split

`while` currently requires `DomainType.RT` ([DFWhile.scala:50,59]), and an RT design or domain body is
exactly where loops are meant to be written (`LoopFSMPhase` turns them into FSMs). So an RT design body
is a `Concurrent` place that nonetheless needs `Sequence` capabilities, which the lattice as drawn does
not give it.

Two ways out:

1. Keep a compound guard: `s <:< Sequence | (s <:< Concurrent & domain <:< RT)`. Cheap, but it
   reintroduces exactly the hand-maintained union the refactor is trying to eliminate.
2. Let the container supply a domain-dependent scope given: `RTDomainContainer`
   ([Container.scala:29]) provides a place that mixes `Sequence` in, while DF and ED containers do
   not. All guards stay pure subtype tests.

Option 2 is the one that keeps the lattice honest, and it generalizes (it is the same move that would
let a DF domain forbid things an ED domain allows without any guard knowing about domains at all).
It needs a check that the scope given and the domain given can be supplied together from
`DomainContainer` without ambiguity.

## 7. `Generate` and the two kinds of `for`

`Generate extends Concurrent`, but the sketch also grants it `for` loops, which live on `Sequence`.
These are two different constructs sharing one syntax: an elaboration-time unrolled loop (concurrent,
generate-like) and a sequential HDL loop (a real loop in the emitted process or subprogram).

Note also that a `for` in a plain design body already works today as ordinary Scala iteration and needs
no capability at all. So the question is narrower than it looks: what `Generate` needs is a
**constant-condition** rule (`if` and `for` over constants only), not the `Sequence` loop capability.
Model it as its own capability on `Generate` rather than by widening `Sequence`.

`Generate` is listed as not yet implemented, so this can be left as a placeholder in the lattice as
long as the shape does not paint us into a corner.

## 8. Implementation order

1. Introduce the capability traits with no behavior change: `Scope` gains the constant capabilities,
   `Local`, `Concurrent`, `Sequence`, `TimedSequence`, `TextOut`, `PublicDcl`. Re-express the existing
   places in terms of them, keeping today's exact capability sets. Nothing should change.
2. Convert every guard to the innermost-scope subtype-test form (§3), keeping `AssertGiven` as the
   mechanism (§3a). Still no behavior change, but this is where a mistake becomes a capability leak,
   so it wants close review and negative tests. Do this for the correctness properties only: there is
   no compile-time win here (§3a).
   **This step is a hard prerequisite for step 4, not a nicety** (§5.1): it is what stops the ambient
   `Function` given from legalizing a global `<> VAR` once `Function` becomes `Local`. It is done only
   when *both* poisoned forms are gone: every capability summon, and every `NotGiven` over a scope.
   Grep for both before declaring it finished.
3. Apply the deltas of §2 (base-scope constants, `Initial` as `Sequence` + `TextOut`,
   `Process extends Procedural`, domain declaration off `PublicDcl`, `Interface` not `Local`). These
   *are* behavior changes and each needs its own negative test. `Initial` is the mild one: it only
   promotes an existing elaboration error to a compile-time error, so its elaboration check and test
   stay green as written.
4. Move `Function` under `Local` (via `Sequence`), **keeping the ambient given and the scope context
   parameter on `EDRET`/`CONSTRET`** (§5). No plugin change, no `PreTyperPhase` injection: that was
   proposed and rejected, and §5.1 shows it buys nothing once step 2 is done. The paired test in §9
   (global `<> VAR` rejected, function-body `<> VAR` accepted) is what proves step 2 was complete.
5. Settle §6 (RT). Prefer the domain-dependent container scope.
6. `Generate` placeholder (§7).

Steps 1 and 2 are independently mergeable and carry no user-visible change, which is what makes this
refactor safe to do alongside the static-domain work rather than before it. Step 4 is the only one
with a user-visible payoff (a top-level static function can declare variables), and it is why step 2
must be done exactly rather than approximately.

## 9. Testing

The refactor's whole risk is capability leaks, so the tests are almost entirely negative, and they must
be written per *place*, not per capability:

- inside a process: no `<>` connection, no port declaration, no nested process, no domain declaration
- inside an initial block: no `:==` ([DFVal.scala:1725-1728]), no `wait`, no task call; printing and
  `for`/`while` are allowed. The `wait` rejection must now fire at compile time, while the existing
  elaboration error ([DB.scala:1368-1369]) stays as the backstop for evidence laundered through a
  helper `def`, so both tests should exist.
- inside an interface: no `<> VAR`, no domain declaration, no initialization
- inside a function body: no `wait`, no process, no port, no connection; `<> VAR`, `:=`, `for`/`while`
  allowed
- inside a procedural body: no step block (today's rule), `wait` allowed
- at global scope: `<> CONST` allowed, `<> VAR` and every port form rejected
- in a DF design body: no `while`; in an RT design body: `while` allowed (§6)

**The one test that proves step 2 was done properly** (§5.1), and it must be written as a pair, since
each half alone is passable by a broken guard:

- a `<> VAR` at true global scope is **rejected**, and
- the same `<> VAR` inside a **top-level** static function body is **accepted**.

Both hold only if the declaration guard subtype-tests the innermost summoned scope. A guard that
summons `Local` instead accepts both (the ambient `Function` given satisfies it everywhere), and one
that forgets `Function` is `Local` rejects both.

Each of these should be a compile-time failure with a readable message, so they belong in the
compile-error test suite rather than in elaboration tests.

[DFC.scala:131-139]: ../core/src/main/scala/dfhdl/core/DFC.scala
[DFC.scala:136-139]: ../core/src/main/scala/dfhdl/core/DFC.scala
[DFC.scala:140-141]: ../core/src/main/scala/dfhdl/core/DFC.scala
[DFC.scala:140-145]: ../core/src/main/scala/dfhdl/core/DFC.scala
[DFVal.scala:406-409]: ../core/src/main/scala/dfhdl/core/DFVal.scala
[DFVal.scala:1700, 1704]: ../core/src/main/scala/dfhdl/core/DFVal.scala
[DFVal.scala:1711-1716]: ../core/src/main/scala/dfhdl/core/DFVal.scala
[DFVal.scala:1712]: ../core/src/main/scala/dfhdl/core/DFVal.scala
[DFVal.scala:1717-1720]: ../core/src/main/scala/dfhdl/core/DFVal.scala
[DFVal.scala:1725-1728]: ../core/src/main/scala/dfhdl/core/DFVal.scala
[Modifier.scala:28,32]: ../core/src/main/scala/dfhdl/core/Modifier.scala
[Modifier.scala:60]: ../core/src/main/scala/dfhdl/core/Modifier.scala
[Fork.scala:32]: ../core/src/main/scala/dfhdl/core/Fork.scala
[DFWhile.scala:50,59]: ../core/src/main/scala/dfhdl/core/DFWhile.scala
[Container.scala:29]: ../core/src/main/scala/dfhdl/core/Container.scala
[CapturePhase.scala:61-79]: ../plugin/src/main/scala/plugin/CapturePhase.scala
[DB.scala:1351-1358]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DB.scala
[DB.scala:1368-1369]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DB.scala
[ElaborationChecksSpec.scala:656]: ../lib/src/test/scala/ElaborationChecksSpec.scala
[ElaborationChecksSpec.scala:713-714]: ../lib/src/test/scala/ElaborationChecksSpec.scala
[static-domain-plan.md]: static-domain-plan.md
