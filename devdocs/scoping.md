# Scoping in DFHDL

How DFHDL decides *where* a construct is allowed to appear: `<> VAR` in a design but not at global
scope, `:==` only in a process, `wait` not in an `initial` block, no text output inside a function.

This describes what is implemented, in [DFC.scala](../core/src/main/scala/dfhdl/core/DFC.scala).
The guards are compile-time, so a misplaced construct is a type error, not an elaboration error.
Coverage lives in [ScopeChecksSpec](../core/src/test/scala/CoreSpec/ScopeChecksSpec.scala).

## 1. The model

`DFC.Scope` is a lattice of two kinds of trait, and the distinction is a hard rule.

**Capabilities** name exactly one thing a scope may do. They are mixins. **They never have a
given.**

| Capability | Grants |
|---|---|
| `HasVars` | `<> VAR` declarations |
| `HasAssign` | `:=` blocking assignment |
| `HasNBAssign` | `:==` non-blocking assignment |
| `HasPorts` | `<> IN/OUT/INOUT`, interface and view instantiation |
| `HasDomains` | domain declarations |
| `HasConnect` | `<>` connections |
| `HasProcesses` | `process` and `initial` declarations |
| `HasHistory` | `.reg` / `.prev` / `.rising` / `.falling` |
| `HasLoops` | `for` and `while` |
| `HasWait` | `wait`, and calls to procedural ED methods (tasks) |
| `HasTextOut` | assertions and printing |
| `HasSteps` | RT step blocks |
| `HasFork` | fork-join blocks |
| `HasLocalBlocks` | `locally` |

Some capabilities are further limited by the **domain**, which is an orthogonal axis: `HasVars`
says a variable may be declared here, while `DomainType.RT` is what allows the `.REG` variant.
Scope answers "where", domain answers "in what timing model". Do not encode one in the other.

A few named **bundles** group the blocks: `Local` (`HasVars`, `HasAssign`, `HasLocalBlocks`),
`PublicDcl` (`HasPorts`), `Concurrent` (`Local` + `HasConnect` + `HasProcesses` + `HasHistory`),
`Sequence` (`Local` + `HasLoops`), `TimedSequence` (`Sequence` + `HasWait`).

**Places** are where the user's code actually is. They mix capabilities, and they are **the only
traits with a given**.

| Place | Is | Notes |
|---|---|---|
| `Global` | (nothing) | the ambient default; only the base `Scope` constant capabilities |
| `Design` | `PublicDcl`, `Concurrent`, `HasDomains`, `HasTextOut` | |
| `Domain` | `PublicDcl`, `Concurrent`, `HasDomains`, `HasTextOut` | |
| `Interface` | `PublicDcl` | purely structural: ports and views, no variables, no domains |
| `Procedural` | `TimedSequence`, `HasTextOut` | a task body (Verilog task / VHDL procedure) |
| `Process` | `Procedural` + `HasNBAssign`, `HasSteps`, `HasFork` | |
| `Initial` | `Sequence`, `HasTextOut` | a `Sequence`, NOT a `TimedSequence`: no `wait`, no task calls |
| `Function` | `HasVars`, `HasAssign`, `HasLoops` | an ED function or static function body |

The base `Scope` carries the constant capabilities (`<> CONST` declarations, arithmetic and logic on
constants), so those are legal everywhere, `Global` included.

Two consequences worth stating outright:

- **`Process` IS a `Procedural`.** A task call is licensed by plain subtyping, with no conditional
  given. Everything a task body can do, a process body can do.
- **`Function` has no `HasTextOut`.** A function is pure by definition (see
  [methods.md](methods.md) §7), so this exclusion is load-bearing, not
  incidental. It also has no `HasConnect`, `HasPorts`, `HasProcesses` or `HasWait`.

## 2. How a scope reaches a guard

Each container supplies its scope as a given: a design class has `given TScope = Scope.Design`, and
a `process { ... }` block passes `Scope.Process` as a context-function parameter. A bare `Scope`
summon therefore resolves to the **innermost** scope, because Scala prefers a more deeply nested
given: a process body's context parameter beats its design's `given TScope`, which in turn beats the
implicit-scope givens in `object Scope`.

One given is different, and it is the source of every subtlety below.

```scala
sealed trait ScopeLP:
  given Scope.Function = Scope.Function   // AMBIENT
object Scope extends ScopeLP:
  given Global = Global
```

`Scope.Function` is **ambient**: it must be summonable at every ED-method call site, since an ED
function is callable from a design body, a process, an `initial` block, or another method body
alike. It lives in a base trait of `object Scope` so that givens declared directly in `object Scope`
(notably `Global`) win a generic `Scope` summon.

But an ambient given is eligible for a summon of **any of its supertypes, from anywhere** — global
scope and plain Scala included. That is the whole reason the capabilities are fine-grained rather
than one coarse `Local` bundle: a guard for a capability `Function` does *not* have is unreachable
by the ambient given, so it fails outside a DFHDL scope exactly as it should.

## 3. Writing a guard

An implicit summon finds **any** given in scope that satisfies it, not the innermost one. Pick the
form by what you are asking.

### Summon the capability — when no enclosing scope has it

```scala
protected type InWaitScope = AssertGiven[
  DFC.Scope.HasWait,
  "`wait` statements are only allowed inside a process or a procedural (task) method body. ..."
]
```

Correct for `HasWait`: nothing outside a process or procedural body has it, so there is no enclosing
given to reach. Also correct for `HasFork`, where reaching the enclosing process is exactly what a
fork-join wants.

**Wrong** for a capability an enclosing scope has. `AssertGiven[HasProcesses]` inside a process body
finds the *enclosing design's* given and cheerfully nests a process inside a process. The failure is
silent: the code is accepted.

### Negate the place — for a nesting prohibition

```scala
protected type NoNestingProcess = AssertGiven[
  util.NotGiven[DFC.Scope.Process],
  "A process cannot be nested inside another process."
]
```

This works because these places' givens are context-function parameters, never ambient: inside a
process `NotGiven[Scope.Process]` is false, outside it is true. **`Scope.Function` must never appear
under a `NotGiven`** — its given is ambient, so the `NotGiven` would be false everywhere.

### Test the innermost scope — when `Function` has the capability

`HasVars`, `HasAssign` and `HasLoops` are reachable via the ambient given, so a plain summon would
accept them at global scope. Take the scope as a type parameter and subtype-test it:

```scala
protected type DclScope[S <: DFC.Scope] = AssertGiven[
  S <:< DFC.Scope.HasVars | DFC.Scope.HasPorts,
  "Port/Variable declarations cannot be global"
]
given evPortVarConstructor[..., SC <: DFC.Scope, ...](using
    tc: DFType.TC.Aux[T, OT],
    ck: SC,                    // summons the INNERMOST scope, and solves SC
    dt: DT,
    checkScope: DclScope[SC]
): ...
```

Two traps here, both of which produce confusing failures:

- `ck: SC` must come **before** the guard. Type variables are not solved in using-clause order, so a
  guard placed first sees an unsolved `SC` and fails even inside a design.
- The guard must **not** mention the modifier's `A` parameter. `Modifier` is covariant in `A`, so
  `M <: Modifier[A, C, I, P]` only lower-bounds it; referencing `A` from a using clause makes the
  compiler instantiate it early and widen it to `Any`, which strips `Assignable` off every port and
  variable and makes every `:=` fail with "Cannot assign to an immutable value". (This is the same
  hazard the `idA: Id[A]` hack guards against in `evAssignDcl`.)

Inside an `inline` body the innermost scope's *type* cannot be named, so the type-parameter form is
unavailable. Route through an intermediate given that summons the scope internally:

```scala
trait InTextOutScope
given [S <: DFC.Scope](using sc: S)(using
    AssertGiven[S <:< DFC.Scope.HasTextOut, "Text output is not allowed here. ..."]
): InTextOutScope with {}
```

and demand it at the use site with `compiletime.summonInline[InTextOutScope]`. `summonInline` is
what defers resolution to the inline expansion site; resolving at the definition site would summon
`Scope.Global` in the defining file itself.

## 4. Scala or DFHDL? Ask for a `DFC`, not a scope

Several constructs exist in both worlds and pick one at compile time: `println`, `assert`, and the
`until`/`to` range builders. The differentiator is **`DFC`**:

```scala
transparent inline def println(inline msg: Any): Unit =
  compiletime.summonFrom {
    case given ScalaPrintsFlag => scala.Predef.println(msg)
    case given DFC             =>
      compiletime.summonInline[InTextOutScope]
      textOut(Op.Println, Some(msg))(using compiletime.summonInline[DFC])
    case _ => scala.Predef.println(msg)
  }
```

`DFC` is the one context genuinely absent outside a DFHDL body. A design body has one (`HasDFC`
supplies it); plain Scala does not. The global fallback `DFCG` does not leak into this, because its
givens live in `object DFCG`, the companion of the opaque *subtype*, which is not in `DFC`'s
implicit scope.

**A scope is the wrong question here.** Any scope-based test is satisfied by the ambient
`Scope.Function` given and so would be true in plain Scala, silently rerouting every user `println`
into a hardware text-out.

Note the two questions are separate and both are asked: `case given DFC` decides *DFHDL or Scala*,
and `InTextOutScope` then decides *does this DFHDL scope actually grant text output* — which is how
a `println` inside a function body becomes a compile error instead of a silent Scala print.

### When Scala code holds a `DFC`

Code that is Scala in spirit but carries a `DFC` will select the DFHDL form. Three places in this
repo do, and each opts out through the `hw.flag` givens that exist for exactly this:

| Site | Flag | Why |
|---|---|---|
| [SimSpec](../compiler/stages/src/test/scala/dfhdl/sim/SimSpec.scala) | all three | a Scala front-end for simulations; it declares a `DFCG` for constant arithmetic on peeked values |
| [DFApp](../lib/src/main/scala/dfhdl/app/DFApp.scala) | `scalaPrints` | CLI banners are console output |
| [AES defs](../lib/src/test/scala/AES/defs.scala) | `scalaRanges` | top-level `<> DFRET` defs whose ranges unroll combinational logic at elaboration |

```scala
import dfhdl.hw.flag.scalaPrints   // or scalaRanges, scalaAsserts
```

If a `for` loop starts yielding a DFHDL value where an `Int` was expected, or a `println` is
rejected for want of a text-output scope, this is why.

## 5. Guards are not airtight, on purpose

A helper `def` launders scope evidence past any type-level guard, because its body is typed in the
scope where it is *declared*, not where it is called:

```scala
class Top extends EDDesign:
  def helperProcess(using DFC): Unit = process(all) {}   // legal: a design body may declare a process
  def bad(l: UInt[8] <> VAL): UInt[8] <> EDRET =
    helperProcess                                        // ... but a process may not be inside an ED function
    l
```

So the elaboration checks in `DB.initialCheck` and `DB.edMethodCheck` remain the backstop, and the
type-level guards are the first line rather than the only one. When you add a scope rule that also
has an IR-level meaning, add it in both places. `wait` inside an `initial` block is checked twice for
this reason.

## 6. Adding a construct

1. Add a capability trait to `object Scope` if no existing one fits. One construct, one trait.
2. Mix it into the places that allow it.
3. Guard the construct, choosing the form from §3 by asking: *does an enclosing scope have this
   capability, and does `Function` have it?*
4. Add both a rejection and a positive control to
   [ScopeChecksSpec](../core/src/test/scala/CoreSpec/ScopeChecksSpec.scala). A leaking guard fails by
   **accepting** code, so a test that only checks the happy path will not catch it. The rejection
   test is the one that matters, and "No error found" is what a leak looks like.
5. If the construct has an IR-level meaning, add the matching elaboration check (§5).

## 7. Known gaps

- **`Generate` blocks** are not implemented. When they land they are a `Concurrent` place with a
  constant-condition rule of their own (`if`/`for` over constants only), which is a new capability
  rather than a widening of `HasLoops`.
- **The port/variable split is not type-level.** `DclScope` asks for `HasVars | HasPorts` and cannot
  ask more precisely, because separating them needs the modifier's `A` parameter, which cannot be
  referenced without widening it (§3). So "an interface declares ports but not variables" rests on
  the runtime owner check in `evPortVarConstructor`.
- **RT and `HasLoops`.** `while` is still gated on `DomainType.RT` rather than on the `HasLoops`
  capability. The two agree today, but the domain guard is the one actually doing the work.
