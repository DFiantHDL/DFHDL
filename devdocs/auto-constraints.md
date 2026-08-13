# Automatic Constraints

What DFHDL does when a width relation is neither provably held nor provably violated, which is the
normal state of affairs once widths are design parameters. It accepts the operation and states the
relation it assumed as a static assertion in the generated design, so every instantiation checks
the contract that this elaboration could not.

This describes what is implemented, in
[AutoConstraint.scala](../core/src/main/scala/dfhdl/core/AutoConstraint.scala) for the mechanism
and in [DFDecimal.scala](../core/src/main/scala/dfhdl/core/DFDecimal.scala) for the checks that
feed it. The generated assertions are ordinary members of the elaborated design, so they print in
the DFHDL code string and in every backend. Coverage lives in
[PrintCodeStringSpec](../compiler/stages/src/test/scala/StagesSpec/PrintCodeStringSpec.scala) (the
generated forms and their minimization),
[PrintVerilogCodeSpec](../compiler/stages/src/test/scala/StagesSpec/PrintVerilogCodeSpec.scala) and
[PrintVHDLCodeSpec](../compiler/stages/src/test/scala/StagesSpec/PrintVHDLCodeSpec.scala) (the
per-dialect printing), and
[ElaborationChecksSpec](../lib/src/test/scala/ElaborationChecksSpec.scala) (the rejections that
stay rejections).

Related: [scoping.md](scoping.md) for the capability that lets a body hold an assertion at all, and
[initial-blocks.md](initial-blocks.md) for the other construct that reaches a Verilog `initial`
block.

## 1. The three-way

Every width relation has three answers, and the middle one is what this document is about.

| Answer | What happens |
|---|---|
| provably held, for every valid parameter assignment | nothing, the operation stands |
| provably violated, for every valid assignment | an error, at compile time over resolved widths and at elaboration over parametric ones |
| undecidable | the operation stands, and the relation becomes a constraint of the design |

The first two are unchanged by anything here: `u8 := u16` is a compile error and `x(W) := y(2 * W)`
an elaboration error, because both are violated whatever the parameters turn out to be. Only the
third answer is new, and it replaces two older behaviours that disagreed with each other: some
sites rejected an undecidable relation outright, demanding a `.resize` of code that is very
probably correct, while most assumed it silently.

The rule for whether an undecidable relation generates anything is that it must be
**load-bearing**: the construction elaboration chose has to be correct only under it.

- A leaf resize consumes an assumption. `z(16) := x(W)` extends only if `16 >= W`, and truncates
  otherwise, which is the data loss the language exists to catch.
- A target-context-widened `+`, `-`, `*` cone consumes none. Truncation commutes with those, so
  re-evaluating at the target agrees with the narrow evaluation whether the target turns out wider
  or narrower. A `<<` is a multiplication and commutes too.
- A widened `>>` consumes one: `(x mod 2^t) >> k` is not `(x >> k) mod 2^t`, so the agreement rests
  on the target being at least as wide as the operand.
- A type bound consumes one as well (`UInt(N)` is a legal type only under `N >= 1`), and generates
  nothing all the same. See §7.

Deriving what to assert from what was assumed, rather than from what was undecidable, is what keeps
the count low before any minimization.

## 2. Static assertions

A generated constraint is an assertion whose condition and message are constant, and that species
is first-class rather than a private form.

**Definition**, derived structurally in `TextOut.isStaticAssert`
([DFValAnalysis.scala](../compiler/ir/src/main/scala/dfhdl/compiler/analysis/DFValAnalysis.scala)):
a `TextOut` whose op is `Assert`, that is **directly owned by a `DFDomainOwner`** (a design or
domain body, an HDL method's block excluded, that being procedural), and whose assertion guard and
every message argument is `isConst`. Nothing is stored: no IR op of its own, no marker tag. A
user-written `assert` with a constant condition is a static assertion by construction, with no API
of its own, and its printed form stays `assert(cond, msg)`.

The position half of the definition is what makes the species printable. The elaboration-time forms
of §6 exist only in concurrent position; an assert meeting the constant criteria inside a process,
a conditional block or a loop is procedural content and keeps its procedural printing.

Three consequences downstream:

- **`ToED`** exempts static asserts and their constant cones from the process sweep
  ([ToED.scala](../compiler/stages/src/main/scala/dfhdl/compiler/stages/ToED.scala)), so an RT or
  DF body's contract survives lowering as a body member instead of firing per evaluation.
- **`DB.textOutCheck`** rejects any other concurrent text output under an ED domain. A runtime
  statement needs a runtime, and a concurrent ED position has none to offer, the sweep that gives
  an RT body statement one not applying to a body that is already ED. So the user is told at
  elaboration rather than discovering it in illegal generated HDL.
- **`OrderMembers`** ranks static asserts, and the anonymous members computing their conditions,
  immediately after the constant declarations, so the emitted output reads them as a contract
  header.

**DFacsimile** evaluates a static assert ONCE at time zero (`once = t.isStaticAssert` in
`buildTextOut`), the simulation analogue of elaboration time, rather than per committed cycle.

## 3. The mechanism

### The guard is the record

A pending constraint is not a side table. It is **the condition value itself, tagged**
`ir.AutoConstraint`, a marker with no payload. Everything else a constraint needs the guard already
carries:

- **its origin** is the guard's own `meta.position`, the guard being built under the DFC of the
  operation that assumed it, so nothing can disagree with where it came from;
- **collection** is a scan of the design context's member list in member order, which is the
  elaboration order of the operations, so the assertions come out in source order with no sorting;
- **its lifetime** is already managed, a guard nothing reads being an unread anonymous value that
  the end-of-design sweep collects.

Two invariants keep the tag honest. It goes on a FRESH anonymous value, tagging something already
read being both a leak past the sweep and a tag carried into the snapshot. And it never survives
materialization, so no member of a finished design has one.

This is why it is neither of the two things a tag must not be. It is not a structural property
that belongs in a `Modifier`, and it is not a stage marker that a later stage reads: it is created
and consumed inside one design's elaboration, before that design's member snapshot exists.

`raise` records nothing under meta-programming (a stage transforms an already-elaborated design and
assumes nothing of its own) or at global scope (no body to state it in), and nothing for a
condition that folded to a constant `true`, which requires nothing. One that folded to `false` is
kept: an assumption that cannot hold is worth the noise.

### Retraction

`raiseFor` ties a constraint to the VALUE that makes it, recorded in
`DesignContext.autoConstraintOf`, and `retract` drops it. An assumption is normally the design's
for good, the operation that made it being a statement of the body. An anonymous operand is not:
target-context widening re-evaluates a whole expression at the target's width
([CarryPromote.scala](../core/src/main/scala/dfhdl/core/CarryPromote.scala)), discarding the narrow
form of every operand in it, and an assumption only that narrow form needed goes with it.

### Materialization

At the end of the design body, under the body's own DFC and before `dfc.exitOwner()`
([Design.scala](../core/src/main/scala/dfhdl/core/Design.scala)), `materialize` collects the tagged
guards, minimizes them (§4), and for each survivor plants a static assert at the tail of the body.

It does not reference the guard it was handed. The check fires wherever the user wrote the
operation, so a guard built inside an `if` is OWNED by that block: perfectly constant, and simply
not accessible from where the assertion belongs, reading a block-owned value from the body being
what `DB.blockScopeCheck` rejects. So the guard's cone is CLONED into the body
(`cloneAnonValueAndDepsHere`) and the assertion made over the clone. Three things follow: the
original is then read by nothing and the sweep collects it, which is also what makes a
minimized-away constraint free; deduplication must compare the constraints rather than the members,
two identical constraints raised in different blocks being distinct cones; and a cone reading a
NAMED block-local declaration is the one shape that does not lift, which `blockScopeCheck` already
reports as the error it is.

**Message**, derived from the condition and nothing else:

```
Design parameter violation found. Expected: 16 >= W
```

The check that raised it has a message of its own, and using it is wrong on both counts. It
describes ONE operation, while the assertion describes the design's INTERFACE, minimization having
merged the assumptions of several operations into one statement. And its reader is different: an
elaboration error is read by whoever wrote the assignment, at a position Scala pins exactly, while
this is read by whoever instantiates the generated module, where naming a DFHDL assignment says
nothing actionable. What that reader needs is the relation to satisfy.

**Severity** is `Fatal`. A violated width contract invalidates everything downstream, and the
elaboration-time forms abort cheaply.

**Naming**: `constraint_0`, `constraint_1`, and so on, enumerated at materialization rather than
left to `UniqueNames`, the printed DFHDL being source and two `val constraint = ...` bindings in one
body not re-elaborating. The enumeration starts at the first constraint even when a design has only
one, because the bare `constraint` is a SystemVerilog keyword that cannot label a generate block.

**Design parameters stay unfolded** in the emitted condition. The generated HDL keeps parameters
overridable, which is the entire point of checking at the instantiation.

### Every design states its own contract

A design parameter is never resolved to decide a relation about the design that declares it, and
least of all to its DEFAULT, which is what the parameter is when nothing says otherwise rather than
what it is. The generated module keeps the parameter overridable, from a DFHDL parent or from
hand-written HDL, so what a body assumes has to hold for whatever that parameter turns out to be. A
decision made on one instantiation's value is not a decision about the design.

So a sub-design's parameters stay symbolic while its body elaborates, exactly as the elaboration
root's do, and a child elaborates to the same thing it would standalone. `IntExprCalc`'s
`AppliedExpr` mode substitutes a parameter only where an instantiation actually supplies a value,
which during the design's own body is never, no instance existing yet, and for the root is never at
all. Substituting the default in its place decided relations on a value the design did not have, in
both directions: rejecting an operation the applied value made legal, and accepting one it did not.

A decision made in the PARENT is a different matter. There the applied value is what the connection
or the operation is really about, and the instantiation is resolvable, so it resolves.

## 4. Minimization

Every comparison normalizes onto ONE canonical form, `IntExprCalc.linearDiff(lhs, rhs) >= 0`, so
`W + W >= 8` and `2 * W >= 8` are one relation, and a user's `W <= 8` is comparable with a generated
`16 >= W` without either being rewritten. A strict comparison is the non-strict one over integers,
one tighter. Never textual comparison.

Two relations are comparable exactly when their symbolic terms cancel (`constOffsetDiff`), and then
the one with the smaller constant is the stronger: `x - y >= 0` means `y >= 0` implies `x >= 0`. So
`8 >= W` subsumes `16 >= W`, and `W >= 6` subsumes `W >= 2`. Nonlinear widths participate as opaque
bases, so same-base constraints minimize identically.

Materialization keeps a constraint only when nothing already kept implies it, and drops anything
kept that IT implies. Deduplication falls out as the case where two constraints imply each other.

A constraint with no comparable form takes no part beyond structural deduplication. A user's
assertion may be anything at all, and a generated multi-part requirement is a conjunction rather
than a relation.

### The user's own assertions participate

A user-written static assertion of severity `Error` or `Fatal` is a design contract like a
generated one, so minimization reads its condition as an input. `Info` and `Warning` do not: they
report, they do not constrain.

The relation is one-way, and that asymmetry is the point. A user assertion is NEVER removed,
subsumed or rewritten: the user wrote it, so it stays as written, in its own position, with its own
message and severity. An AUTO constraint IS removed when a user assertion implies it. Having
written `assert(W >= 8, ...)`, the user should not then read a generated `W >= 1` next to it.

## 5. Which checks generate

An undecided `Check` is the source. A `Check1`/`Check2` instance is applied over `Int`s, so a check
whose arguments do not fold to literals never runs, and its undecided arm is where the assumption
is made. One elaboration-half helper per check family sits beside the check the family already has,
so a family is wired once and every site reaching its undecided arm goes through it.

| Site | Helper | States |
|---|---|---|
| assignment, connection, and the LHS-dominant `-`, `/`, `%` | `widthFitCheck` | `LW >= RW'` |
| a wildcard `Int` with a known minimum width adapting | `wildcardFitCheck` | `baWidth >= wcWidth` |
| a wildcard `Int` whose VALUE does not resolve | `wildcardValueFitCheck` | see below |
| a width-adjustment permission (`.extend`, `.truncate`) | `permitsWidthAdjust` | the direction the permission covers |
| a widened `>>` | `AutoConstraint.raiseUndecidedFit` | `target >= operand` |

**The LHS-dominant three** take the LHS width and convert the RHS to it, so all three need the same
fit and take the same three answers. `-` used to reject the undecided one outright, which answered
"cannot tell" with "no" for one operation in three.

**A wildcard whose value does not resolve** is every manifestation of an overridable parameter:
neither its width nor its sign is known, for this elaboration or any other, so there is nothing to
compare and the bound is on the VALUE. It is the same relation, stated as the width that value
needs, which is `clog2(v + 1)` bits for an unsigned `v` and `clog2(max(v + 1, -v)) + 1` for a signed
one, both exactly and for every `v`. Through `clog2` rather than as `v <= 2 ** width - 1`
deliberately: a 64-bit target would overflow the 32-bit integer arithmetic the generated HDL
evaluates the contract in. The sign being unknown too, an unsigned target adds `v >= 0` in the same
constraint, the halves being what one adaptation needs together. Nothing is stated for a target that
is itself a wildcard, which adapts to nothing, nor for a wildcard the body cannot read, a `for`
iterator having no value in the finished design either.

**A permission covers the WIDTH relation and only that.** Signedness is not a permission's to give,
so it is checked either way. `.extend` and `.truncate` each cover one direction and state it where
it is undecided; the carte-blanche `.resize` covers both and states nothing, which makes it the one
spelling under which a widened `>>` has no other guard.

**A `.truncate` also decides against widening.** It states that the target is narrower, the exact
contradiction of what an undecided comparison optimistically assumes, so where the widths cannot be
compared the author's statement is what decides and the value keeps its own width. Only there: a
permission whose direction does not apply contributes nothing, so a provably wider target widens as
it always did.

**A comparison** has two undecided cases and only one generates. A wildcard `Int` argument adapts to
the receiver, so it states the fit it needs. Two bit-accurate operands are held to EQUAL widths, and
an unprovable pair is REJECTED: a comparison has no adaptation semantics to assume anything for, so
the resize it would otherwise emit is not a semantics worth asserting, it is the silent truncation
the equality rule exists to prevent.

### What a `max` decides away

A `max` is at least each of its own branches and a `min` at most each of its, so a comparison
between such a chain and one of its own branches is either an answer or a comparison with what is
left of the chain. The shape is what a width taken as the COMMON width of two operands meets when it
comes back to one of them, which every binary operation over unrelated parametric widths does.

The identity is asked at two levels and implemented at both. `SimplifyFunc.CompareAgainstMaxMin`
rewrites the expression, so a design holding `x(W1) + y(W2)` in `W1` bits states `W1 >= W2` rather
than restating the common width it went through. `IntExprCalc`'s `dominatesByBranch` is a fallback
in `widthFitCompare`, so `max(W1, W2) >= W1` is proven rather than left undecided; it runs last, so
it only ever turns an undecided answer into a decided one and never overrides the max/min
elimination, which reads a mixed chain by its constants and is deliberately lenient.

Proving it is also what lets a stacked resize through a common width fold away, `toDFXIntOf`'s
`unstack` asking whether the inner resize loses anything rather than whether it strictly widens.

## 6. Printing

The key is position: a static assert in CONCURRENT position prints as an elaboration-time
construct, and in procedural position as it always did.

| Backend | Concurrent form |
|---|---|
| VHDL v93 / v2008 / v2019 | concurrent `assert COND report MSG severity S;`, a static expression, so it fires at elaboration |
| SystemVerilog sv2009 and later | `if (!(COND)) $fatal(1, MSG);` at module scope: a generate-`if` with an elaboration system task (IEEE 1800-2009 par. 20.11), so synthesis and simulation both catch it at elaboration |
| sv2005 | `initial assert (COND) else $error(MSG);`, immediate asserts and severity tasks being procedural in 1800-2005, checked at simulation time zero |
| v95 / v2001 | `initial if (!(COND)) begin $display(...); $finish; end`, an `initial` block being a module item where a bare statement is not |

Verilog names a block, not a statement, so a named assertion becomes a named block: the generate
block of the elaboration form, or the `initial` block of the others. Naming the generate block is
also what keeps a linter from complaining about the implicit `genblk<n>` the LRM would otherwise
assign.

## 7. Adding a check

1. Find the undecided arm. It is the `case _ =>` where a `Check` over `Int`s could not run because
   a width did not fold.
2. Decide whether the relation is LOAD-BEARING there (§1). If the construction is correct whatever
   the relation turns out to be, state nothing.
3. Discharge before stating: `AutoConstraint.widthFitGE` decides the two decidable answers, and
   only `None` becomes a constraint. A provably violated relation stays the check's own hard error,
   with the check's own message.
4. State it with `raise`, or with `raiseFor` if the value it constrains is an anonymous operand
   something else can supersede.
5. Nothing on this path may mint an `IntParamRef` for a width it is holding. A reference no member
   holds has no origin, and the printer resolves a name relative to the origin's owner, so
   rendering through a freshly minted one dies with a missing-ref lookup. Both the rendering and the
   discharge work from the width VALUE (`IntParam.errorString`, `IntExprCalc.widthFitCompare`).
6. Add the generated form to
   [PrintCodeStringSpec](../compiler/stages/src/test/scala/StagesSpec/PrintCodeStringSpec.scala),
   and the surviving rejection to
   [ElaborationChecksSpec](../lib/src/test/scala/ElaborationChecksSpec.scala). Expect reference HDL
   churn under `lib/src/test/resources/ref/` and review it deliberately: a new constraint on a
   documented example is a user-visible change.

## 8. Known gaps

- **Type-construction bounds generate nothing.** The `toScalaIntOpt.foreach(check(_))` sites across
  `DFBits.scala`, `DFDecimal.scala` and `DFVector.scala` have exactly the same shape of undecided
  arm and are the widest source of unprovable predicates: width positivity, signed width,
  `.until`/`.to` bounds, `repeat`/`eby` positivity, sel widths, vector cell dims, `UBArg` bounds.
  `SInt(W)` appears in essentially every parametric declaration, so asserting each construction
  would put a wall of `W >= 2`-class contracts in front of the ones that say something about the
  design, and those bounds are also the likeliest to be implied by a constraint generated elsewhere.
  Nothing about the mechanism needs to change if that judgement is revisited.
- **A signed width restates its own type bound.** `widthFitCompare`'s proof knows only that a width
  is `>= 1`, so `a + b + 1` over `SInt(W)` states `W >= 2`, which `SInt(W)` already guarantees.
  Discharging those means giving the proof the operand's signedness, which the shared
  `widthFitCompare(a, b)` does not take.
- **`Bits` strictness stays a hard reject.** `Bits` is the strict type by design and has no
  adaptation semantics to assert an assumption for. If ever relaxed, the mechanism extends
  trivially, but that is a separate decision.
- **No suppression option.** There is no elaboration option or annotation to turn the constraints
  off for a user who wants lean output. The assertions are the feature.
- **No user documentation.** Nothing under `docs/` describes any of this, even though the generated
  assertions appear both in the elaborated design and in the emitted HDL, `Blinker` and `UART_Tx`
  included.
