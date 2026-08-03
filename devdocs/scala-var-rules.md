# Scala `var` Rules

A Scala `var` holding a DFHDL value is rebound during **elaboration**, while a DFHDL variable is
assigned at **runtime** with `:=`. The two look alike and mean different things, so a `var` is
admitted only in the positions where it cannot express something the elaboration is unable to
honour, and rejected everywhere else. There is no relaxation flag: an allowed position is safe by
construction.

The compile-time rules are in
[ScalaVarPhase.scala](../plugin/src/main/scala/plugin/ScalaVarPhase.scala), covered by
[ScalaVarSpec](../core/src/test/scala/CoreSpec/ScalaVarSpec.scala). The elaboration backstop is
`DB.blockScopeCheck`, covered by
[ElaborationChecksSpec](../lib/src/test/scala/ElaborationChecksSpec.scala). The user-facing write-up
is [docs/user-guide/type-system](../docs/user-guide/type-system/index.md#scala-var).

This replaced a blanket "highly discouraged" warning in `MetaContextGenPhase.getMetaInfo`. Issue
[#433](https://github.com/DFiantHDL/DFHDL/issues/433) came from an agent that read that warning and
proceeded: the result compiled, reported `COMPILE_OK`, and emitted SystemVerilog referencing a loop
iterator outside its loop. A `var` accumulated across a DFHDL loop is too tempting to leave behind a
warning, and the warning's advice ("Consider changing to `val`") does not even apply to an
accumulator, which is a good part of why it read as boilerplate.

## 1. The permission list

| # | Rule |
|---|---|
| 1 | A **constant** `var` inside a `simulation { … }` host block is exempt from rules 2 and 3 |
| 2 | No `var` **declaration** inside a sequential scope |
| 3 | No `var` **access** from a sequential scope, nor from inside a **named** method |
| 4 | A DFHDL `var` must be `private` or local |
| 5 | A DFHDL `var` must be ascribed `T <> VAL` or `T <> CONST` |
| 6 | No `var` holding a design, domain, or interface instance |
| 7 | Nothing may read, from outside a DFHDL block, a declaration made inside it |

Rules 1 to 6 are the plugin's; rule 7 is the elaboration backstop, because the plugin's view is
lexical and can be laundered. Same split as `wait` and `initial` blocks.

A `var` is subject to any of this only when it holds a DFHDL value: `DFVal`-typed for rules 2 to 5,
`Container`-derived (a design, domain, or interface) for rule 6. Everything else is plain Scala
state and is untouched.

The one legitimate idiom the list exists to preserve is elaboration-time accumulation in a
concurrent body:

```scala
class Foo extends EDDesign:
  private var acc: Bits[Int] <> VAL = lanes(0).bits
  for (i <- 1 until 4) acc = acc ++ lanes(i).bits  // Scala range in an ED body, unrolled
  val word = acc                                   // accumulation ends here
  process(all):
    y := word
```

## 2. The sequential/concurrent boundary

The line is the sequential/concurrent split, and **the only concurrent scope is an ED design or
domain body**. RT and DF bodies are sequential, as are processes, `initial` blocks, and method
bodies. A sequential scope is elaborated once, not once per execution, so a `var` there cannot
accumulate across a loop.

The plugin answers "am I in a sequential scope?" by scanning `ownersIterator`, the technique
`isWaivedSimConstVar` already used for `SimCtx`. Both halves are visible that way, and neither
needed new API.

**Sequential blocks** arrive as **context functions**, so the scope is a real parameter on an
anonymous method:

```scala
def apply(all: …)(block: DFC.Scope.Process ?=> Unit)          // Process.scala
def initial(block: DFC.Scope.Initial ?=> Unit)                // Process.scala
case Unit => (DFC, DomainType.ED, DFC.Scope.Procedural) ?=> … // the `<> EDRET` match type
case Any  => (DFC, DomainType.ED, DFC.Scope.Function)   ?=> …
```

So: an owner that `is(Method)` with a parameter conforming to `DFC.Scope.Sequence` or
`DFC.Scope.Function`. `Function` has to be named separately, since it mixes its capabilities
directly rather than extending the `Sequence` bundle.

**Sequential domain bodies** are class bodies, not context functions, but `Container` carries the
discriminator as a type member:

```scala
private[core] type TDomain <: DomainType                       // Container.scala:12-14
abstract class DomainContainer[D <: DomainType](domainType: D):
  private[core] type TDomain = D                               // Container.scala:24-25
abstract class EDDesign extends DomainContainer(DomainType.ED), Design   // Design.scala:375
```

So: an owner that is a class deriving from `Container` whose `TDomain` is `DomainType.ED`.

Scan outward and take the **first** hit of either kind: the innermost owner decides.

Reading a parameter's declared type is also what keeps this sound. The Scala-side form of the same
question is booby-trapped: `Scope.Function`'s given is ambient, so a plain summon of any capability
it has succeeds from anywhere, which is what silently disabled the concurrent-`:=` rejection in
`538d6d904` (fixed in `a16fdb308`). If any part of this ever moves Scala-side, it must take the
scope as a **type parameter and subtype-test it** (see [scoping.md](scoping.md) §3).

### 2.1 `DomainType.ED` must be compared by SYMBOL, never by `<:<`

`DomainType.ED`, `.RT` and `.DF` are opaque aliases of the same `DomainType.Dynamic`, and **opacity
is already gone by the time a plugin phase runs**, so every pair of them conforms mutually.
`tdomain <:< DomainType.ED` is therefore `true` for an RT design body, which silently classifies
every RT and DF body as concurrent. The ED cases stay correct either way, so only an RT test catches
it. Compare `hiBound.typeSymbol` against the `ED` symbol.

### 2.2 A sequential block is a SYNTHETIC anonymous method

The barrier walk of rule 3 excludes synthetic and anonymous owners, on the grounds that only a
user-written `def` can launder. But a `process(all): …` closure is both, so those exclusions must
come **after** the scope test, not before it, or no access from a process is ever flagged. The
exclusions belong to the named-method clause alone.

## 3. Access, and why the rule says *named* method

Rule 3 covers **all** access, read included, and extends past scopes to any **named** method. A read
is not dangerous in itself, but allowing it means the rule has to reason about whether the `var` is
still being accumulated, and the method clause is what closes the laundering hole a purely lexical
check would otherwise leave open:

```scala
private var acc: UInt[8] <> VAL = a
def bump(): Unit = acc = acc + 1     // rejected here, at the def
process(all):
  for (i <- 0 until 4) bump()        // would otherwise execute inside a hardware loop
```

The cost is that an accumulator's result cannot be read straight from a process. The fix is a `val`
that freezes it, which is a better thing to have written anyway, since it marks where accumulation
ends (`word` in §1).

Rule 2 is a deliberate over-approximation on top: a `var` inside a sequential scope is either never
reassigned, in which case it is a misspelled `val`, or it is reassigned, in which case rule 3 would
reject it anyway. Banning the declaration outright gives a better error position than waiting for
the access.

**Named, because a lambda is not a laundering vehicle.** `for (i <- 1 until 4) acc = acc ++
lanes(i).bits` over a Scala `Range` desugars to `(1 until 4).foreach(i => acc = acc ++
lanes(i).bits)`, so the accumulation itself sits inside an anonymous function, and it still is one
at this phase. Phrased as "any method owner between the access and the `var`", the rule would reject
the very idiom of §1, and `var` would collapse to straight-line rebinding only. The distinguishing
property is not "is there a method between" but "can this method be invoked from a scope other than
the one it is defined in": a named `def` can, a lambda handed to `Range.foreach` runs in place. The
residual leak (a lambda stored in a `val` and invoked later) is what rule 7 backstops.

## 4. `private` or local

A public `var` member of a design is reassignable from outside the design after elaboration, and it
participates in the design's `reflect.Selectable` surface. Both are nonsense. `private` members, and
`var`s local to a method or block, are unaffected.

**`private` only, not `protected`:** a `protected var` is still reachable from a subclass of the
design, which is the same leak with an extra step.

## 5. Ascription: `<> VAL` or `<> CONST`

Three reasons, and the third is the one that names the rule.

**It is what makes the legitimate idiom expressible.** Inference takes the type from the
initializer, so `var acc = lanes(0).bits` infers `Bits[8]` and the next `acc = acc ++ lanes(1).bits`
is a width error. `var acc: Bits[Int] <> VAL = …` is the only spelling that works, which turns the
accumulator's width from an accident of the first assignment into a decision.

**An inferred type smuggles the declaration site's modifier.** `evPortVarConstructor` builds the
result modifier as `A & SC & DT`, so an inferred `var` type remembers assignability, the scope and
the domain of whatever initialised it, and every later use is guarded against those markers no
matter what the `var` currently holds. `NotREG`, `VarOnly` and `` `InsideProcess:=` `` all read
them. Ascription cuts the channel.

**It stops the two assignment forms meeting on one name.** `<> VAL` and `<> CONST` are the only two
non-assignable spellings. Any other ascription (a variable or a port) would let one name be rebound
with `=` during elaboration and assigned with `:=` at runtime, which is the confusion the whole
permission list exists to prevent. At the type level this is one test: `T <> VAL` is
`DFVal[_, Modifier[Any, Any, Any, Any]]` and `T <> CONST` is
`DFVal[_, Modifier[Any, Any, Any, ISCONST[true]]]`, while every declaration modifier narrows at
least one of the first three arguments.

**A trade-off the error message states outright:** the ascription that makes an accumulator compile
is `Bits[Int] <> VAL`, and an unbounded `Int` width is checked at elaboration rather than at compile
time (see the width-collapse work behind
[#431](https://github.com/DFiantHDL/DFHDL/issues/431)). The rule buys intent and stable typing, not
more static width safety, and the message must not let it read as a safety upgrade. The message also
prints the exact ascription to write, by re-showing the inferred type with the `VAL` (or `CONST`)
modifier substituted in, so the fix is copy-paste.

## 6. The simulation waiver

A **constant** `var` inside a `simulation { … }` host block (detected by a `SimCtx` parameter in the
owner chain) is plain testbench Scala: nothing there is elaborated, so the scope rules have nothing
to protect. It is what lets a testbench keep a reference model in a `var` and update it with
constant arithmetic, including from a helper `def` inside the same block, which rule 3 would
otherwise reject.

The waiver covers rules 2 and 3 only. Rules 4, 5 and 6 still apply, and a testbench `var` satisfies
them on its own once it is ascribed. That ascription was the whole in-repo migration when the rules
landed (seven sites, all reference models in `simulation` blocks), and it is an improvement:
constness becomes a property the author **declared** rather than one inferred from the initializer.

## 7. The elaboration backstop

`DB.blockScopeCheck` rejects a read, from outside a DFHDL block, of a declaration made inside it.
Being in `DB.subDBCheck`, it binds every stage through `SanityCheck` as well as the user. Its
design notes:

- an anonymous value is **transparent**: it has no place of its own, because the printers emit it
  inline at its reader, so the question is not where it sits but what it reads, asked from the
  reader's position. A rule phrased over "member owned by a block" flags shapes that are harmless;
- a `for` iterator's `Dcl` is owned by the **enclosing** block in the IR while every backend emits
  it in the loop header, so it needs an explicit scope override;
- walk owners with `ownerRef.get`, never `getOwner`, which throws for globals, the top design and
  the `Goto` step placeholders.

What it catches that the plugin cannot: the plugin's view is lexical and keyed on a Scala name being
rebound, so **any other Scala-level container** carries a loop-internal value out just as well. A
mutable collection held in a plain `val` needs no `var` at all:

```scala
class Top extends EDDesign:
  private val picked = collection.mutable.ArrayBuffer.empty[UInt[8] <> VAL]
  process(all):
    for (i <- 0 until 4)
      picked += lanes(i)
    last := picked.last    // scope error: reads `i` from outside its loop
```

That is the `ElaborationChecksSpec` case. It had to be written that way: the original repro was
issue #433's own `var`-in-a-process, which the plugin now rejects before elaboration ever runs.

**One shape that looks like a hole and is not.** A `var` declared and read entirely inside a plain
elaboration-time `def` passes rules 2 and 3 (the def's own enclosing scope is the concurrent design
body), and the def's body does run at its call site. But whether `for (i <- 0 until 4)` is a
hardware loop or a Scala range is decided **lexically**, by the capabilities of the scope the `for`
is written in. Written at design-body level the range stays a Scala `Range` and unrolls during
elaboration, no matter where the def is called from, so the accumulation is simply correct.

## 8. Where each rule is enforced

| Rule | Where |
|---|---|
| 1 waiver | `ScalaVarPhase.isWaivedSimConstVar` |
| 2 declaration scope | `ScalaVarPhase.transformValDef` → `enclosingScopeKind` |
| 3 access | `ScalaVarPhase.transformIdent` / `transformSelect` → `barrierBetween` |
| 4 `private` | `ScalaVarPhase.transformValDef` |
| 5 ascription | `ScalaVarPhase.transformValDef` → `isInferredTpt`, `isValOrConstTpe` |
| 6 no instance | `ScalaVarPhase.transformValDef` → `isContainerTpe` |
| 7 backstop | `DB.blockScopeCheck`, called from `DB.subDBCheck` |

The phase is registered in `Plugin.initialize` **and** in `PluginTestPhase.freshPluginPhases`;
without the second, `assertPluginError` does not see its diagnostics.

## 9. Tests

- Plugin errors: `assertPluginError` under `-P:dfhdl.plugin:testing`, in
  [ScalaVarSpec](../core/src/test/scala/CoreSpec/ScalaVarSpec.scala). See
  [plugin-error-testing.md](plugin-error-testing.md).
- The **positive controls** in that spec carry as much weight as the rejections: the accumulator
  idiom and a `var` local to a plain `def` are written as ordinary spec code, so a rule that grows
  too wide fails the spec's own compilation, while a rule that stops firing shows up as
  "No error found".
- Rule 7: [ElaborationChecksSpec](../lib/src/test/scala/ElaborationChecksSpec.scala).

## 10. Open issues

- Should a DFHDL `var` that is **never reassigned** be banned everywhere, rather than only inside a
  sequential scope? It is identical to a `val` in both places, so the split is hard to justify to a
  user, and "change to `val`" is exactly right for it. Left out because it needs a reassignment
  scan the other six rules do not.
- Rules 2 to 5 key on a `DFVal`-typed `var`. A `var` holding a **collection** of DFHDL values
  (`var acc: List[UInt[8] <> VAL]`) is not a DFHDL `var` by that test and passes untouched. Rule 7
  is the only thing covering it, and only when the shape actually escapes a block.

## 11. Key file references

| What | Where |
|---|---|
| The permission list | [ScalaVarPhase.scala](../plugin/src/main/scala/plugin/ScalaVarPhase.scala) |
| The sequential-scope predicate, Scala side (`:=` guard) | [DFVal.scala:1858-1868](../core/src/main/scala/dfhdl/core/DFVal.scala#L1858-L1868) |
| The scope lattice | [DFC.scala:180-323](../core/src/main/scala/dfhdl/core/DFC.scala#L180-L323) |
| `TDomain`, the ED-vs-sequential discriminator on a container | [Container.scala:12-27](../core/src/main/scala/dfhdl/core/Container.scala#L12-L27) |
| Scope arriving as a context-function parameter | [Process.scala:74-101](../core/src/main/scala/dfhdl/core/Process.scala#L74-L101), [DFVal.scala:132-134](../core/src/main/scala/dfhdl/core/DFVal.scala#L132-L134) |
| Guard forms and their three failure modes | [scoping.md](scoping.md) §3 |
| Per-design elaboration checks | `DB.subDBCheck` in [DB.scala](../compiler/ir/src/main/scala/dfhdl/compiler/ir/DB.scala) |
| Plugin error testing | [plugin-error-testing.md](plugin-error-testing.md) |
