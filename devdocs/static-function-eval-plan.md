# Static Function Evaluation (the folding interpreter)

Status: PLAN ONLY, nothing implemented. This works out the deferred "folding interpreter" listed in
[methods.md](methods.md) §11. Read that document first: it establishes the `Func(Op.Def)` call model
and the formals-as-static-ports model that everything here rests on.

## 0. What this is, and what it is not

It is an **analysis, not a rewrite**. No stage turns a `Func(Op.Def)` into a `DFVal.Const`, and the
emitted HDL keeps calling the function. The whole change is that `getConstData` answers
`KnownConst` where it answers `UnknownConst` today.

There is exactly one seam, already marked in the code:

```scala
// DFMember.scala:761-765
case Func.Op.Def(staticRef) =>
  if dfType == DFUnit then ConstData.NotConst
  // static function always returns a constant. currently we don't evaluate it during elaboration.
  else ConstData.UnknownConst(this)
```

Everything below is about what has to exist behind that `else`.

## 1. The invariant that must not break

**The body stays data-blind.** `Dcl.protGetConstData` ([DFMember.scala:690-706]) keeps returning
`UnknownConst` for a declaration owned by a static function. The interpreter carries its **own**
environment and never writes call-site data back into the shared body members.

This is load-bearing, not stylistic. Per the static-domain plan's Status item 2, one body is emitted
per design-load key. If a formal could resolve to its actual, then `n.toScalaInt` inside a body
would succeed, a Scala-level branch on `n` would specialize the body per call site, and the
divergence hole that the current model closes *by construction* reopens. So:

> Evaluation is a call-site-outward operation. It reads the body; the body never reads it.

Corollary: the interpreter cannot reuse `getConstData` for members **inside** the callee, both
because of the invariant and because that cache is per-member and call-site-independent
([DFMember.scala:233-256]). It needs its own env-sensitive evaluator. It *does* delegate to
`getConstData` for values **outside** the body (globals, captured constants, the actual args).

## 2. Architecture

One new IR-level file, `compiler/ir/src/main/scala/dfhdl/compiler/ir/StaticEval.scala`. IR-level
because the seam is in the IR and it must be able to run during elaboration.

```scala
object StaticEval:
  def evalCall(call: DFVal.Func, key: StaticRef)(using MemberGetSet, ConstData.CachePolicy)
    : ConstData[Any]
```

Four pieces:

| Piece | Role |
|---|---|
| `Env` | `mutable.Map[DFVal.Dcl, Data]` for formals, locals and loop iterators |
| `evalVal(v: DFVal): Result[Data]` | expression evaluation over the ref graph, env-sensitive |
| `exec(members: List[DFMember]): Result[Unit]` | statement execution in member order |
| `assign(target: DFVal, newData: Data): Result[Unit]` | the write path (§5) |

`evalCall` is **total**: every construct it cannot handle degrades to `UnknownConst(call)`, which is
exactly today's answer. That makes every phase strictly monotone. It can only turn Unknowns into
Knowns, never break a path that works today.

Binding: the callee's formals are its non-phantom IN `Dcl`s in declaration order, then its phantom
ones ([r__For_Plugin.scala:274-282]), zipped positionally against `call.args`. This is the same
pairing `visibleFormalCountOf` / `methodPrinterAt` already do ([DFValPrinter.scala:170-186]), so it
should be factored out rather than written a third time.

## 3. Expression evaluation (read side)

| Member | Rule |
|---|---|
| `Dcl` in env | the env value |
| `Const` | its data |
| `Func` (non-`Def`) | recurse args, then `calcFuncData` ([DataOps.scala:117]) |
| `Func(Op.Def)` | recursive `evalCall` |
| `Alias.AsIs` | `dataConversion` |
| `Alias.ApplyRange` | `selRangeData` |
| `Alias.ApplyIdx` / `SelectField` | per-family select |
| `DFConditional.Header` in expression position | §3.1 |
| anything outside the callee subtree | fall through to its normal cached `getConstData` |
| anything else | bail to Unknown |

The four alias cases duplicate logic that already exists inside `protGetConstData`
([DFMember.scala:896-906, 992-1011, 1049-1072, 1111-1116]), but it cannot be called directly because
it routes through the *cached* `getConstData` on `relVal`. Extract those read cases into pure
`(dfType, relData, ...) => Data` helpers in `DataOps.scala`, called by both. That kills the
duplication and gives the write-side counterparts (§5) an obvious home.

The outside-the-callee test walks owners up to the callee block. It **must** use the guarded
`ownerRef.getOption` walk, not a forced `getOwnerDesign`. Forcing it is the documented RT-loop crash
class ([DFMember.scala:696-704]), and it has already bitten twice.

### 3.1 Conditionals as expressions

Before `ExplicitCondExprAssign` runs, an `if`/`match` used as a value is a `DFConditional.Header`
whose taken block's **last folded member** is its value. That is precisely the shape
`ExplicitCondExprAssign.patchChains` relies on ([ExplicitCondExprAssign.scala:84]). So: walk
`conditionalChainTable(header)`, evaluate each block's guard (if/else) or match its pattern (case),
and evaluate the taken block's last member.

This incidentally answers the two long-standing `TODO`s at [DFMember.scala:1451-1457] and
[DFMember.scala:1590] ("if the selector and all branch results are constant, the result is
constant"). **Keep it interpreter-local anyway.** Promoting it into `protGetConstData` would make
every conditional in every design start folding, which is a much wider blast radius and belongs in
its own change.

## 4. Statement execution

**The only statement in a static body is assignment.** Ports and signals cannot be declared
(`Scope.Function` grants `HasVars`, not `HasPorts`) and design instances are rejected, so no
user-written connection can exist. The one connection member in the body is the harness-created
return wiring, and it is **not** a statement (§4.0).

Walk `design.members(MemberView.Folded)` in order and act on:

- `DFNet.Assignment(to, from)` -> `assign(to, eval(from))`
- `DFConditional.Header` with `dfType == DFUnit` -> evaluate the chain, exec the taken block
- `DFLoop.DFForBlock` -> resolve the `DFRange` (`start`/`end`/`step`, `Op.Until|To`), then per value
  bind the iterator `Dcl` and exec the block. Note the iterator `Dcl` and the range members live in
  the **enclosing** scope, before the block ([DFFor.scala:27-29]). Comprehension `if` guards are
  already nested `DFIf` blocks inside the loop, so they fall out of the conditional rule for free.
- `DFLoop.DFWhileBlock` -> §4.1
- `Dcl` (local declaration) -> seed the env from `initRefList` if present, else bubble (§6)
- anonymous expression members -> no-op; they are evaluated on demand by their consumers
- anything else -> bail to Unknown

The final bullet is a backstop, not a limitation: `Wait`, `Goto`, `ProcessBlock`, `DFDesignInst`,
`TextOut`, `StepBlock`, `ForkBlock` and `DomainBlock` are all already rejected in a static body by
the plugin's `checkHDLMethodContent` and by `SanityCheck.hdlMethodCheck`
([SanityCheck.scala:353-423]).

### 4.0 The return is not a statement

The body's result reaches the caller through a `DFNet.Connection` into the `"o"` OUT port, created
by `output.connect(retIdent)` after the body is elaborated ([r__For_Plugin.scala:409-415]); that is
the member `methodReturnPort` recognizes ([DFValAnalysis.scala:20-23]).

A connection is continuous wiring, not a sequenced statement, so it stays **out** of the executor's
walk. After `exec` finishes, the result is `eval` of the return port's driver, read through
`methodReturnPort`. In practice the connection is always the last member and evaluating it in place
would give the same answer, but sequencing it would encode the wrong semantic model, and the
distinction is exactly the one that would break first if a body ever gained a second connection.

### 4.1 The while-guard subtlety

`DFWhile.plugin` takes its guard **by value**, not by name ([DFWhile.scala:19]). The guard
expression's members therefore sit in the enclosing scope **before** the `DFWhileBlock`, and the
block holds only a `guardRef` back at them. Today's HDL is correct only because the printers
*inline* that anonymous expression into the loop header
([VHDLOwnerPrinter.scala:405-411], [VerilogOwnerPrinter.scala:448-453]).

Two consequences:

1. The interpreter must re-evaluate the guard tree **on every iteration**. So: no memoization of
   expression nodes across iterations, or memoize keyed by an iteration generation counter.
2. A latent hazard worth pinning with a test while we are here: if any stage ever *names* that guard
   `Func` (`ExplicitNamedVars`, `NamedAliases`), it becomes a value computed once before the loop
   and the emitted HDL silently becomes a wrong or infinite loop. Nothing guards against that today.
   Not caused by this work, but this work is the first thing that makes the semantics explicit.

## 5. The write side, which does not exist yet

There is **no** `(oldData, subLocation, newData) => newData` helper anywhere in the repo. `DataOps`
has only `dataConversion`, `selRangeData` and `calcFuncData`, all read-side. The simulator does
partial writes on its own wide-vector representation ([DFacsimile.scala:578-626]), not on `Data`.

Recommended shape: a **recursive functional lens** mirroring the read cases.

```
assign(target, newData):
  Dcl               -> env(dcl) = newData
  ApplyIdx(rel, i)  -> assign(rel, updIdxData(rel.dfType, eval(rel), eval(i), newData))
  ApplyRange(r,h,l) -> assign(r,   updRangeData(...))
  SelectField(r, f) -> assign(r,   updFieldData(...))
  AsIs(rel)         -> assign(rel, dataConversion(rel.dfType, alias.dfType)(newData))
```

**Why the lens and not `departial` plus a bit splice.** `departial` ([DFMember.scala:373-405])
yields a bit range on the root `Dcl`, and it gives up entirely on a non-constant index
([DFMember.scala:389-392]). The lens handles a dynamic index naturally, because at eval time we know
the index's value; it keeps `DFVector`/`DFStruct` in their native `Vector`/`List` form; and it needs
no `dataToBitsData` round trip, which `DFString` and `DFNumber` cannot do at all
([DFType.scala:746-747]).

New helpers in `DataOps.scala`, one per family, mirroring the read cases: `updIdxData`,
`updRangeData`, `updFieldData`.

The `AsIs` inverse needs care: `dataConversion` is lossy in places (resize truncation at
[DataOps.scala:28-41], fixed-point fraction shift at [DataOps.scala:46-53]). Recommendation:
implement the inverse only for width-preserving and widening cases, and bail to Unknown otherwise. A
cast on the left-hand side of an assignment inside a static body is rare, and a wrong inverse is far
worse than an Unknown.

### 5.1 A pre-existing weakness this work leans on

**`getConstData` has no cycle protection.** The ready flag is set only *after* `protGetConstData`
returns ([DFMember.scala:248-251]), so it cannot act as an in-progress guard, and a cyclic ref is a
bare `StackOverflowError` with no diagnostic. The interpreter adds real recursion (nested calls,
loop bodies), so it must carry its own explicit call-depth and step budgets rather than lean on the
stack.

## 6. Bubbles, read-before-write, failure modes

DFHDL has no "uninitialized" beyond bubble. A local `<> VAR` with no init is unbound at declaration.
Two options: seed it with `dfType.createBubbleData`, or leave it unbound and bail on first read.

**Recommendation: seed with bubble.** It matches what the HDL will actually do, and it matches
`DFBits.defaultData = createBubbleData` ([DFType.scala:157]). A partially assigned var then carries
bubble in its untouched bits. Caveat: only `DFBits` has bit-granular bubble (its second
`BitVector`); every other scalar family is `Option`, i.e. all-or-nothing, so a partial assignment to
a non-Bits scalar unbubbles the whole value. That is acceptable and consistent with how
`calcFuncData` already propagates bubble in bulk ([DataOps.scala:200-202]).

Failure taxonomy, all degrading to `UnknownConst(call)`:

- unhandled member kind or op
- step, iteration or call-depth budget exceeded (a `while` whose guard never goes false is the one
  construct here that can genuinely fail to terminate)
- an unresolvable reference outside the body
- an inverse cast we do not implement

Never `NotConst`. A static function's result **is** a constant by definition. Only a non-const
argument makes a call non-const, and that is already decided before the seam is reached
([DFMember.scala:758]).

## 7. Caching, and the one real hazard

`getConstData` caches per member in two mutable `var`s ([DFMember.scala:233-256]) and, under the
default `Always` policy, caches everything except `NotConst`-during-elaboration. For a call `Func`
that is safe: a `Func`'s args are fixed, so its evaluated result is a function of the member alone.

**The hazard.** During elaboration a def block can still be an unplanted forward reference. This is
exactly what crashed the parametric-width case before `CONSTRET` returned a `DFConst` (see the
static-domain plan's Status). If the interpreter is asked too early and answers `UnknownConst`, that
Unknown is **cached**, and the call never resolves even after the body lands.

Fix, small and principled: extend the no-cache rule at [DFMember.scala:251-256] from
`isMutable && NotConst` to also cover an `UnknownConst` produced while `isMutable`. It is symmetric
with the existing rule and correct on the same grounds, that an unknown during elaboration may
become known later. Check it does not regress the deliberate `DesignParam` `UnknownConst` under
`Always` ([DFMember.scala:574]): that one is a *stable* symbolic answer, so recomputing it is a cost
and not a change. If it costs, narrow the rule to `Op.Def` calls specifically.

Separately, memoize `evalCall` on `(callee block, arg data)`. Sound because static functions are
enforced pure, and it keeps a call inside a loop body from re-interpreting per iteration.

## 8. What changes in the emitted HDL

The intended answer is **nothing**. Printers do not const-fold: the only `getConstData` uses in
printing are the wait-literal simplification ([Printer.scala:714]) and Verilog design-param defaults
([VerilogValPrinter.scala:50]). A call that now folds still prints as a call.

But `KnownConst` flowing where `UnknownConst` used to means `IntParamRef.getIntOpt` and
`DFType.widthIntOpt` start returning `Some` for static-function-derived widths
([DFRef.scala:139-149]). Anything branching on `widthIntOpt.isDefined` may switch paths, most
notably `Alias.AsIs.protGetConstData`, which currently degrades to Unknown on a symbolic width
([DFMember.scala:896-906]). That is mostly *more* folding, but it can change output.

So the enabling phase is gated on a full reference-HDL diff (`docExamplesRefUpdate` produces no
diff), with an explicit decision on any diff that does appear.

## 9. Phasing

Each phase is independently mergeable with the tree green.

0. **Pin today's behavior.** Add static-body tests for the forms that currently yield
   `UnknownConst`: local var plus assignments, partial assignment, `if`/`match`, `for`, `while`.
   Assert what they do *today* (elaborate and print, no folding). This is the regression net, and it
   fills a real gap: every static-function test body in the printer specs is a straight-line
   expression (`n + n`, `n + gK`, `twice(twice(n))`). The only exception is `StaticFunctionSpec`'s
   `sum3`, and it needs an `asInstanceOf[UInt[8] <> CONST]` to return a static var, which is worth a
   look on its own.
1. **Extract the read-side selection helpers into `DataOps`** and re-point `protGetConstData` at
   them. Pure refactor, byte-identical output.
2. **Add the write-side helpers** with `Data`-level unit tests. Nothing consumes them yet.
3. **The interpreter, straight-line only**: env, expression eval, assignment, return. Enable the
   `Op.Def` seam. This covers `sum3`-shaped bodies.
4. **Conditionals**, both statement and expression form.
5. **Loops**: `for` first, then `while` with the budget and the fresh-guard rule.
6. **Caching hardening** (§7), the `evalCall` memo, the budgets.
7. **The consumer decision** (§8 gate): what should now be allowed that was not. `toScalaInt` on a
   call result at a call site; static vars in initial blocks, which falls out of this work; any
   stage that wants the number.

## 10. Open questions

1. **Elaboration, or compile-time only?** Enabling at elaboration is what makes `twice(4).toScalaInt`
   work at a call site and what unblocks static vars in initial blocks. It is also where the
   forward-reference and caching hazard lives (§7). Compile-time only is strictly safer and still
   serves "a stage needs the number". Recommendation: both, with the §7 cache fix, but phase 3 can
   land compile-time-only first if a smaller blast radius is wanted.
2. **What is driving this now?** The design above is shaped for "make the number available at the
   call site". If the real target is static variables in initial blocks, then loops and partial
   assignment move earlier and the width path matters much less.
3. **Runaway `while` policy**: silent `UnknownConst` (safe, but a mistyped loop just quietly fails
   to fold) or an elaboration error. Recommendation: Unknown by default with a generous budget, plus
   a debug-mode warning.

[DFMember.scala:233-256]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:248-251]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:251-256]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:373-405]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:389-392]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:574]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:690-706]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:696-704]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:758]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:761-765]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:896-906]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:896-906, 992-1011, 1049-1072, 1111-1116]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:1451-1457]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DFMember.scala:1590]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFMember.scala
[DataOps.scala:28-41]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DataOps.scala
[DataOps.scala:46-53]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DataOps.scala
[DataOps.scala:117]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DataOps.scala
[DataOps.scala:200-202]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DataOps.scala
[DFRef.scala:139-149]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFRef.scala
[DFType.scala:157]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFType.scala
[DFType.scala:746-747]: ../compiler/ir/src/main/scala/dfhdl/compiler/ir/DFType.scala
[DFValAnalysis.scala:20-23]: ../compiler/ir/src/main/scala/dfhdl/compiler/analysis/DFValAnalysis.scala
[DFValPrinter.scala:170-186]: ../compiler/ir/src/main/scala/dfhdl/compiler/printing/DFValPrinter.scala
[Printer.scala:714]: ../compiler/ir/src/main/scala/dfhdl/compiler/printing/Printer.scala
[ExplicitCondExprAssign.scala:84]: ../compiler/stages/src/main/scala/dfhdl/compiler/stages/ExplicitCondExprAssign.scala
[SanityCheck.scala:353-423]: ../compiler/stages/src/main/scala/dfhdl/compiler/stages/SanityCheck.scala
[VerilogValPrinter.scala:50]: ../compiler/stages/src/main/scala/dfhdl/compiler/stages/verilog/VerilogValPrinter.scala
[VerilogOwnerPrinter.scala:448-453]: ../compiler/stages/src/main/scala/dfhdl/compiler/stages/verilog/VerilogOwnerPrinter.scala
[VHDLOwnerPrinter.scala:405-411]: ../compiler/stages/src/main/scala/dfhdl/compiler/stages/vhdl/VHDLOwnerPrinter.scala
[DFacsimile.scala:578-626]: ../compiler/stages/src/main/scala/dfhdl/sim/DFacsimile.scala
[DFFor.scala:27-29]: ../core/src/main/scala/dfhdl/core/DFFor.scala
[DFWhile.scala:19]: ../core/src/main/scala/dfhdl/core/DFWhile.scala
[r__For_Plugin.scala:274-282]: ../core/src/main/scala/dfhdl/core/r__For_Plugin.scala
[r__For_Plugin.scala:409-415]: ../core/src/main/scala/dfhdl/core/r__For_Plugin.scala
