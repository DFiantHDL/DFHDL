# Register DIN

`r.din` is the register-input handle of a `VAR.REG` / `OUT.REG` declaration under an RT domain. It
is both written and read:

```scala
val r = UInt(8) <> VAR.REG init 0
r.din := r.din + 1
r.din := r.din + 1        // r advances by 2 per cycle
```

The write side is the original construct and its IR is unchanged. The read side is the later
addition, and everything below exists to keep the write side untouched while making the read side
mean the right thing.

## 1. Semantics

A DIN read yields the register's **pending next-cycle value**: the latest value committed to it so
far in the current cycle body, or the register's own value when nothing has been assigned yet. It is
cycle-scoped and **position-sensitive**, exactly like a VHDL process variable: two reads of `r.din`
at different points in the body can differ.

* Partial forms read the corresponding slice of the pending value: `r(5, 0).din`, `pixel.x.din`.
  Note the selection comes first: `r.din(5, 0)` does not resolve (see §3).
* `.din` is RT-only, enforced by the existing `RTDomainOnly` guard.
* A DIN read cannot be named (§3).

## 2. Frontend: reads cost nothing when you only write

`.din` returns `REG_DIN[T]` (`core/.../DFVal.scala`), which holds the register value and builds the
read alias lazily:

```scala
final class REG_DIN[T <: DFTypeAny](val relVal: DFVarOf[T])(using dfc: DFC):
  trydf { if (!dfc.isAnonymous) throw new IllegalArgumentException(REG_DIN.namedErrMsg) }(...)
  lazy val dinVal: DFValOf[T] = DFVal.Alias.RegDIN(relVal)
```

`:=` goes through `relVal` and never touches `dinVal`, so `r.din := x` emits exactly the same
`DFNet.Assignment(regDcl, rhs)` it always has and **constructs no IR member at all**. Only a read
forces `dinVal`, which is where the `Alias.RegDIN` member is created and added to the design.

Keeping the assignment IR unchanged is not cosmetic: `departialDcl`, `ToED.addDinRef`,
`Printer.csDFNet`, `isInitialConvertible` and `DFacsimile` all key on
`DFNet.Assignment(regDcl | partialOfReg, rhs)`.

`PrintCodeStringSpec` asserts the no-member invariant directly, by checking that a write-only design
contains no `Alias.RegDIN`.

### How a read reaches a DFVal

`REG_DIN` is not a `DFVal`, so reads are wired through the operator machinery rather than by
subtyping. The binary operators take `inline lhs: SupportedValue` and resolve through `exactOp2`, so
three givens in `object REG_DIN` cover the whole read surface:

| Given | Covers |
|---|---|
| `evREG_DIN_OpLHS` | `r.din + 1`, every binary operator with the DIN on the left |
| `evREG_DIN_OpRHS` | `1 + r.din`, every binary operator with the DIN on the right |
| `evREG_DIN_TC` | receiver positions: `y := r.din`, connections, method arguments |

Each summons the corresponding given for `DFValOf[T]` and calls `dinVal`, so no per-operator or
per-DFType plumbing is needed. `REG_DIN[?]` is a member of the `SupportedValue` union alongside the
other non-`DFVal` wrappers (`BoolSelWrapper`, `SameElementsVector`).

**Not covered**: the extension groups keyed on `DFValAny` / `DFVal[T, M]`, namely `apply` selection,
`.bits`, `.as`, `.reg`, `.prev`. So `r.din(5, 0)` does not resolve; the canonical partial spelling is
`r(5, 0).din`, where `r(5, 0)` is already a `DFVal` and `.din` applies to it.

## 3. A named DIN read is rejected

`val d = r.din` is an elaboration error, raised eagerly in the `REG_DIN` constructor (not in
`dinVal`, so that an unread binding is still reported) and positioned at the `.din` site. Three
reasons:

1. **It reads as a snapshot and behaves as a view.** The alias is live, so
   `val d = r.din; r.din := 5; y := d` gives `y` the value 5, not the pre-assignment value.
2. **It is the only way the memoised node can straddle scopes.** Sharing one wrapper across scopes
   requires binding it to a `val`; without that, construction and use are always at the same site,
   so reachability never arises.
3. **It removes the one uncleanable member.** `val d = r.din` followed only by `d := 5` would
   otherwise materialise a named member nothing reads, which `DropUnreferencedAnons` cannot remove.

The check keys on the plugin naming only the outermost call site of a val RHS, so
`val y = r.din + 1` names the `+` and leaves the `.din` anonymous. Covered in
`ElaborationChecksSpec`.

## 4. IR

`DFVal.Alias.RegDIN(dfType, relValRef, ownerRef, meta, tags)` is an `Alias.Consumer`, deliberately
not an `Alias.Partial`. `Consumer` cannot propagate assignability, so the node can never reach an
assignment LHS and `departialDcl` stops at it and returns `None`. That is what lets it travel from
elaboration all the way to `ToED` without disturbing anything: to a generic alias walker
(`collectRelMembers`, `getReadDeps`, `dealias`, `StateAnalysis`, `SanityCheck`) it reads exactly like
`Alias.History`, i.e. a plain read of the register.

The DFHDL printer emits `${relVal}.din`. The VHDL and Verilog printers report `unsupported`: `ToED`
removes every `RegDIN` before them.

## 5. Lowering: `ToED`

There is no dedicated stage. `ToED` already builds precisely the structure a DIN read needs, for
every REG in a non-purely-sequential RT domain: a `${reg}_din` shadow variable, a
`dcl_din := dclREG` default at the top of the generated `process(all)`, a redirect of every REG
assignment LHS onto the shadow, and `dclREG :== dcl_din` in the clocked process. A separate stage
would have duplicated all of it. It is also the right home: the shadow is an RT-to-ED lowering
artifact, while at the RT level `r.din` is a legitimate abstract construct.

Reads add four hooks:

1. **Collect** `Alias.RegDIN` members of the domain and group them by their root REG
   (`relVal.departialDcl`), iterating `members` for determinism.
2. **Force a combinational process**: `domainIsPureSequential = false` when the domain has any DIN
   read. Without it a purely sequential domain emits only the clocked process and no shadow exists
   to read. This hook is **domain-wide**: one read gives every REG in the domain a shadow.
3. **Force the default**, per register: only DIN-read REGs join `dclREGRequiresDefaultSet`, so only
   they get `dcl_din := dclREG`. Every other REG keeps the existing heuristic (forced only when
   conditionally or partially assigned). Ordering is already right, since the defaults precede
   `processBlockAllMembers`, which preserves body order.
4. **Resolve the reads**: `addDinRef` is called on each alias's `relValRef`, putting the read into
   the same `dclChangeRefMap` that already drives the assignment redirect, and
   `dinReadPatches` then replaces the marker itself.

A partial read needs no cloning: `addDinRef` already recurses through `Alias.Partial`, so redirecting
the innermost ref re-roots the existing chain, and `RegDIN(ApplyRange(r, 5, 0))` becomes
`ApplyRange(r_din, 5, 0)` once the marker is dropped. A whole-value read is replaced by the shadow
directly.

`Alias.RegDIN` is excluded in `collectFilter` so the marker is not also moved into the generated
process: the move and the replacement would collide on the same member.

Result:

```scala
process(all):
  r_din := r
  r_din := r_din + d"8'1"
  r_din := r_din + d"8'1"
process(clk):
  if (clk.actual.rising) r :== r_din
```

### 5.1 A DIN read is never hoisted out of the process

`ToED` promotes a variable assigned exactly once at domain level into a concurrent connection. A
statement containing a DIN read must not be promoted: outside the process it would read the shadow's
*final* value rather than its value at that point, and when the same statement also feeds the shadow
it closes a combinational loop:

```verilog
always_comb begin r_din = r; r_din = sum; end
assign sum = r_din + 8'd1;   // sum depends on r_din depends on sum
```

`readsDIN` therefore excludes from `singleAssignments` any net whose relative members contain an
`Alias.RegDIN`. Such statements stay in the process body, in order:

```verilog
always_comb begin
  r_din = r;
  sum = r_din + 8'd1;
  r_din = sum;
end
```

### 5.2 Under VHDL the shadow is a process variable

VHDL signal assignment evaluates every RHS against the pre-process value, so a shadow *signal* would
turn a read-modify-write chain into a single increment, and being self-referential inside
`process(all)` it would never settle:

```vhdl
r_din <= r;
r_din <= r_din + 8d"1";
r_din <= r_din + 8d"1";   -- increments once, then oscillates
```

For DIN-read registers under VHDL the shadow is therefore a **process-local variable** with blocking
assignments, published to the design-level signal as the last statement of the process. The signal
is what the clocked process and any concurrent reader see:

```vhdl
process (all)
  variable r_din_v : unsigned(7 downto 0);
begin
  r_din_v := r;
  r_din_v := r_din_v + 8d"1";
  r_din_v := r_din_v + 8d"1";
  r_din <= r_din_v;
end process;
```

`DropLocalDcls` keeps non-REG locals inside VHDL processes, and `csDFValDclWithoutInit` prints
`variable` for a declaration whose owner is not a design block, so no printer work was needed.

Every redirected ref (assignment LHS and DIN read alike) originates from a member that lands inside
the combinational process, so `dclChangePatch` can point them all at the local without partitioning.
The `dclChangeList` entry stays the signal, which is what `regSaveBlock` reads. Registers without DIN
reads are untouched, and Verilog needs none of this since its shadow is already assigned blocking in
`always_comb`.

Ordinary signals assigned inside the combinational process and read later in it (such as `sum` above)
remain signals and converge across delta cycles through `process(all)` re-triggering, exactly as they
did before this feature.

## 5.3 `DFacsimile`

`Simulation.dbTransform` defaults to `identity`, so the native simulator consumes the elaborated DB
and never runs `ToED`. It needs its own support, and the model was already there: `env(dcl)` is the
pending value and is what closes the design
(`for (dcl, regWV) <- regNodeOf do wide.setNext(regWV, env.getOrElse(dcl, regWV))`).

`buildRegDIN` therefore resolves the alias chain to its declaration with the same `assignTarget`
walk a partial *write* uses, and slices `env.getOrElse(dcl, readWV(dcl))` instead of the committed
register. Position sensitivity is free: `env` is walked in body order, and a process re-seeds it per
site program, so a read inside a state sees only that state's writes. A memory-backed (`VecRepr.Ram`)
register vector and a non-register relative value are `unsupported`.

## 5.4 The `fallThrough` rewrite

A `fallThrough` condition is decided on the transition *into* its step, in the very cycle in which
entering that step already assigns registers: a `FALL_THROUGH` `for` loop resets its iterator, an
entered step runs its `onEntry`, a wrap-around re-runs the process prologue. Reading the registered
values there decides the skip on the values the entering state is about to replace, one cycle behind
what the condition names.

The `ExplicitFallThroughDIN` stage therefore rewrites every register read in a `fallThrough` block
into a DIN read. It sits between `DropRTWaits` and `FlattenStepBlocks`, which is the only window
where the rule can be stated once: `DropRTWaits` synthesizes the `fallThrough` sub-step (`!guard`)
for a `FALL_THROUGH` loop, so before it the loop form does not exist yet, and `FirstStepFusion`
(inside `FlattenStepBlocks`) already consumes the condition. By that point a user-written block and
a loop-generated one are the same shape, and one pass handles both.

Running it *after* `DropRTWaits` rather than inside it also keeps each stage's printout honest.
`DropRTWaits` prints the condition as written; re-elaborating that and continuing still reaches the
same result, because this stage is what defines the reading. That is the same reasoning that moved
the process bootstrap decision from `DropRTWaits` into `FlattenStepBlocks`.

The block is **rebuilt**, member by member, rather than edited in place. Two reasons: a register read
shared by two `fallThrough` blocks needs a distinct DIN read per block, which a reference redirect
keyed on the register cannot express; and redirecting an existing member's references from inside a
`MetaDesign` corrupts the reference table. Rebuilding also means a named intermediate inside the
block (`val edge = x && !armed` followed by `edge || armed`) is rewritten along with the condition
proper, so the whole block reads one way.

Two invariants the rebuild has to respect:

* **The marker stays last.** `DropRTProcess` and `FirstStepFusion` both read the block's condition
  positionally (`members.last` is the `Ident`), so each DIN read is emitted *before* the member that
  reads it, never appended.
* **One DIN read per reading reference.** An anonymous value may only be read once, so the wrappers
  are never shared between two readers even when they name the same register.

`Alias.RegDIN` is a `Consumer` and so is never itself register-rooted, which makes the rewrite a
fix-point. A read reached through a partial selection is wrapped at its outermost point, giving the
canonical `r(3, 0).din` rather than the unspellable `r.din(3, 0)`.

Two consumers had to follow:

* `FirstStepFusion.substDin` resolves a DIN read to the register's pending value *at that point of
  the expansion* (this region's write, else the value forwarded across the removed boundary), one
  step ahead of `substDcl`, which crosses only the boundary. A partial DIN read aborts fusion.
* `fallThroughSubsumed` compares the hook against the dispatch's leading guard through
  `sameAtFusedEntry`, which looks past the DIN reads: fusion inlines the dispatch into the hook's own
  cycle with nothing in between, so both resolve to the same forwarded value.

`DFacsimile` mirrors the rewrite rather than seeing it (it reads the elaborated IR, where the hook is
still a plain read): `dinGuardMode` gives `fallThroughCond` the pending read view. That is what
retired the `FALL_THROUGH` `for` loop `unsupported` guard, whose whole reason was the stale-iterator
behavior this rewrite removes.

## 6. Where each rule is enforced

| Rule | Enforced in |
|---|---|
| A write constructs no IR | `REG_DIN.:=` bypasses `dinVal` |
| A read materialises the marker once | `lazy val dinVal` |
| A named `.din` is rejected | `REG_DIN` constructor, eager |
| The marker never reaches an assignment LHS | `Alias.Consumer` |
| A shadow exists to read | `domainIsPureSequential = false` |
| A read before any assignment yields the register | `dclREGRequiresDefaultSet` |
| A read resolves to the shadow | `addDinRef` plus `dinReadPatches` |
| A read keeps its position | `readsDIN` excluded from `singleAssignments` |
| VHDL read-modify-write is correct | process-local variable plus publish |

## 7. Tests

| Spec | Covers |
|---|---|
| `PrintCodeStringSpec` | write-only adds no member, whole-value read, partial read, named expression |
| `ElaborationChecksSpec` | named `.din` rejection |
| `ToEDSpec` | the lowering, per-register defaults, the no-hoist rule, the VHDL process variable |
| `PrintVHDLCodeSpec` / `PrintVerilogCodeSpec` | end-to-end backend output for all four shapes |
| `RegDINSimSpec` | the `DFacsimile` read: position sensitivity, read-modify-write, partial reads, a read after a conditional assignment |
| `ExplicitFallThroughDINSpec` | the `fallThrough` rewrite: hook, named intermediate, partial read, and the untouched `onEntry`/`onExit` |
| `DropRTProcessSpec` | the FSM the rewritten condition lowers to |
| `RTProcessSimSpec` | the staged oracle for `.din` in a process, the `FALL_THROUGH` `for` loop, a register-guarded loop at the wrap-around, and a hook over its own `onEntry`'s register |

## 8. Open issues

* **A whole-vector DIN read on a RAM-repr REG** forces a full shadow copy. `DFacsimile` refuses it;
  `ToED` does not, so the two disagree on a shape neither handles well. Undecided whether to reject
  it everywhere or accept it with a documented cost.
* **A DIN read inside an RT `initial` block** is not rejected yet; that region is const-RHS only.
* **Cross-domain DIN reads** rely on the same "one domain assigns a given REG" assumption `ToED`
  already makes for assignments, without an explicit error.
* **`NameRegAliases`** returns `unique = false` for a named relative value, which plants the capture
  assignment after the declarations rather than at the use site. A named `.din` is rejected today, so
  this is unreachable; it becomes relevant if that restriction is lifted.
* **A REG read via `.din` but never assigned** still gets a shadow and a `r :== r_din` hold, which is
  correct but redundant.
* **Deferred**: teach `ToED` to reuse an existing domain-local variable as the shadow when a REG's
  sole assignment is an unconditional full-width blocking assignment from it (`q.din := d` becoming
  `q :== d` with no `q_din` copy). Worthwhile for hand-written code, but it moves many
  `lib/src/test/resources/ref/` goldens and should be its own change.
