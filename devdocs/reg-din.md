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

## 8. Open issues

* **`DFacsimile` has no DIN-read support yet.** `Simulation.dbTransform` defaults to `identity`, so
  the native simulator consumes the elaborated DB and never runs `ToED`. The model is already there:
  `env(dcl)` is the pending value and defaults to the register node
  (`for (dcl, regWV) <- regNodeOf do wide.setNext(regWV, env.getOrElse(dcl, regWV))`), so a read
  resolves to `env.getOrElse(dcl, regNodeOf(dcl))` at the point of the read. The RAM-repr
  (`VecRepr.Ram`) and initial-block cases need `unsupported` guards.
* **A whole-vector DIN read on a RAM-repr REG** forces a full shadow copy. Undecided whether to
  reject it or accept it with a documented cost.
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
