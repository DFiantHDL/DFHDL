# DFacsimile — the Native DFHDL Simulator Guide

> **For contributors working on the DFacsimile native simulator** (`compiler/stages/src/main/scala/dfhdl/sim/`).
> DFacsimile executes a DFHDL design **directly from the compiler IR** — no Verilog/VHDL, no external
> simulator, no foreign boundary. This skill is version-controlled: **update it** with any new
> architectural facts, testing patterns, or gotchas you discover (see "Keeping This Skill Up to Date").

You are helping improve or extend the DFacsimile simulator. The design/vision document is
`private-plans/dfacsimile-simulator-plan.md` (locked decisions, phased roadmap, the M-series RT
milestones). Read the relevant section there before large changes; keep its status notes current.

---

## The one rule that governs everything: fidelity to the shipped hardware

DFacsimile simulates the **IR**, but the *shipped* design is the printed HDL produced by the compiler
stages. So (Locked decision 11) **DFacsimile's cycle-accurate behavior must match what the FSM/RTL
lowering stages produce — bug-for-bug.** It may reuse read-only *analyses* but never runs IR-to-IR
transformations. Consequences you must internalize:

- If a lowering stage has a bug (e.g. the FALL_THROUGH for-loop stale-iterator bug), DFacsimile must
  **match that bug or refuse the construct (`unsupported(...)`)** — never silently do the "correct"
  thing, or the simulator lies about the hardware.
- Every RT-process feature is validated by a **lockstep test against the actual stage lowering**
  (`dropRTProcess`). If you can't make lockstep pass, either your model is wrong or you've found a
  stage bug — investigate before assuming.
- When a construct is genuinely un-modelable faithfully, call `unsupported("...")` in the pre-pass
  (`prepassProcess`) rather than producing wrong cycles.

---

## Architecture (bottom to top)

**File map** (`compiler/stages/src/main/scala/dfhdl/sim/`):

| File | Role |
|---|---|
| `Netlist.scala` | Pre-scheduled netlist: nodes (by index) over a shared `sig: Array[Long]`. Ops: `REG`, `CONST`, `MOV`, arithmetic/logic, `MUX`, `MEMRD`, `ROM`, comb-array (`ANEW`/`ALOAD`/`ASTORE`). `combNodeIds` = DFS-topo-sorted comb nodes (errors on comb cycles + unpatched MOVs). |
| `WideOps.scala` | Word-slicing layer. A value handle `WV(lanes: Vector[Int], width)` splits a wide value into 64-bit lanes (LSB-first). Every wide op decomposes to lane-wise scalar netlist ops (carry chains, barrel shifts, OR-composition). |
| `DFacsimile.scala` | The lowering: `Builder(rawDB)` → a `Scope` per design (hierarchical). `Scope.elaborate()` walks IR members and emits netlist nodes. Contains `ProcLowering` (the RT-process FSM compiler). |
| `Codegen.scala` | Tier-1: generates Java over primitive `long[]`, compiled in-memory. CSE + copy-propagation + dead-code elimination over the netlist. |
| `SimulationAPI.scala` | The public typed surface: `dsn.simulation { dut => ... }`, `run()`, `peek`/`poke`, `continue`, `inspect`, `withTier`. `class Simulation(mkDsn, block, tier, seed, dbTransform)`. |

**Two execution tiers, one state layout** (`SimTier.Interpreter` / `SimTier.Codegen`): both execute
the same `Netlist` over the same `sig` array. Interpreter walks an op stream (zero compile latency,
semantic reference). Codegen JIT-compiles straight-line Java (the speed tier). They must be
bit-identical — always test **`bothTiers`**.

**Value model**: `WV` = lanes. `env: Map[Dcl, WV]` holds a wire/register's current driven value (for
registers, the *pending din*). `readWV(v)` resolves a value to its `WV`, compiling lazily and
memoizing in `nodeOf`. Registers live in `regNodeOf`; child input ports are `inPortMov` MOV
placeholders patched by the parent's connection (connections are order-free — see the order-free
connection resolution, `sinkMov`/`bitMov`).

---

## The RT-process FSM model (`ProcLowering`) — where most RT work happens

An RT `process:` lowers to an FSM over an implicit **state register** (`segCellVar`), directly from
the elaborated IR, replicating the *combined* effect of `SimplifyRTOps → DropRTWaits →
FlattenStepBlocks + FirstStepFusion → DropRTProcess`.

- **Sites (= parks = FSM states).** Each construct that consumes a cycle becomes a *site* with a
  program: a `Wait`, a *park loop* (control-free body — one cycle per iteration), a *control loop*
  (a loop that keeps a control state), and a *park step* (a pure-dispatch step). `segCellVar` holds
  the current site index; `jump(k)` sets the next site.
- **Fused (zero-cycle) transitions.** `emitFrom`/`emitCont` walk *through* fused constructs (whose
  entry inlines into the current cycle) until they reach a park, then `jump`. `enterStep`/`enterLoop`
  fuse a step/loop's leading payload into the transition cycle when it has no own site.
- **Park classification** (the crux — get this right):
  - `isTimeConstructM(m)`: `Wait` | **regular** `StepBlock` | non-combinational loop. (onEntry/
    onExit/fallThrough hook blocks are **not** time constructs.)
  - `isParkStep(sb) = !hasTimeIn(sb)` — a **pure-dispatch step** (only gotos/payload, no waits, no
    nested steps) **is a park** (1 cycle). A step containing a wait or nested step is *fused*; its
    inner parks are the real states. (A nested `StepBlock` counts as a time construct, which is how
    "nested first steps fuse into the innermost park" works.)
  - `isParkLoop(lb) = !hasControlIn(lb)`.
- **`crossBoundary()`** models crossing a conceptual cycle boundary during a fused emission: pending
  register writes become the forwarded read view (`fwdRegs`), so a guard evaluated on the transition
  edge reads the *post-`.din`* (next-cycle) register values — matching the FSM lowering's
  incoming-edge guards.
- **Fusion fallback** (`computeFallbacks`/`ruleCPass`): a visit-capped walk (`walkSeq`/`walkGoto`/
  `walkLoopEntry`) that detects dispatch cycles that cannot const-fold and keeps a control state for
  them — mirroring `FirstStepFusion`'s victim/restart discipline.
- **Prologue / bootstrap**: statements before the first construct are the prologue.
  `foldInitialStatic` folds a constant-convertible prologue into time-zero register state; a
  non-convertible prologue (or a trailing statement that shares a prologue-assigned register) keeps a
  one-cycle bootstrap state (`needsBoot`). The *reset-site fold* eliminates the bootstrap when the
  first construct's dispatch const-folds under the prologue values (zero bootstrap cycles).
- **Text output** lowers to *actions* — `(guard node, message segments)` fired per committed cycle
  with the cycle's settled values (register operands read through `snap` MOVs that survive the
  commit).

### Step hooks (onEntry / onExit / fallThrough) — the M3 shape

Hook blocks are nested `StepBlock`s named `onEntry`/`onExit`/`fallThrough` (predicates
`sb.isOnEntry`/`isOnExit`/`isFallThrough`; `sb.isRegular` excludes all three). They are **not time
constructs**, are **skipped by the ordered body walk** (`case sb: StepBlock if !sb.isRegular`), and
are emitted **only at the transition edges** in `emitGoto`:

- `stepTransition(cur, tgt, jump)`: on a non-self edge, run `onExit(cur)` then `enterWithHooks`. A
  self-transition (`ThisStep`, or target == source) fires **no** hooks — the FSM lowering's static
  `currentStep != nextStep` gate, not a runtime state compare.
- `enterWithHooks(origin, tgt, jump)`: run `onEntry(tgt)`, `jump()`, then if `tgt` has a
  `fallThrough`, `emitBranch2(cond, cascade→nextRegular(tgt), stay)` — a same-cycle nested-conditional
  cascade along declaration order (`nextRegular` wraps last→first), stopping when the cascade reaches
  the edge origin (circular guard). `state`/register writes are last-write-wins, exactly the FSM
  lowering's nested `state.din` overwrites.
- `fallThrough`'s condition is the last `DFVal` in the block body (an `Ident`); `compileGuardFresh`
  it.

**FALL_THROUGH loops** reuse the same idea at loop entry: `enterLoop` for a FALL_THROUGH park loop
does `crossBoundary(); emitBranch2(guard, jump(site), emitCont(exitCont))` — a zero-cycle skip to the
following park when the guard is false on entry.

---

## Testing methodology (this is how you get RT changes right)

Tests live in `compiler/stages/src/test/scala/dfhdl/sim/`. Base class `SimSpec` (typed
`assertEquals` on DFHDL constants; `bothTiers(name)(body)` runs Interpreter + Codegen).

**1. The lockstep oracle — the gold standard for RT-process fidelity** (`RTProcessSimSpec.lockstep`):

```scala
lockstep(new SomeProc, tier, cycles, watch = List(_.y, _.state),
         pokes = t => dut => if t == 3 then dut.x.poke(1))
```

It runs the design **directly** (DFacsimile) *and* through the real FSM lowering
(`new Simulation(mkDsn, None, tier, 0L, dbTransform = _.dropRTProcess)`), comparing every watched
member, the accumulated text output, and the run status **every cycle**. If it passes on both tiers,
your model matches the hardware. Add DUTs to `RTProcessDesigns.scala`, tests to `RTProcessSimSpec.scala`.

**2. Typed cycle traces** for shapes whose counts the cycle-semantics rules pin down directly
(`dsn.simulation { dut => for t do assertEquals(dut.sig.peek, expected); simCtx.step() }`).

**3. Both tiers, always.** A bug often shows on only one tier (Codegen DCE/observed issues).

### Debugging a divergence

- **Direct raw-vs-oracle trace.** Drop a throwaway `SimSpec` that runs the design and its
  `_.dropRTProcess` oracle side by side, printing the watched signals per cycle with a `<<<` marker
  on mismatch. This localizes the *first* diverging cycle far faster than munit's single-cycle diff.
  (Delete the scratch spec when done.)
- **See munit's actual diff:** `--batch "set compiler_stages/Test/logBuffered := false" "compiler_stages/testOnly ..."`. Note `assertEquals(member.peek, expected)` in lockstep reports
  **obtained = oracle, expected = raw(DFacsimile)**.
- **Inspect the lowered FSM** the oracle simulates:
  ```scala
  class ScratchSpec extends StageSpec(stageCreatesUnrefAnons = true):
    test("x") { assertCodeString((new Foo).dropRTProcess, "SHOW-ME") }  // diff prints the real FSM
  ```
  `stageCreatesUnrefAnons = true` is required because the `for→while` rewrite leaves a dangling
  `DFRange` that `dropUnreferencedAnons` cleans in the full pipeline; a bare `sanityCheck` flags it.

### Build/run commands

- Prefer `sbtn.bat` (project convention); if it wedges (a known intermittent), use
  `/c/Users/OronPort/AppData/Local/Coursier/data/bin/sbt.bat --batch "compiler_stages/testOnly ..."`.
  Both were used successfully this session; `sbt.bat --batch` is the reliable fallback.
- Fast inner loop: `compiler_stages/testOnly dfhdl.sim.RTProcessSimSpec` (~7 s). Run the full
  `compiler_stages/test` + `lib/test` (clean the sandbox first — `clearSandbox` — for output-affecting
  stage changes) before declaring done.

---

## Gotchas banked (each cost real debugging time)

1. **Fidelity, not correctness.** (See the top rule.) The FALL_THROUGH for-loop is the canonical
   trap: the FSM lowering resets the iterator (`i.din:=0`) and evaluates the fall-through guard
   (`if(!(i<n))`) in the **same** state, so register read-before-write makes the guard read the
   **stale** iterator — hardware skips the loop every other run. DFacsimile does the "right" thing and
   thus diverges, so it is `unsupported`. A `while` loop has no iterator init, so it works.
2. **Codegen `observed` set.** Any node the runtime reads from `sig` after a bulk run (watch
   aggregate, action guards, wait bounds, peeks) **must** be in the codegen `observed` set (which
   drives spill + syncOut), independent of copy-prop pinning. Dropping one → stale reads that only
   fail on the Codegen tier.
3. **Trailing statements fuse into a construct's exit.** Statements between a construct and the next
   park attach to that construct's *own* exit state; a **fall-through skip bypasses them** (it goes to
   the construct's `nextBlocks`, not the sequential continuation). So fall-through / FALL_THROUGH
   targets should be clean parks — a test DUT that puts a bare assignment right after a FALL_THROUGH
   loop, or a `fallThrough` step whose target has a leading-payload-then-wait body, will diverge for
   this reason (not a real bug — restructure the DUT with a park immediately after).
4. **`crossBoundary` forwarding.** A guard on a transition edge must read *post-`.din`* register
   values — call `crossBoundary()` before compiling it (`compileGuardFresh`/`loopGuardNode`).
5. **Park classification is by content, not name.** A step is a park iff pure-dispatch; a nested
   `StepBlock` counts as a time construct (drives the "innermost park" fusion). Hook blocks are the
   exception — excluded from `isTimeConstructM` so a hook-carrying pure-dispatch step stays a park.
6. **Lowering inspection needs `stageCreatesUnrefAnons = true`** (dangling `DFRange` from `for→while`).
7. **munit test-name filter is a regex** — `--tests=*Foo*` is an invalid pattern (`Dangling meta
   character '*'`); just run the whole spec (it's fast) or use `.*Foo.*`.
8. **Phantom `lib/test` failures** after output-affecting stage changes = stale `sandbox/`
   (FullCompileSpec doesn't clean it). `clearSandbox` and re-run before believing a regression.

---

## Keeping This Skill Up to Date

DFacsimile is under active development (see the plan's phased roadmap: Tier-1 co-simulation rigging,
the parallel kernel, activity gating, DF dynamic-dataflow, etc.). When you learn something general
while working on it, update the right place here:

- **New architectural fact** (a netlist op, a WideOps decomposition, a Codegen pass) → the
  Architecture section.
- **New RT-process/FSM behavior** → the ProcLowering section.
- **New testing pattern or debugging technique** → the Testing section.
- **A new pitfall that cost you time** → a numbered "Gotchas banked" entry.

And update `private-plans/dfacsimile-simulator-plan.md`'s status notes (the M-series and phase exit
criteria) when a milestone advances. Rule of thumb: *would a future contributor hit this same wall if
they didn't already know it?* If yes, write it down.
