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
- **Prologue / bootstrap**: the prologue is the statements before the first construct **plus the
  first step's `onEntry`** (that is the user-facing definition too). `foldInitialStatic` folds a
  constant-convertible prologue into time-zero register state (leading statements first, then the
  `onEntry`, and an assigned register's declaration `init` is superseded — "initial wins"); a
  non-convertible prologue *or* `onEntry` (or a trailing statement that shares a prologue-assigned
  register) keeps a one-cycle bootstrap state (`needsBoot`), and the wrap-around then returns to that
  bootstrap. The `onEntry` folds only when its step is the process's **first state** — with a leading
  wait/loop the lowering keys the generated initial block on that construct instead, so the `onEntry`
  just fires on its transition edge. The *reset-site fold* eliminates the bootstrap when the first
  construct's dispatch const-folds under the prologue values (zero bootstrap cycles).
- **Text output** lowers to *actions* — `(guard node, message segments)` fired per committed cycle
  with the cycle's settled values (register operands read through `snap` MOVs that survive the
  commit).

### Step hooks (onEntry / onExit / fallThrough) — the M3 shape

Hook blocks are nested `StepBlock`s named `onEntry`/`onExit`/`fallThrough` (predicates
`sb.isOnEntry`/`isOnExit`/`isFallThrough`; `sb.isRegular` excludes all three). They are **not time
constructs** and are **skipped by the ordered body walk** (`case sb: StepBlock if !sb.isRegular`).

**Where they are emitted is the whole game.** The lowering plants a goto's hooks *at the goto site* —
which, after `FlattenStepBlocks` relocates the inter-step trailing statements and clones the prologue
before the wrap-around goto, sits at the **very end** of the state's body. So DFacsimile emits them
at the transition's **landing**, not where the walk into it starts:

- `curStateStep` / `curSite` (set per site program) are the lowering's `currentStepBlock`; a step
  site's owner is recorded in `stepOfSite`.
- `landOn(site)` emits `onExit(curStateStep)` **once per execution path** (`exitEmitted`, saved and
  restored by `emitBranch2`/`emitDispatchChain`), and only for a non-self landing. Every jump goes
  through `jumpTo(site) = landOn(site); jump(site)`.
- `enterState(sb, site, cascaded)` is the step landing: `landOn`, `onEntry(sb)`, `jump`, then the
  `fallThrough` cascade. A self-transition (`ThisStep`, or target == source) fires **no** hooks — the
  lowering's static `currentStep != nextStep` gate, not a runtime state compare.
- Because the hooks live in `enterState`, a **sequential** entry (a nested step reached by falling
  into it from its parent's body, no explicit goto) fires the parent's `onExit` and the child's
  `onEntry` — which is exactly what the flattened form does, since flattening turns that into a goto.
- `cascadeFrom(sb)`: the zero-cycle advance emits **only** the next state's `onEntry` + state write
  (the skipped state's own body and trailing statements never run), re-executing the prologue when it
  passes the last state. It stops **after** re-entering the step the transition left (the lowering's
  `if (nextStepBlock != currentStepBlock)` sits *inside* `handleNextStep`, after that step's `onEntry`
  and `state.din`), i.e. one step later than a naive "stop before the origin" reading, and also on a
  step the same cascade already passed through (the chain is not a single ring, so it can close on
  itself without reaching the origin). `state`/register writes are last-write-wins, exactly the
  nested `state.din` overwrites.
- **Where the cascade goes** is `defaultExitOf(sb)`, mirroring the lowering's function of the same
  name: the target of the last `Goto` on the step's dispatch path (hook bodies excluded). Only a
  goto that names its target (or `FirstStep`) resolves here; `NextStep`/`ThisStep` fall back to the
  sequential `parkOrder` walk, which is how `FlattenStepBlocks` resolves them. It is emphatically
  **not** the declaration-order successor — a loop step whose body waits is followed in the state
  list by its own body's first state, while its exit leaves the loop.
- `fallThrough`'s condition is the last `DFVal` in the block body (an `Ident`); `compileGuardFresh`
  it.
- A step carrying an `onEntry`/`onExit` — or one whose dispatch's first time-consuming action is
  hook-carrying — is **never** a `FirstStepFusion` candidate (`hasNonRegularChild` / the `Blocked`
  scan). `hookBlocked` mirrors that and forces the step into `fallback`, so it always keeps a state
  of its own. Without this those hooks would have no edge to land on.
- A **pure `fallThrough`** (a hook holding nothing but its condition) is the exception: it does not
  block fusion, because a fused step costs no cycle at all, which subsumes the conditional
  zero-cycle skip the hook asks for. `enterStep`'s fused path therefore does
  `crossBoundary(); emitBranch2(fallThroughCond(ft), fusedFallThroughExit(sb), emitFrom(body))` —
  the condition is **forwarded** (unlike `enterState`'s edge-hook evaluation), matching the stage,
  which materializes it as the inlined dispatch's first decision. `fusedFallThroughExit` resolves
  the nested-form equivalent of the flat default exit: a trailing `NextStep` in a step that owns
  nested steps enters the first of them (what `FlattenStepBlocks` Rule 4 makes of it). The
  process's first step is excluded on both sides — it survives as the reset bootstrap state, where
  the hook has no edge left to run on.
- When the condition is the **negation of the dispatch's leading guard** the stage drops the hook
  outright, because materializing it would make the guard-false path unreachable (one value, one
  cycle, exact complements) and that path is where `FlattenStepBlocks` relocates the continuation —
  trailing statements and the forever-rotation's prologue clone. This is the shape every
  `FALL_THROUGH` loop has, so a fused one lowers to exactly the unmarked loop. DFacsimile needs no
  mirror: `enterLoop`'s fused path never had a hook to begin with, and it is the side that was
  already right.

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

**4. The benchmark suite — the wide-coverage regression net.** The benchmarks live in the
`benchmarks/` submodule (`dfhdl.benchmarks`, *Compile* scope, `runMain` mains — not specs):

```
benchmarks/runMain dfhdl.benchmarks.benchRun [--verilator]     # all three suites + summary table
benchmarks/runMain dfhdl.benchmarks.serv.servBench             # or one at a time
benchmarks/runMain dfhdl.benchmarks.sha_farm.shaFarmBench
benchmarks/runMain dfhdl.benchmarks.protocol_engine.protocolEngineBench
```

Each prints throughput **and an architectural state line** — that line is the point. `serv/*`
(bit-serial RISC-V, ~10M cycles of real instruction execution) and `proto` (step defs, if/match
dispatch, dynamic-bound while, dyn-gap waits, a second process, ~102M cycles) exercise far more
RT-process lowering than any unit DUT, and `sha/*` covers the wide datapath. A hook/fusion/prologue
change that survives unit lockstep can still be caught here. `--verilator` additionally builds and
runs the external Verilog model of each top and cross-checks the signatures (much longer; needs the
Verilator toolchain).

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
- **Baseline swap for a before/after signature check.** The benchmarks have no checked-in golden
  values, so get the baseline by running them against the pre-change file — without touching git
  state (and without committing, which is the standing rule):
  ```bash
  cp <file> "$SCRATCHPAD/mine.scala"                 # save your version
  git show HEAD:<file> > <file>                      # baseline in place
  sbt.bat --batch "benchmarks/runMain dfhdl.benchmarks.benchRun"   # capture state lines
  cp "$SCRATCHPAD/mine.scala" <file>                 # restore, then re-run and diff the lines
  ```
  Signatures must be bit-identical; throughput only needs to land in the same band (gotcha 12).

### Build/run commands

- Prefer `sbtn.bat` (project convention); if it wedges (a known intermittent), fall back to
  `/c/Users/OronPort/AppData/Local/Coursier/data/bin/sbt.bat --batch "compiler_stages/testOnly ..."`.
- Fast inner loop: `compiler_stages/testOnly dfhdl.sim.RTProcessSimSpec` (~7 s). Then the full
  `compiler_stages/test` + `lib/test` (clean the sandbox first — `clearSandbox` — for output-affecting
  stage changes). For anything touching RT-process lowering, finish with `benchRun` (~35 s) and
  compare the state lines against a baseline swap.

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
   park attach to that construct's *own* exit state; a **fall-through skip bypasses them** (it lands
   on the skipped step's exit *state*, it does not run the sequential continuation). For step
   `fallThrough` this is modelled directly (`cascadeFrom` lands on a state, it does not `emitCont`);
   for FALL_THROUGH **loops** the skip runs `emitCont(exitCont)`. A loop that **fuses** now agrees:
   its hook is subsumed by its own dispatch and dropped, so the guard-false path *is* the
   continuation. A loop that keeps a state (a park body, or a fusion fallback) still diverges here,
   so put a clean park right after such a loop in a test DUT.
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
9. **Where a hook is emitted is not where the walk enters the transition.** The lowering's goto sits
   at the end of the flattened state body, i.e. after the relocated trailing statements and the
   wrap-around prologue clone. Emitting `onExit` eagerly at the goto reverses it against the prologue
   (`y.din := 3` then `y.din := 9`, not the other way round) — emit at the landing instead. The same
   applies to the target's `onEntry`: at the wrap-around it must come *after* the re-executed
   prologue.
10. **A test DUT can silently miss the path you meant to test.** A `fallThrough` ring whose first
    step's `onEntry` is non-constant gets a bootstrap state, which changes the cascade's origin and
    hides a circular-guard bug. Validate each new lockstep test by *temporarily breaking* the
    implementation it targets and confirming it fails (cheap, and it caught two dead tests here).
11. **Inspect the lowering before modelling it.** A four-shape throwaway `StageSpec` with
    `assertCodeString(..., "SHOW-ME")` answered every question about the prologue/`onEntry`/bootstrap
    interaction in one run — far faster than reading `DropRTWaits` + `FlattenStepBlocks` +
    `DropRTProcess` + `FirstStepFusion` and guessing how they compose.
12. **Do not read a benchmark Mcps dip as a regression.** In-sbt runs sit a few percent under a clean
    forked run and swing several percent between runs (e.g. `sha/n=32` 3.36 vs 3.45 across two runs of
    the *same* code). The recorded best-case numbers in the plan come from a **forked JDK 25 run with
    `--add-modules jdk.incubator.vector`**; the repo's `.jvmopts` supplies that module to sbtn-started
    servers, and without it the vectorized commit silently falls back to scalar (`n=64` ≈ 1.33 instead
    of ≈ 1.65). Compare *signatures* for correctness; only compare throughput like-for-like.

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
