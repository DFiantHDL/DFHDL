# Processes

Processes define *when* a block of logic runs. DFHDL supports processes in two domains:

- **RT domain**: A **clock-bound** process used to describe finite-state machines (FSMs). The process runs in lockstep with the domain clock and uses steps, waits, and control flow that the compiler lowers to registers and combinational logic.
- **ED domain**: **Sensitivity-driven** processes that run when listed signals change (or all read signals with `process(all)`), giving the same level of control as `process` in VHDL or `always` in Verilog.

Processes are not available in the dataflow (DF) domain. Processes cannot be nested inside another process.

Both domains also support [**`initial` blocks**](#initial-blocks) for once-only initialization.

## RT domain: clock-bound FSM process

In an [RT design][design-domains], a process is used to describe a finite-state machine that is **clock-bound**: it advances on the domain clock and is compiled to a state register plus combinational next-state and output logic.

### Syntax: `process:`

Use the argument-less `process:` inside an `RTDesign` or `RTDomain`. The block contains either plain combinational logic (assignments, no steps) or step definitions that form an FSM.

### Step-based FSM

Define states as `def Name: Step = ...` and control flow with:

- **`NextStep`**: advance to the next step in definition order.
- **`ThisStep`**: stay in the current step for another cycle.
- **`FirstStep`**: jump to the first step, exactly as naming it would. It is a jump, not a restart: it does not re-run the [process prologue](#cycle-semantics).
- **Step name** (e.g. `S1`, `S2`): jump to that step.

You can optionally name the process (e.g. `val my_fsm = process:`) so the compiler uses that name for the generated state enum and state register.

```scala
class SimpleFSM extends RTDesign:
  val x = Bit <> IN
  val y = Bit <> OUT.REG init 0

  process:
    def S0: Step =
      y.din := 0
      if (x) NextStep else S0
    def S1: Step =
      y.din := 1
      if (x) S2 else FirstStep
    def S2: Step =
      y.din := 0
      if (x) ThisStep else FirstStep
```

The compiler lowers this to a state enum, a state register, and a `match` on the state; see [Design Domains][design-domains] for the compilation flow.

Steps may be nested: a `def Name: Step` body can define further steps, which execute in definition order within their parent. Statements written between two steps belong to the cycle in which the earlier step completes. After the last step of the process completes, the FSM wraps around to the first step, re-executing the process prologue (see [Cycle semantics](#cycle-semantics)).

### Cycle semantics

An RT process advances in lockstep with the domain clock, and DFHDL defines exactly how many cycles each construct consumes. The rules are designed so that composing the same total behavior in different ways costs the same number of cycles.

**Rule 1: a time-consuming construct consumes exactly its own time, and nothing else does.**
The time-consuming constructs are:

- **Cycle waits**: `n.cy.wait` consumes exactly `n` cycles. `1.cy.wait` is a single-cycle delay.
- **Timed waits**: `100.ms.wait`, `5.us.wait`, etc. are converted to a cycle count using the domain clock rate and then behave like cycle waits.
- **Condition waits**: `waitUntil(cond)` and `waitWhile(cond)` sample their condition once per cycle and therefore consume at least one cycle.
- **Steps**: entering a step consumes one cycle (a registered state transition). A step containing only a jump, such as `def S: Step = NextStep`, is a one-cycle delay. Sequential steps consume one cycle each.

```scala
// waits consume exactly their own time (assuming a 100MHz domain clock)
process:
  10.cy.wait        // exactly 10 cycles
  1.us.wait         // exactly 100 cycles (converted via the clock rate)
  waitUntil(ack)    // samples ack once per cycle: at least 1 cycle

// sequential steps consume one cycle each
process:
  def S1: Step =
    y.din := 1
    NextStep        // one cycle in S1
  def S2: Step =
    y.din := 0
    NextStep        // one cycle in S2, then wrap-around to S1
```

**Rule 2: every loop pass consumes at least one cycle.**
Loops are time-consuming constructs: each executed iteration of a `for` or `while` loop is a registered state transition, so it consumes the cycles its body consumes, with a minimum of **one cycle per iteration**. An empty body does not make iterations free:

```scala
process:
  for (i <- 0 until 10)
    for (j <- 0 until 10) {}   // 100 cycles: one per innermost iteration
```

A loop that runs zero iterations (its guard is false on entry) still consumes its one-cycle minimum, unless it is wrapped with `FALL_THROUGH` (see [Loops](#loops)):

```scala
process:
  while (false) {}                 // one cycle to enter and skip each loop
  while (false) {}
  while (false) {}
  finish()                         // fires within the third cycle (fused into the last skip)

process:
  FALL_THROUGH:                    // each wrapped loop is skipped with zero cycles
    while (false) {}
  FALL_THROUGH:
    while (false) {}
  FALL_THROUGH:
    while (false) {}
  finish()                         // fires within the first cycle
```

**Rule 3: control flow fuses into the first time-consuming construct it reaches.**
Control flow on its own never consumes cycles. Between one time-consuming construct and the next, every boundary crossed (loop entry, loop exit, the loop-back edge, and step entries) executes combinationally within a transition cycle that is already paid for, either by the construct being left or by the one being entered. This rule has two direct consequences.

*Loop control adds nothing beyond Rule 2's minimum.* When the loop body consumes cycles, the iteration boundaries fuse with the body's own cycles, so the loop costs exactly the sum of the cycles its iterations consume. Consequently, all of the following consume exactly the same number of cycles:

```scala
// A: one flat wait
process:
  100.ms.wait

// B: a loop of waits
process:
  for (i <- 0 until 100)
    1.ms.wait

// C: nested loops of waits
process:
  for (i <- 0 until 10)
    for (j <- 0 until 10)
      1.ms.wait
```

*Nested first steps fuse.* When the first time-consuming action of a step is entering an inner step, the outer and inner entries are a single entry and share one cycle (they are the same FSM "label"). Statements before the inner entry (assignments, prints) execute in that shared cycle. This applies recursively, so this process consumes one cycle per wrap-around, not three:

```scala
process:
  def S1: Step =
    println("hello")
    def S2: Step =
      println("dear")
      def S3: Step =
        println("friend")
        NextStep
      NextStep
    NextStep
```

The compiler reduces it to a single step that executes all three prints in its one shared cycle:

```scala
process:
  def S1: Step =
    println("hello")
    println("dear")
    println("friend")
    NextStep
```

**Guard sampling.** Because the loop-back edge adds no extra cycle, a loop guard is evaluated combinationally in the transition cycle, using the values that registers will hold in the *next* cycle. For example, when an iteration ends with the implicit increment `i.din := i + 1`, the guard `i < 100` is evaluated in that same cycle as `(i + 1) < 100`. External signals (ports) read by a guard are sampled in the transition cycle. This matches how a hand-written FSM decides its next state.

```scala
process:
  for (i <- 0 until 100)     // guard evaluated as (i + 1) < 100 in each iteration's last cycle
    dataOut.din := mem(i)
    1.cy.wait
  done.din := 1              // iteration 99 exits the loop within its own cycle, so this
                             // executes in the loop's 100th and final cycle
```

**Process prologue.** Statements written *before* the first step, together with the first step's `onEntry` block, form the process **prologue**. The prologue runs in exactly two situations:

1. **Initialization**: when the FSM starts, either on reset when the domain has a reset, or at power-on otherwise.
2. **Forever wrap-around**: each time the process completes its last step and implicitly (through a `NextStep` jump) wraps back to the first step.

The leading statements do **not** re-run on an explicit jump to the first step (`FirstStep` or the first step's name), nor on a `ThisStep` self-transition. The first step's `onEntry` is an ordinary entry hook on top of that: it runs on *every* entry into the first step from a different step, explicit jumps included (but not on a self-transition).

This holds whether or not the prologue is initial-convertible. When it is not (see [Cycle semantics](#cycle-semantics)), the compiler gives the prologue a state of its own to run it, and the wrap-around passes through that state. `FirstStep` does not: it jumps to the process's first step — the first step *you* wrote, or the one a leading wait or loop yielded — paying neither the prologue nor that state's cycle. `FirstStep` and naming the first step are therefore always the same jump, and are always distinct from a wrap-around.

```scala
process:
  sum.din := 0          // prologue: runs at initialization (reset or power-on)
                        // and again at each wrap-around
  def Accum: Step =
    sum.din := sum + x
    if (sum > 100) NextStep
    else ThisStep       // self-transition: no prologue re-run
  def Flush: Step =
    y.din := sum
    if (err) Accum      // explicit jump to the first step: no prologue re-run
    else NextStep       // wrap-around past the last step: sum.din := 0 re-executes
```

**Rule 4: initialization is free.**
When the prologue is *initial-convertible*, meaning it consists only of blocking assignments of **constant** values (through `.din` for registers), possibly inside combinational (`COMB_LOOP`) `for` loops with constant bounds or conditionals with constant guards, both situations above consume **zero cycles**. The compiler lowers the prologue into a generated [`initial` block](#initial-blocks) (absorbed into the register reset branch, or into declaration initials when there is no reset), and re-executes a copy of it combinationally in the wrap-around transition cycle. The FSM therefore starts directly in its first state after reset. This is why a process beginning with a `for` loop pays no cycle for the iterator initialization, neither at reset nor on any wrap-around:

```scala
process:
  for (i <- 0 until 8)     // i = 0 costs no cycle, at reset or on wrap-around
    dataOut.din := mem(i)
    1.cy.wait              // 8 cycles total per pass
```

/// admonition | When a control cycle is still consumed
    type: note
Fusion falls back to the previous behavior of one extra control cycle (per loop entry and per iteration boundary) when the loop or step boundary cannot be resolved combinationally:

- A `while` guard that reads registers assigned *conditionally* or *partially* inside the loop body (the next-cycle guard value cannot be expressed at the boundary).
- Nested loops whose inner iteration count is not statically known (for example, `for (j <- 0 until n)` with a dynamic `n` inside an outer loop). Single dynamic loops still fuse; only the re-entry of a dynamic nest keeps a control state.
- Steps that carry `onEntry` or `onExit` blocks (their statements must land on a real state edge). A `fallThrough` block does not keep a control cycle: a fused step costs none at all, so the condition simply becomes the first decision of the step's own dispatch.
- Steps whose jump dispatch is a `match` rather than `if` conditionals.

Similarly, Rule 4 falls back to a synthetic bootstrap state when the prologue cannot be lowered into an `initial` block. That state runs the prologue, so it costs a cycle at process start *and* on each wrap-around, which passes through it. An explicit `FirstStep` jump does not pass through it and costs nothing.

- The prologue (or the first step's `onEntry`) is not initial-convertible: it contains non-constant right-hand sides, assignments to wires/ports (non-registered), prints, `while` loops, or conditionals with non-constant guards/selectors.
- A variable assigned by the prologue is also assigned by trailing statements of the process body (statements executed in the wrap-around exit cycle): the wrap-around re-initialization would shadow that trailing assignment in the same cycle, so the bootstrap state is kept instead.
- The process's first time-consuming construct is guarded by a condition that is not constant at initialization (for example, a first loop bounded by a dynamic `n`): the entry state cannot be selected at reset, so the entry dispatch occupies a one-time bootstrap state (one cycle at process start only; loop iterations are unaffected).

These cases are deterministic: a given shape either always fuses or always keeps its control/bootstrap cycle.
///

### Waits

RT processes can use **cycle waits** (`1.cy.wait`, `n.cy.wait`), **timed waits** (`100.ms.wait`, converted through the domain clock rate), and **condition waits** (`waitUntil(cond)`, `waitWhile(cond)`). The compiler converts these into step blocks and counters so that the behavior remains clock-bound and synthesizable. A wait can be named (`val MyWait = 1.cy.wait`) to control the generated step and counter names.

A bare **`wait`**, with no duration or condition, is an **endless wait**: the FSM enters a terminal state and halts there (until reset). Use it to end a run-once sequence. A process ending in an endless wait has no wrap-around, so its prologue runs only at initialization.

### Loops

`for` and `while` loops inside an RT process describe sequential (multi-cycle) iteration, following the cycle semantics above: each executed iteration costs its body's cycles, with a minimum of one cycle per iteration, and the loop boundaries add no cycles beyond that. Loop iterators become registers.

```scala
process:
  for (i <- 0 until 8)
    dataOut.din := mem(i)
    1.cy.wait          // one cycle per element: 8 cycles total
```

A `while` loop with a body that consumes no cycles samples its guard once per cycle (one cycle per iteration), which is exactly the behavior of `waitUntil`/`waitWhile`.

Wrapping an RT loop with a **`FALL_THROUGH`** block marks the loop to fall through without consuming any cycles when its guard is false on entry, continuing at whatever follows the loop.

The marker is only needed for a loop whose body consumes no cycles, since that is the shape that pays a cycle to enter and skip (Rule 2 above). A loop whose body does consume cycles fuses (Rule 3), so it already enters and exits for free: adding `FALL_THROUGH` to one costs nothing and changes nothing.

The skip is decided in the same cycle that enters the loop, so the guard reads its registers as [`.din`][din], the pending next-cycle value. This is what makes a `FALL_THROUGH` `for` loop mean what it reads as: the loop entry resets the iterator, and the skip decision follows that reset rather than the count left over from the previous pass.

Wrapping an RT loop with a **`COMB_LOOP`** block marks it combinational: the whole loop executes within a single cycle and generates no steps (so its body must not consume cycles).

Both annotations are allowed under RT domains only; applying them elsewhere is a compile-time error.

For elaboration-time (unrolled) loops outside processes, and for hardware loops at RT design scope, see [Loops][loops].

### fallThrough

A step can define **`def fallThrough = cond`** where `cond` is a Boolean/Bit expression. When the condition holds, the step advances in the same cycle without registering in it (conditional advancement); when it does not, the FSM enters the step normally.

The step it advances to is the one the step itself would have gone to: the target of the goto on its own default path, not whichever step happens to be declared next. In the example below, entering `S1` with `x` set advances straight to `S3`, and `S2` is never visited:

```scala
process:
  def S0: Step =
    NextStep
  def S1: Step =
    def fallThrough = x   // when x, advance to S3 in the same cycle
    S3
  def S2: Step =
    FirstStep
  def S3: Step =
    S2
```

The advance runs the target step's `onEntry`, and cascades: if the step it advances to also falls through, control keeps advancing within the same cycle, stopping when it reaches a step that does not fall through, or one it has already passed through in this cycle.

The condition is decided on the transition into the step, in the same cycle in which entering it already assigns registers, so it reads every register it names as [`.din`][din]: the pending next-cycle value. A condition over a register that the step's own `onEntry` writes therefore sees what `onEntry` has just written, not the value it is about to replace:

```scala
def Armed: Step =
  def onEntry =
    armed.din := x
  def fallThrough = !armed   // reads armed.din, so it follows the assignment above
  NextStep
```

A step that fuses (see [Cycle semantics](#cycle-semantics)) costs no cycle to begin with, so on such a step `fallThrough` no longer decides whether a cycle is spent, only whether the step's own statements run: the condition becomes the first decision of the step's dispatch, and is evaluated on the transition edge like the step's other guards, on the values the registers will hold in the next cycle.

### onEntry and onExit

Inside a step you can define:

- **`def onEntry = ...`**: runs when entering the step (once per transition into this state).
- **`def onExit = ...`**: runs when leaving the step (once per transition out of this state).

Self-transitions do not trigger these hooks: a step that jumps to itself (`ThisStep`) fires neither its `onExit` nor its `onEntry`.

```scala
def S1: Step =
  def onEntry =
    y.din := 1
  if (x) S2 else FirstStep
def S2: Step =
  def onExit =
    y.din := 0
  if (x) ThisStep else FirstStep
```

### Process with no steps

If the process body has no step definitions, waits, or loops, it is purely combinational and runs every cycle:

```scala
process:
  y.din := x
```

## ED domain: sensitivity-driven processes

In an [ED design][design-domains], processes are **sensitivity-driven**: they run when an event occurs on one of their sensitivity signals (or on any read signal with `process(all)`).

## ED process forms

### Sensitivity list: `process(sig1, sig2, ...)`

The process runs whenever any of the listed signals change.

```scala
class CombAndSeq extends EDDesign:
  val clk = Bit <> IN
  val rst = Bit <> IN
  val x   = UInt(8) <> IN
  val y   = UInt(8) <> OUT

  // Combinational logic: runs when x changes
  process(x):
    y := x + 1

  // Sequential logic, Verilog style: runs on clock (and optionally reset) events
  val r1 = UInt(8) <> VAR init 0
  process(clk.rising):
    r1 :== x

  // Sequential logic, VHDL style: runs on clock (and optionally reset) events
  val r2 = UInt(8) <> VAR init 0
  process(clk):
    if (clk.rising)
      r2 :== x
```

You can list multiple signals, including edge-qualified signals (see [Edge sensitivity](#edge-sensitivity)).

### Combinational-style: `process(all)`

The process is sensitive to *all* signals that are read in the block. Use this for combinational logic that should react to any input change. The compiler infers the actual sensitivity list from the block body.

```scala
class CombLogic extends EDDesign:
  val a = UInt(8) <> IN
  val b = UInt(8) <> IN
  val y = UInt(8) <> OUT

  process(all):
    y := a + b
```

/// admonition | The inline single-line `process(all): stmt` form does not parse
    type: warning
A process body must be a **block**, not an inline statement on the same line as the `process(...)` colon. Writing the body inline like `process(all): y := a + b` does **not** parse. Use one of these two accepted forms instead:

```scala
// 1. Braces around the body (body may be on the same line)
process(all) { y := a + b }

// 2. Colon with the body on the next, indented line
process(all):
  y := a + b
```

This applies to every process form (`process(sig)`, `process(all)`, `process(clk)`, etc.).
///

### Forever process: `process:`

A process with no sensitivity list runs continuously. It is allowed in RT and ED, but **not** in DF. 

- **In RT**: `process:` is the [clock-bound FSM process](#rt-domain-clock-bound-fsm-process) described above (steps, waits, etc.).
- **In ED**: Use it for testbenches or clock generation (e.g. toggling a clock with `wait`).

```scala
class Testbench extends EDDesign:
  val clk = Bit <> VAR
  process:
    clk := !clk
    5.ns.wait
```

A bare **`wait`**, with no duration or condition, halts the process forever. Use it to turn a forever process into a run-once stimulus process:

```scala
process:
  x := 1
  10.ns.wait
  x := 0
  wait   // halt forever
```

It compiles to `wait;` in VHDL and `wait(0);` in Verilog.

## Edge sensitivity

For sequential (clocked) logic you typically want the process to run only on a specific clock edge. You can either:

1. **List the clock and check the edge inside the block** (VHDL-style):

```scala
process(clk):
  if (clk.rising)
    reg :== nextVal
```

2. **Put the edge in the sensitivity list** (Verilog-style; compiler may normalize to this):

```scala
process(clk.rising):
  reg :== nextVal
```

Edge options are `.rising` and `.falling` on clock (or bit) signals. When reset is used, list both clock and reset and branch on reset then clock edge:

```scala
process(clk, rst):
  if (rst)
    out :== 0
  else if (clk.rising)
    out :== nextVal
```

For the **Verilog-style async reset** pattern, put the edges in the sensitivity list and branch on reset only:

```scala
process(clk.rising, rst.rising):
  if (rst)
    out :== 0
  else
    out :== nextVal
```

/// admonition | ED is a faithful mirror of Verilog/VHDL, so write synthesizable patterns
    type: warning
The ED domain is intentionally a low-level, faithful mapping to Verilog `always` blocks / VHDL `process` blocks. **DFHDL does not enforce synthesizability in the ED domain.** If you describe a non-synthesizable process pattern, the generated Verilog/VHDL will faithfully reflect that pattern, and downstream synthesis (or even a parser like Yosys) may reject it.

It is your responsibility to write process bodies that match a synthesizable template. In particular, when an edge is **already in the sensitivity list**, do not re-check that edge inside the body: the body has already been triggered by it.

**Non-synthesizable** (clock edge appears both in sensitivity list and as a nested `else if`):

```scala
// BAD: emits `else if (posedge clk)` inside an always_ff, which is not valid Verilog
process(clk.rising, rst.rising):
  if (rst)
    x :== 0
  else if (clk.rising)   // redundant: body already runs on rising clk
    x :== x + 1
```

**Synthesizable** equivalents, pick the style that matches the intent:

```scala
// Verilog-style async reset: edges in the sensitivity list, branch on reset only
process(clk.rising, rst.rising):
  if (rst)
    x :== 0
  else
    x :== x + 1

// VHDL-style: list the signals, branch on reset, then on the clock edge
process(clk, rst):
  if (rst)
    x :== 0
  else if (clk.rising)
    x :== x + 1
```

The rule of thumb: an edge qualifier (`.rising` / `.falling`) belongs in **either** the sensitivity list **or** an `if` inside the body, not both for the same signal.
///

## Initial blocks

An **`initial` block** describes **once-only initialization**, available in the RT and ED domains (in the DF domain, initialization is expressed with declaration `init` only). It is written at the design (or domain) level: an `initial` block cannot be placed inside a process, cannot be nested inside another `initial` block, and cannot contain processes, steps, or waits.

```scala
class InitRT extends RTDesign:
  val EN: Boolean <> CONST = true
  val x   = SInt(16)     <> IN
  val y   = SInt(16)     <> OUT.REG
  val vec = SInt(16) X 4 <> VAR
  initial:
    if (EN) y.din := 0
    else y.din    := 1
    for (i <- 0 until 4)
      vec(i) := 0
  y.din := x + vec(0)
```

### Initialization semantics

- **RT with a reset**: the block content is re-applied on every reset assertion (it is lowered into the register reset branch). Because RT `initial` content is restricted to constant values, this is indistinguishable from once-only initialization.
- **RT without a reset, and ED**: power-on initialization (declaration initials / a Verilog `initial` block).

Register targets are assigned through `.din` inside `initial`, exactly as everywhere else in RT.

### Content rules

**RT domain**: restricted so the block can always be lowered into reset branches or declaration initials.

- Blocking assignments (`:=`, or `.din :=` for registers) whose right-hand side is **constant**.
- `for` loops (the loop iterator may index the assignment target, as in the `vec` example above).
- `if` / `match` conditionals whose guards and selectors are all constant (an iterator-dependent guard is *not* constant).
- No waits, no prints, no `while` loops, no non-blocking `:==`.

**ED domain**: additionally allows non-constant expressions, non-constant conditionals, `while` loops, local variables, and text output (`println`, `report`, `assert`). Waits and `:==` remain disallowed.

### Conflict rules

Checked at elaboration:

- A variable may be assigned by at most **one** `initial` block.
- A declaration `init` and an `initial`-block assignment of the same variable are **mutually exclusive**; pick one form.

### Compilation

- **Verilog**: an `initial begin ... end` block (all dialects).
- **VHDL**: the block is split per variable. A single constant assignment becomes a declaration default (`signal v : ... := ...`); multi-statement constant initialization (loops, constant-guarded conditionals) becomes a declaration default computed by a generated [static function][methods] (`pure function v_init return t is ... signal v : t := v_init;`); any remaining content (e.g. simulation-only prints) becomes a one-shot `process ... wait; end process`.
- **RT with a reset**: the content is merged into the register reset branch, as described above.

The compiler also *generates* `initial` blocks on its own: an RT process prologue made of constant assignments, combinational loops, and constant-guarded conditionals is lowered into one, which is what makes process-start initialization free (see [Cycle semantics](#cycle-semantics)).

## Assignments inside processes

### Blocking assignment `:=`

Takes effect immediately within the process. Use for combinational logic and for intermediate values that are read later in the same process.

```scala
process(all):
  val temp = a + b   // read a, b
  y := temp          // immediate update of y
```

### Non-blocking assignment `:==`

Schedules an update at the end of the current evaluation step. Use for registers and outputs that should not create combinational feedback within the same process.

```scala
process(clk):
  if (clk.rising)
    counter :== counter + 1   // register update
```

/// admonition | Rule of thumb
    type: tip
Use `:=` for combinational (e.g. in `process(all)` or combinational branches). Use `:==` for register and sequential outputs in clocked processes.
///

## Local variables

You can declare local variables inside a process with `VAR`; they are visible only within that process and help structure combinational or sequential logic.

```scala
process(all):
  val z = UInt(8) <> VAR
  if (x > 10)
    z := x + 1
  else
    z := x - 1
  y := z
```

You can also use plain Scala `val` declarations (without `<> VAR` or `<> CONST`) inside process blocks to name intermediate sub-expressions. These are DFHDL values created inline; they do not declare new ports or variables but serve as readable names for parts of a computation:

```scala
process(clk):
  if (clk.rising)
    val sum = a + b          // intermediate DFHDL value
    val overflow = sum(8)    // single-bit check
    if (overflow) result :== max_val
    else result :== sum.resize(8)
```

Do not use `<> CONST` or `<> VAR` modifiers inside processes for these intermediates; plain `val name = expr` is sufficient.

/// admonition | Local `VAR` in clocked processes become registers
    type: warning
Local `VAR` declared inside a clocked `process(clk):` block are synthesized as **flip-flop registers** in the generated Verilog, not combinational wires. This is because the DFHDL compiler treats any variable written inside a clocked process as sequential storage.

```scala
process(clk):
  if (clk.rising)
    // This VAR becomes a register in Verilog:
    val temp = UInt(8) <> VAR
    temp := x + 1
    y :== temp
```

If you need a purely combinational intermediate inside a clocked process, use a plain Scala `val` (without `<> VAR`) for simple expressions, or compute the intermediate in a separate `process(all):` block and read the result in the clocked process.
///

## Relation to design domains

| Domain | Processes |
|--------|-----------|
| **DF** | No processes and no `initial` blocks. Behavior is expressed with dataflow and `.prev`; the compiler introduces registers and eventually ED processes. |
| **RT** | **Clock-bound FSM process**: `process:` with optional step definitions (`def Name: Step = ...`), `onEntry`/`onExit`, waits, and loops. Compiled to a state register and match logic with exact cycle semantics (see [Cycle semantics](#cycle-semantics)). `initial` blocks with constant content, lowered into reset branches / declaration initials. Plain RT register code (no process) is also lowered to ED processes by the compiler. |
| **ED** | **Sensitivity-driven**: `process(sig1, sig2, ...)`, `process(all)`, and `process`. Full control over sensitivity and blocking vs non-blocking assignment. `initial` blocks for power-on initialization and simulation-time output. |

See [Design Domains][design-domains] for the overall flow from DF → RT → ED and how processes fit into compilation.

## Summary

- **RT**: Use **`process:`** in **RTDesign** / **RTDomain** for a clock-bound FSM with **`def Name: Step = ...`**, **`NextStep`** / **`ThisStep`** / **`FirstStep`**, and optional **`onEntry`** / **`onExit`**, waits, and loops.
- **RT cycle semantics**: waits and steps consume exactly their own cycles; each loop iteration consumes at least one cycle, and loop control adds nothing beyond the body's cycles; nested first steps fuse into one state; a flat wait, a loop of waits, and nested loops of waits with the same total time consume identical cycle counts.
- **Process prologue**: statements before the first step (plus the first step's `onEntry`) run at initialization and at each forever wrap-around, costing zero cycles when they are constant assignments, combinational loops, or constant-guarded conditionals (lowered into a generated `initial` block).
- **`initial` blocks**: once-only initialization in RT (constants only; re-applied on reset) and ED (power-on; may include simulation output). A variable is initialized by declaration `init` or by one `initial` block, never both.
- A bare **`wait`** halts a process forever (terminal FSM state in RT; `wait;` / `wait(0);` in ED output).
- **ED**: Use **`process(sig1, sig2, ...)`** or **`process(all)`** in **EDDesign** / **EDDomain** to define when a block runs; **`process(all)`** for combinational logic; **`process(clk)`** (and optionally **`process(clk, rst)`**) with **`clk.rising`** / **`clk.falling`** for sequential logic.
- Use **`:=`** for immediate (blocking) updates and **`:==`** for register (non-blocking) updates in ED processes.
- Processes cannot be nested and are not available in the DF domain.

[design-domains]: ../design-domains/index.md
[din]: ../design-domains/index.md#din-read
[loops]: ../loops/index.md
[methods]: ../methods/index.md#static-functions
