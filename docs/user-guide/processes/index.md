# Processes

Processes define *when* a block of logic runs. DFHDL supports processes in two domains:

- **RT domain**: A **clock-bound** process used to describe finite-state machines (FSMs). The process runs in lockstep with the domain clock and uses steps, waits, and control flow that the compiler lowers to registers and combinational logic.
- **ED domain**: **Sensitivity-driven** processes that run when listed signals change (or all read signals with `process(all)`), giving the same level of control as `process` in VHDL or `always` in Verilog.

Processes are not available in the dataflow (DF) domain. Processes cannot be nested inside another process.

## RT domain: clock-bound FSM process

In an [RT design][design-domains], a process is used to describe a finite-state machine that is **clock-bound**: it advances on the domain clock and is compiled to a state register plus combinational next-state and output logic.

### Syntax: `process:`

Use the argument-less `process:` inside an `RTDesign` or `RTDomain`. The block contains either plain combinational logic (assignments, no steps) or step definitions that form an FSM.

### Step-based FSM

Define states as `def Name: Step = ...` and control flow with:

- **`NextStep`**: advance to the next step in definition order.
- **`ThisStep`**: stay in the current step for another cycle.
- **`FirstStep`**: go to the first step (e.g. reset to initial state).
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

Steps may be nested: a `def Name: Step` body can define further steps, which execute in definition order within their parent. Statements written between two steps belong to the cycle in which the earlier step completes. After the last step of the process completes, the FSM wraps around to the first step.

### Cycle semantics

An RT process advances in lockstep with the domain clock, and DFHDL defines exactly how many cycles each construct consumes. The rules are designed so that composing the same total behavior in different ways costs the same number of cycles.

**Rule 1: a time-consuming construct consumes exactly its own time, and nothing else does.**
The time-consuming constructs are:

- **Cycle waits**: `n.cy.wait` consumes exactly `n` cycles. `1.cy.wait` is a single-cycle delay.
- **Timed waits**: `100.ms.wait`, `5.us.wait`, etc. are converted to a cycle count using the domain clock rate and then behave like cycle waits.
- **Condition waits**: `waitUntil(cond)` and `waitWhile(cond)` sample their condition once per cycle and therefore consume at least one cycle.
- **Steps**: entering a step consumes one cycle (a registered state transition). A step containing only a jump, such as `def S: Step = NextStep`, is a one-cycle delay. Sequential steps consume one cycle each.

**Rule 2: control flow is free.**
Loop entry, loop exit, and the loop-back edge of `for` and `while` loops consume zero extra cycles. A loop costs exactly the sum of the cycles its body actually consumes, so a loop that runs zero iterations costs zero cycles. Consequently, all of the following consume exactly the same number of cycles:

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

**Rule 3: nested first steps fuse.**
When the first time-consuming action of a step is entering an inner step, the outer and inner entries are a single entry and share one cycle (they are the same FSM "label"). Statements before the inner entry (assignments, prints) execute in that shared cycle. This rule applies recursively, so this process consumes one cycle per wrap-around, not three:

```scala
process:
  def S1: Step =
    def S2: Step =
      def S3: Step =
        NextStep
      NextStep
    NextStep
```

Rule 2 is a consequence of Rule 3: a loop is internally a step whose first action, on every iteration, is entering its body, so the iteration boundary fuses with the body's last cycle.

**Guard sampling.** Because loop boundaries are free, a loop guard is evaluated combinationally in the transition cycle, using the values that registers will hold in the *next* cycle. For example, when an iteration ends with the implicit increment `i.din := i + 1`, the guard `i < 100` is evaluated in that same cycle as `(i + 1) < 100`. External signals (ports) read by a guard are sampled in the transition cycle. This matches how a hand-written FSM decides its next state.

**Reset entry.** After reset the FSM starts at its first state. When the process begins with a construct that requires entry work (for example, a loop guard evaluation or an iterator initialization), one bootstrap cycle is consumed once at process start. This cost is paid only at reset, never per iteration or per wrap-around.

/// admonition | When a control cycle is still consumed
    type: note
Fusion falls back to the previous behavior of one extra control cycle (per loop entry and per iteration boundary) when the loop or step boundary cannot be resolved combinationally:

- A `while` guard that reads registers assigned *conditionally* or *partially* inside the loop body (the next-cycle guard value cannot be expressed at the boundary).
- Nested loops whose inner iteration count is not statically known (for example, `for (j <- 0 until n)` with a dynamic `n` inside an outer loop). Single dynamic loops still fuse; only the re-entry of a dynamic nest keeps a control state.
- Steps that carry `onEntry`, `onExit`, or `fallThrough` blocks.
- Steps whose jump dispatch is a `match` rather than `if` conditionals.

These cases are deterministic: a given loop shape either always fuses or always keeps its control cycle.
///

### Waits

RT processes can use **cycle waits** (`1.cy.wait`, `n.cy.wait`), **timed waits** (`100.ms.wait`, converted through the domain clock rate), and **condition waits** (`waitUntil(cond)`, `waitWhile(cond)`). The compiler converts these into step blocks and counters so that the behavior remains clock-bound and synthesizable. A wait can be named (`val MyWait = 1.cy.wait`) to control the generated step and counter names.

### Loops

`for` and `while` loops inside an RT process describe sequential (multi-cycle) iteration, following the cycle semantics above: each executed iteration costs exactly its body's cycles, and the loop boundaries are free. Loop iterators become registers.

```scala
process:
  for (i <- 0 until 8)
    dataOut.din := mem(i)
    1.cy.wait          // one cycle per element: 8 cycles total
```

A `while` loop with a body that consumes no cycles samples its guard once per cycle (one cycle per iteration), which is exactly the behavior of `waitUntil`/`waitWhile`.

Inside an RT loop, the **`FALL_THROUGH`** statement marks the loop to fall through to the next step without consuming any cycles when its guard is false on entry.

For elaboration-time (unrolled) loops outside processes, see [Loops][loops].

### fallThrough

A step can define **`def fallThrough = cond`** where `cond` is a Boolean/Bit expression. When the condition holds, the step advances to the next step in the same cycle (conditional advancement); when it does not, the FSM stays in the current step.

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

You can also use plain Scala `val` declarations (without `<> VAR` or `<> CONST`) inside process blocks to name intermediate sub-expressions. These are DFHDL values created inline -- they do not declare new ports or variables but serve as readable names for parts of a computation:

```scala
process(clk):
  if (clk.rising)
    val sum = a + b          // intermediate DFHDL value
    val overflow = sum(8)    // single-bit check
    if (overflow) result :== max_val
    else result :== sum.resize(8)
```

Do not use `<> CONST` or `<> VAR` modifiers inside processes for these intermediates -- plain `val name = expr` is sufficient.

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
| **DF** | No processes. Behavior is expressed with dataflow and `.prev`; the compiler introduces registers and eventually ED processes. |
| **RT** | **Clock-bound FSM process**: `process:` with optional step definitions (`def Name: Step = ...`), `onEntry`/`onExit`, waits, and loops. Compiled to a state register and match logic with exact cycle semantics (see [Cycle semantics](#cycle-semantics)). Plain RT register code (no process) is also lowered to ED processes by the compiler. |
| **ED** | **Sensitivity-driven**: `process(sig1, sig2, ...)`, `process(all)`, and `process`. Full control over sensitivity and blocking vs non-blocking assignment. |

See [Design Domains][design-domains] for the overall flow from DF → RT → ED and how processes fit into compilation.

## Summary

- **RT**: Use **`process:`** in **RTDesign** / **RTDomain** for a clock-bound FSM with **`def Name: Step = ...`**, **`NextStep`** / **`ThisStep`** / **`FirstStep`**, and optional **`onEntry`** / **`onExit`**, waits, and loops.
- **RT cycle semantics**: waits and steps consume exactly their own cycles; loop entry, exit, and loop-back are free; nested first steps fuse into one state; a flat wait, a loop of waits, and nested loops of waits with the same total time consume identical cycle counts.
- **ED**: Use **`process(sig1, sig2, ...)`** or **`process(all)`** in **EDDesign** / **EDDomain** to define when a block runs; **`process(all)`** for combinational logic; **`process(clk)`** (and optionally **`process(clk, rst)`**) with **`clk.rising`** / **`clk.falling`** for sequential logic.
- Use **`:=`** for immediate (blocking) updates and **`:==`** for register (non-blocking) updates in ED processes.
- Processes cannot be nested and are not available in the DF domain.

[design-domains]: ../design-domains/index.md
[loops]: ../loops/index.md
