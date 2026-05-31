# Processes

Processes define *when* a block of logic runs. DFHDL supports processes in two domains:

- **RT domain**: A **clock-bound** process used to describe finite-state machines (FSMs). The process runs in lockstep with the domain clock and uses steps, waits, and control flow that the compiler lowers to registers and combinational logic.
- **ED domain**: **Sensitivity-driven** processes that run when listed signals change (or all read signals with `process(all)`), giving the same level of control as `process` in VHDL or `always` in Verilog.

Processes are not available in the dataflow (DF) domain. Processes cannot be nested inside another process.

## RT domain: clock-bound FSM process

In an [RT design][design-domains], a process is used to describe a finite-state machine that is **clock-bound**: it advances on the domain clock and is compiled to a state register plus combinational next-state and output logic.

### Syntax: `process` / `process.forever`

Use the shorthand `process:` (or `process.forever`) inside an `RTDesign` or `RTDomain`. The block contains either plain combinational logic (assignments, no steps) or step definitions that form an FSM.

### Step-based FSM

Define states as `def Name: Step = ...` and control flow with:

- **`NextStep`** — advance to the next step in definition order.
- **`ThisStep`** — stay in the current step for another cycle.
- **`FirstStep`** — go to the first step (e.g. reset to initial state).
- **Step name** (e.g. `S1`, `S2`) — jump to that step.

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

### fallThrough

A step can define **`def fallThrough = cond`** where `cond` is a Boolean/Bit expression. When the condition holds, the step advances to the next step in the same cycle (conditional advancement); when it does not, the FSM stays in the current step.

### onEntry and onExit

Inside a step you can define:

- **`def onEntry = ...`** — run when entering the step (once per transition into this state).
- **`def onExit = ...`** — run when leaving the step (once per transition out of this state).

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

### Waits and loops

RT processes can use **cycle waits** (e.g. `1.cy.wait`, `n.cy.wait`) and **wait conditions** (`waitUntil(cond)`, `waitWhile(cond)`). The compiler converts these into step blocks and counters so that the behavior remains clock-bound and synthesizable.

### Process with no steps

If the process body has no step definitions, it is purely combinational and runs every cycle:

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

### Forever process: `process.forever` / `process`

A process with no sensitivity list runs continuously. It is allowed in RT and ED, but **not** in DF. The shorthand `process:` (no arguments) is rewritten by the compiler to `process.forever`.

- **In RT**: `process:` is the [clock-bound FSM process](#rt-domain-clock-bound-fsm-process) described above (steps, waits, etc.).
- **In ED**: Use it for testbenches or clock generation (e.g. toggling a clock with `wait`).

```scala
class Testbench extends EDDesign:
  val clk = Bit <> VAR
  process.forever:
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

/// admonition | ED is a faithful mirror of Verilog/VHDL — write synthesizable patterns
    type: warning
The ED domain is intentionally a low-level, faithful mapping to Verilog `always` blocks / VHDL `process` blocks. **DFHDL does not enforce synthesizability in the ED domain.** If you describe a non-synthesizable process pattern, the generated Verilog/VHDL will faithfully reflect that pattern — and downstream synthesis (or even a parser like Yosys) may reject it.

It is your responsibility to write process bodies that match a synthesizable template. In particular, when an edge is **already in the sensitivity list**, do not re-check that edge inside the body — the body has already been triggered by it.

**Non-synthesizable** (clock edge appears both in sensitivity list and as a nested `else if`):

```scala
// BAD: emits `else if (posedge clk)` inside an always_ff — not valid Verilog
process(clk.rising, rst.rising):
  if (rst)
    x :== 0
  else if (clk.rising)   // redundant — body already runs on rising clk
    x :== x + 1
```

**Synthesizable** equivalents — pick the style that matches the intent:

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

The rule of thumb: an edge qualifier (`.rising` / `.falling`) belongs in **either** the sensitivity list **or** an `if` inside the body — not both for the same signal.
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
| **RT** | **Clock-bound FSM process**: `process:` (or `process.forever`) with optional step definitions (`def Name: Step = ...`), `onEntry`/`onExit`, and waits. Compiled to a state register and match logic. Plain RT register code (no process) is also lowered to ED processes by the compiler. |
| **ED** | **Sensitivity-driven**: `process(sig1, sig2, ...)`, `process(all)`, and `process.forever` / `process`. Full control over sensitivity and blocking vs non-blocking assignment. |

See [Design Domains][design-domains] for the overall flow from DF → RT → ED and how processes fit into compilation.

## Summary

- **RT**: Use **`process:`** in **RTDesign** / **RTDomain** for a clock-bound FSM with **`def Name: Step = ...`**, **`NextStep`** / **`ThisStep`** / **`FirstStep`**, and optional **`onEntry`** / **`onExit`** and waits.
- **ED**: Use **`process(sig1, sig2, ...)`** or **`process(all)`** in **EDDesign** / **EDDomain** to define when a block runs; **`process(all)`** for combinational logic; **`process(clk)`** (and optionally **`process(clk, rst)`**) with **`clk.rising`** / **`clk.falling`** for sequential logic.
- Use **`:=`** for immediate (blocking) updates and **`:==`** for register (non-blocking) updates in ED processes.
- Processes cannot be nested and are not available in the DF domain.

[design-domains]: ../design-domains/index.md
