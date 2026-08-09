# Loops

DFHDL supports loops in several contexts with different semantics depending on the domain and placement.

## Elaboration-Time Loops (Generate Loops)

By default, `for` loops in a concurrent scope (a design or domain body, not within a function, procedure, or process) run at elaboration time: their range is an ordinary Scala range, even when the range arguments are DFHDL `Int` values (such as `Int <> CONST` parameters, whose values are read during elaboration). The loop unrolls into repeated hardware, equivalent to Verilog `generate for`, so the generated HDL contains no loop; each iteration produces distinct instances.

A range bound is the one place a DFHDL constant is read implicitly. Anywhere else that Scala needs the number (a `List` size, an index computation, a plain method argument) the constant has to be read explicitly with `.toScalaInt`. See [reading a constant into Scala][toScala].

```scala
// A design that adds 1 to its input
class Plus1 extends EDDesign:
  val x = UInt(8) <> IN
  val y = UInt(8) <> OUT
  y <> x + 1

// Chains four `Plus1` instances with an elaboration-time loop
class Plus4 extends EDDesign:
  val x = UInt(8) <> IN
  val y = UInt(8) <> OUT
  val plus1s = List.fill(4)(Plus1())
  plus1s.head.x <> x
  for (i <- 0 until 3)
    plus1s(i + 1).x <> plus1s(i).y
  y <> plus1s.last.y
end Plus4
```

When a design containing an elaboration-time loop is instantiated with different parameter values, DFHDL creates distinct elaborated designs (with enumerated names), each with a different number of unrolled instances.

### Elaboration-Time Conditionals

An `if` whose condition is a **constant** resolves during elaboration, so only the taken branch produces hardware. Both branches are still ordinary Scala code, though, so Scala type-checks both. Whether that rejects an untaken branch depends on whether the widths involved are **literal** or **parameterized**.

/// admonition | Which `if` you get depends on the domain
    type: note
In an **ED** design body, an implicit `.toScalaBoolean` is applied to a constant condition, so the `if` is a Scala `if` and resolves at elaboration.

In an **RT** or **DF** design body, an `if` is a DFHDL (hardware) `if` unless its condition is a Scala `Boolean`. Force that with `.toScalaBoolean` when you want the elaboration-time behavior:

```scala
if ((WIDTH == 4).toScalaBoolean)  // a Scala `if` in an RT/DF body
```

A DFHDL `if` elaborates **both** branches, so a width that is invalid in either one is an error regardless of which is taken:

```
The argument width (((WIDTH - 1) - (WIDTH - 2)) + 1) is different than the receiver width (WIDTH).
```
///

What decides the Scala-level check is the **type ascription** on the width, not the value. These two declarations look almost identical and behave differently:

```scala
val WIDTH = 4                  // a plain Scala Int: a literal the Scala type level tracks
val WIDTH: Int <> CONST = 4    // a DFHDL constant: unbounded at the Scala type level
```

**A plain Scala `Int`** gives `Bits(4)` the bounded type `Bits[4]`, so Scala tracks the width and rejects an invalid untaken branch at compile time:

```scala
class narrow_lit extends EDDesign:
  val WIDTH = 4                 // plain Scala Int
  val din  = Bits(WIDTH) <> IN
  val dout = Bits(WIDTH) <> OUT
  if (WIDTH == 4)
    dout <> din
  else
    dout <> din.msbits(2)       // rejected even though this branch is never taken
```

```
The argument width (2) is different than the receiver width (4).
Consider applying `.resize` to resolve this issue.
```

**An `Int <> CONST`** gives `Bits(WIDTH)` the unbounded type `Bits[Int]`, which the Scala type level does not track, so there is nothing for it to reject. The width check moves to elaboration, and elaboration only ever visits the taken branch:

```scala
class narrow_const extends EDDesign:
  val WIDTH: Int <> CONST = 4   // same value, ascribed as a DFHDL constant
  val din  = Bits(WIDTH) <> IN
  val dout = Bits(WIDTH) <> OUT
  if (WIDTH == 4)
    dout <> din
  else
    dout <> din.msbits(2)       // never elaborated, so never checked
```

The same holds for a width that arrives as a design parameter (`class narrow(val WIDTH: Int <> CONST = 4)`), which is the usual case when translating a Verilog `parameter`. This is why a `generate if` whose branches are each valid only for their own parameter value translates directly, with no `.resize` guard and no `.toScalaInt`. If you do need both branches valid at the Scala level, use `.resize` or guard the index computations, as in the plain-`Int` example above.

The ascription has a second, visible consequence: an `Int <> CONST` survives into the generated HDL as a `localparam`, while a plain Scala `Int` is inlined away. See [`localparam`][localparam] for that side of the same distinction.

## ED Domain Loops

In ED designs, `for` and `while` loops inside processes produce combinational or sequential logic depending on the process type. Unlike a design-scope loop, a loop inside a process **stays a loop**: it is elaborated once and emitted as a real `for` in the generated HDL, as the `OnesCount` example below shows.

That difference decides what its iterator is. A design-scope iterator is an ordinary Scala `Int`, so it can index Scala collections and be used anywhere Scala needs a number. A **process-scope iterator is a hardware value**, and cannot be read out into Scala at all, `.toScalaInt` included:

```scala
val lanes = List.fill(3)(new lane)  // a Scala collection, built at design scope
process(all):
  for (i <- 0 until LANES)
    out_bus.lsbitsAt(i * 8, 8) := lanes(i.toScalaInt).q  // error
```

```
Scala value access error!
Message:   Cannot fetch a Scala value from a non-constant DFHDL value.
```

The reported position is the `i` in the `for` binding rather than the use that actually needs a Scala value, so read the message as "something in this loop body wanted a Scala `Int`" and look at the uses, not the range.

To write per-index slices of a packed bus, do the work in a **design-scope** loop and give each iteration its own small process. The `i` is then a Scala value captured by closure, and no loop exists inside a process:

```scala
class Foo(val LANES: Int <> CONST = 3) extends EDDesign:
  val out_bus = Bits(8 * 3) <> OUT
  for (i <- 0 until LANES)
    val u = new lane
    process(all):
      out_bus.lsbitsAt(i * 8, 8) := u.q
```

/// tab | Generated Verilog
```verilog
module Foo#(parameter int LANES = 3)(
  output logic [23:0] out_bus
);
  logic [7:0] u_0_q;
  logic [7:0] u_1_q;
  logic [7:0] u_2_q;
  lane u_0(.q /*-->*/ (u_0_q));
  lane u_1(.q /*-->*/ (u_1_q));
  lane u_2(.q /*-->*/ (u_2_q));
  always_comb
  begin
    out_bus[7:0]   = u_0_q;
  end
  always_comb
  begin
    out_bus[15:8]  = u_1_q;
  end
  always_comb
  begin
    out_bus[23:16] = u_2_q;
  end
endmodule
```
The design-scope loop unrolls: three `lane` instances and three separate `always_comb` blocks, each writing one static slice. No loop remains.
///

/// tab | Generated VHDL
```vhdl
entity Foo is
generic (
  LANES : integer := 3
);
port (
  out_bus : out std_logic_vector(23 downto 0)
);
end Foo;

architecture Foo_arch of Foo is
  signal u_0_q : std_logic_vector(7 downto 0);
  signal u_1_q : std_logic_vector(7 downto 0);
  signal u_2_q : std_logic_vector(7 downto 0);
begin
  u_0 : entity work.lane(lane_arch) port map (q => u_0_q);
  u_1 : entity work.lane(lane_arch) port map (q => u_1_q);
  u_2 : entity work.lane(lane_arch) port map (q => u_2_q);
  process (all)
  begin
    out_bus(7 downto 0) <= u_0_q;
  end process;
  process (all)
  begin
    out_bus(15 downto 8) <= u_1_q;
  end process;
  process (all)
  begin
    out_bus(23 downto 16) <= u_2_q;
  end process;
end Foo_arch;
```
Same unrolling: three component instantiations and three processes, each driving one slice of `out_bus`.
///

Contrast this with the `OnesCount` example above, where the loop is **inside** the process and survives into the generated HDL as a real `for`.

Arithmetic on the iterator that stays inside DFHDL is fine either way: a process-scope `i` is a valid part-select base (see [Bit Selection and Slicing][common-bit-vector-ops]), and emits a variable-base part-select.

### Loops Accumulation Example {#loop-accumulators}

A loop that stays a hardware loop is elaborated once, not once per iteration, so a Scala `#!scala var` cannot accumulate across it. Reassigning the `var` in the body only rebinds the Scala name to a value built inside the loop, and reading it after the loop reaches the loop's own iterator, which does not exist outside it. Declaring a `#!scala var` in a process is a compile error for this reason, and the elaboration rejects the shape as a scope error wherever it is laundered in through a helper `#!scala def`. See [Scala `var` with DFHDL values][scala-var] for the full permission list.

Accumulate into a DFHDL variable instead, driven with `:=` inside the loop:

```scala title="Accumulating in hardware"
/** Counts the set bits of an 8-bit input */
class OnesCount extends EDDesign:
  /** the bits to count */
  val bits  = Bits(8) <> IN
  /** how many of them are set */
  val count = UInt(4) <> OUT
  process(all):
    //the running total, declared before the loop so
    //that it exists both inside it and after it
    val sum = UInt(4) <> VAR
    sum := 0
    for (i <- 0 until 8)
      if (bits(i)) sum := sum + 1
    count := sum
end OnesCount
```

/// tab | Generated Verilog
```verilog
module OnesCount(
  /* the bits to count */
  input  wire logic [7:0] bits,
  /* how many of them are set */
  output      logic [3:0] count
);
  `include "dfhdl_defs.svh"
  logic [3:0] sum;

  always_comb
  begin
    sum   = 4'd0;
    for (int i = 0; i < 8; i = i + 1) begin
      if (bits[i]) sum = sum + 4'd1;
    end
    count = sum;
  end
endmodule
```
The loop survives into the generated code as a real `for` loop, and `sum` is hoisted to a module-scope `logic`, since Verilog has no notion of a declaration local to an `always` block. DFHDL's `:=` becomes Verilog's blocking `=`, which is what makes the accumulation work: each iteration reads the value the previous one wrote.
///

/// tab | Generated VHDL
```vhdl
entity OnesCount is
port (
  -- the bits to count
  bits  : in  std_logic_vector(7 downto 0);
  -- how many of them are set
  count : out unsigned(3 downto 0)
);
end OnesCount;

architecture OnesCount_arch of OnesCount is
begin
  process (all)
    variable sum : unsigned(3 downto 0);
  begin
    sum   := 4d"0";
    for i in 0 to 8-1 loop
      if bits(i) then sum := sum + 4d"1";
      end if;
    end loop;
    count <= sum;
  end process;
end OnesCount_arch;
```
VHDL keeps the accumulator inside the process, as a `variable` rather than a `signal`. That distinction is not cosmetic: a signal assignment is scheduled and would not take effect until the process suspended, so every iteration would read the same stale value. A variable assignment (`:=`) takes effect immediately, which is the semantics DFHDL's `:=` carries. The output port stays a signal, so the final `count <= sum` is a signal assignment.

Declaring the accumulator in the **design body** rather than inside the process would make it a VHDL `signal`, and the accumulation would then be wrong. Keep a loop accumulator local to its process.
///

Contrast this with the [elaboration-time accumulation][scala-var] a Scala `#!scala var` is for, where the loop is a Scala loop and nothing of it survives into the generated code.

## RT Domain Loops

In RT designs, `for` and `while` loops inside processes create synthesizable procedural FSMs. The compiler transforms the loop body into state machine transitions. Loop iterators become registers, and the loop boundaries (entry, exit, and loop-back) consume zero extra cycles: each executed iteration costs exactly the cycles its body consumes, so a flat wait, a loop of waits, and nested loops of waits with the same total time are cycle-identical. See [Processes][processes] for the full RT cycle semantics.

### Combinational Loops (`COMB_LOOP`)

Wrapping a loop with a **`COMB_LOOP`** block marks it combinational: the whole loop executes within a single cycle. At RT design scope (outside processes), the wrapper also keeps the `for` range as a hardware range, so the loop is emitted as a single procedural loop in the generated HDL instead of unrolling at elaboration time. Like `FALL_THROUGH`, this annotation is allowed under RT domains only; applying it elsewhere is a compile-time error.

```scala
class Foo(
    val PORT_WIDTH: Int <> CONST = 5
) extends RTDesign:
  val r = Bits(PORT_WIDTH) <> OUT.REG init all(0)
  COMB_LOOP:
    for (i <- 0 until PORT_WIDTH)
      r(i).din := 1
end Foo
```

Inside an RT process, where loops are sequential (multi-cycle) by default, the same wrapper keeps a loop combinational; its body must not consume cycles.

`COMB_LOOP` is a block wrapper because it marks a whole region: a loop nested inside a combinational loop cannot consume cycles either, so it is combinational too. The other RT annotation, `FALL_THROUGH`, marks a single loop or condition wait and is written on that construct's own condition or range instead (see [Processes][processes]).

[processes]: ../processes/index.md
