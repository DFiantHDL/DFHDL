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

Unlike Verilog `generate if`, DFHDL type-checks **both** branches of an `if` expression at elaboration time, regardless of the parameter value. This means both branches must be type-correct for all possible parameter values:

```scala
// PROBLEM: when DEPTH == 1, the else branch has an invalid slice
if (DEPTH == 1)
  out := in
else
  out := (in, data(WIDTH - 1, ELEM_WIDTH))  // invalid range when DEPTH=1

// SOLUTION: use .resize or guard index computations
if (DEPTH == 1)
  out := in.resize(WIDTH)
else
  out := (in, data.msbits(WIDTH - ELEM_WIDTH))
```

## ED Domain Loops

In ED designs, `for` and `while` loops inside processes produce combinational or sequential logic depending on the process type. These loops are unrolled by the compiler.

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
