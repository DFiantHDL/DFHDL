# Loops

DFHDL supports loops in several contexts with different semantics depending on the domain and placement.

## Elaboration-Time Loops (Generate Loops)

By default, `for` loops in a concurrent scope (a design or domain body, not within a function, procedure, or process) run at elaboration time: their range is an ordinary Scala range, even when the range arguments are DFHDL `Int` values (such as `Int <> CONST` parameters, whose values are read during elaboration). The loop unrolls into repeated hardware, equivalent to Verilog `generate for`, so the generated HDL contains no loop; each iteration produces distinct instances.

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

`COMB_LOOP` is a block wrapper because it marks a whole region: a loop nested inside a combinational loop cannot consume cycles either, so it is combinational too. The other RT loop annotation, `FALL_THROUGH`, marks a single loop and is written on that loop's own condition or range instead (see [Processes][processes]).

[processes]: ../processes/index.md
