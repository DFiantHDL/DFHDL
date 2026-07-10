# Loops

DFHDL supports loops in several contexts with different semantics depending on the domain and placement.

## Elaboration-Time Loops (Generate Loops)

Scala `for` loops at design scope (outside processes) run at elaboration time. They unroll into repeated hardware -- equivalent to Verilog `generate for`. The generated HDL contains no loop; each iteration produces distinct instances.

```scala
class Pipeline(
  val STAGES: Int <> CONST = 4
) extends EDDesign:
  val din  = UInt(8) <> IN
  val dout = UInt(8) <> OUT

  // Elaboration-time loop: unrolls into STAGES instances
  val stages = for i <- 0 until STAGES.toScalaInt yield
    val s = new Stage(IDX = i)
    s
  // Connect chain...
end Pipeline
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

[processes]: ../processes/index.md
