---
hide:
  - toc
---

# N-Bit Full Adder

```scastie 
import dfhdl.* //import all the DFHDL goodness

class FullAdder1 extends EDDesign:
  val a, b, c_in = Bit <> IN
  val sum, c_out = Bit <> OUT

  sum   <> (a ^ b ^ c_in)
  c_out <> (a && b || b && c_in || c_in && a)

@top class FullAdderN(val n: Int = 4) extends EDDesign:
  val a, b  = Bits(n) <> IN
  val c_in  = Bit     <> IN
  val sum   = Bits(n) <> OUT
  val c_out = Bit     <> OUT

  val adder = List.fill(n)(FullAdder1())
  for (i <- 0 until n)
    adder(i).a   <> a(i)
    adder(i).b   <> b(i)
    adder(i).sum <> sum(i)
    if (i < n - 1)
      adder(i).c_out <> adder(i + 1).c_in
  adder.head.c_in  <> c_in
  adder.last.c_out <> c_out

////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Compiler Options:                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////
// Enables printing the generated chosen backend code:
given options.CompilerOptions.PrintGenFiles = true
// Uncomment to select vhdl compilation (default is verilog):
// given options.CompilerOptions.Backend = backends.vhdl
// Uncomment to enable printing design code before compilation (after elaboration):
// given options.CompilerOptions.PrintDesignCodeBefore = true
// Uncomment to enable printing design code after compilation:
// given options.CompilerOptions.PrintDesignCodeAfter = true
////////////////////////////////////////////////////////////////////////////////////////////////
```

