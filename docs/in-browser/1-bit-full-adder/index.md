---
hide:
  - toc
---

# 1-Bit Full Adder {#run-in-browser}

```scastie 
import dfhdl.* //import all the DFHDL goodness

@top class FullAdder1 extends EDDesign:
  val a, b, c_in = Bit <> IN
  val sum, c_out = Bit <> OUT

  sum <> (a ^ b ^ c_in)
  c_out <> (a && b || b && c_in || c_in && a)

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

