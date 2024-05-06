---
hide:
  - toc
---

# 1-Bit Full Adder

```scastie 
import dfhdl.*

class FullAdder1 extends EDDesign:
  val a, b, c_in = Bit <> IN
  val sum, c_out = Bit <> OUT

  sum <> (a ^ b ^ c_in)
  c_out <> (a && b || b && c_in || c_in && a)

//The entry point to your compilation program starts here
@main def main =
  // Uncomment to select vhdl compilation (default is verilog):
  // given options.CompilerOptions.Backend = backends.vhdl

  // instantiate the design as top-level
  FullAdder1()
    .printCodeString // print design after elaboration in DFHDL syntax
    .compile // compile according to the selected backend dialect
    .printGenFiles // print generated backend files
```

