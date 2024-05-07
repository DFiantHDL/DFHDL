---
hide:
  - toc
---

# Counter

```scastie 
import dfhdl.* //import all the DFHDL goodness

class Counter(val width: Int <> CONST) extends RTDesign:
  val en  = Bit         <> IN
  val cnt = UInt(width) <> OUT.REG init 0
  if (en)
    cnt.din := cnt + 1

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
// Uncomment to set different clock and reset configurations:
// given options.CompilerOptions.DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising)
// given options.CompilerOptions.DefaultRstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.Low)
////////////////////////////////////////////////////////////////////////////////////////////////

//The entry point to your compilation program starts here
@main def main = Counter(8).compile
```

