---
hide:
  - toc
---

# Counter

```scastie 
import dfhdl.* //import all the DFHDL goodness

@top class Counter(val width: Int <> CONST = 8) extends RTDesign:
  val en  = Bit         <> IN
  val cnt = UInt(width) <> OUT.REG init 0
  if (en)
    cnt.din := cnt + 1

////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Elaboration Options:                                                                 //
////////////////////////////////////////////////////////////////////////////////////////////////
// Uncomment to set different clock and reset configurations:
// given options.ElaborationOptions.DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising)
// given options.ElaborationOptions.DefaultRstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.Low)
////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Compiler Options:                                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////
// Select backend compiler:
given options.CompilerOptions.Backend = backends.verilog
// Enables printing the generated chosen backend code:
given options.CompilerOptions.PrintGenFiles = true
// Uncomment to enable printing design code before compilation (after elaboration):
// given options.CompilerOptions.PrintDesignCodeBefore = true
// Uncomment to enable printing design code after compilation:
// given options.CompilerOptions.PrintDesignCodeAfter = true
////////////////////////////////////////////////////////////////////////////////////////////////
```

