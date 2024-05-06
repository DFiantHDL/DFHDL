---
hide:
  - toc
---

# Counter

```scastie 
import dfhdl.*

class Counter(val width: Int <> CONST) extends RTDesign:
  val cnt = UInt(width) <> OUT.REG init 0
  cnt.din := cnt + 1

@main def main = 
  // Uncomment to select vhdl compilation (default is verilog):
  // given options.CompilerOptions.Backend = backends.vhdl

  // Uncomment to set different clock and reset configurations:
  // given options.CompilerOptions.DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising)
  // given options.CompilerOptions.DefaultRstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.Low)

  // instantiate the design as top-level 8-bit counter
  Counter(8)
    .printCodeString // print design after elaboration in DFHDL syntax
    .compile // compile according to the selected backend dialect
    .printCodeString // print design after compilation in DFHDL syntax
    .printGenFiles // print generated backend files
```

