---
hide:
  - toc
---

# LED Blinker

```scastie 
import dfhdl.* //import all the DFHDL goodness

/** This is a led blinker */
@top class Blinker(
    val CLK_FREQ_KHz: Int <> CONST = 50000,
    val LED_FREQ_Hz:  Int <> CONST = 1
) extends RTDesign:
  /** Half-count of the toggle for 50% duty cycle */
  val HALF_PERIOD = (CLK_FREQ_KHz * 1000) / (LED_FREQ_Hz * 2)

  /** LED output */
  val led = Bit                     <> OUT.REG init 1
  val cnt = UInt.until(HALF_PERIOD) <> VAR.REG init 0
  if (cnt == HALF_PERIOD - 1)
    cnt.din := 0
    led.din := !led
  else cnt.din := cnt + 1
end Blinker

////////////////////////////////////////////////////////////////////////////////////////////////
// DFHDL Elaboration Options:                                                                 //
////////////////////////////////////////////////////////////////////////////////////////////////
// Uncomment to set different clock and reset configurations:
// given options.ElaborationOptions.DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising)
// given options.ElaborationOptions.DefaultRstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.Low)
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

