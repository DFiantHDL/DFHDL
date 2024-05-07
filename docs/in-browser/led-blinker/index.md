---
hide:
  - toc
---

# LED Blinker

```scastie 
import dfhdl.* //import all the DFHDL goodness

/** This is a led blinker */
class Blinker(
    val CLK_FREQ_KHz: Int <> CONST,
    val LED_FREQ_Hz:  Int <> CONST
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
@main def main = Blinker(CLK_FREQ_KHz = 50000, LED_FREQ_Hz = 1).compile
```

