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

//The entry point to your compilation program starts here
@main def main =
  // Uncomment to select vhdl compilation (default is verilog):
  // given options.CompilerOptions.Backend = backends.vhdl

  // Uncomment to set different clock and reset configurations:
  // given options.CompilerOptions.DefaultClkCfg = ClkCfg(ClkCfg.Edge.Rising)
  // given options.CompilerOptions.DefaultRstCfg = RstCfg(RstCfg.Mode.Async, RstCfg.Active.Low)

  // instantiate the design as top-level with 50MHz clock and 1Hz led toggle defaults
  Blinker(CLK_FREQ_KHz = 50000, LED_FREQ_Hz = 1)
    .printCodeString // print design after elaboration in DFHDL syntax
    .compile // compile according to the selected backend dialect
    .printCodeString // print design after compilation in DFHDL syntax
    .printGenFiles // print generated backend files
```

