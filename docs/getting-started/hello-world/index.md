# Hello Hardware World {#hello-world}

<style>
div.scastie {
	font-size: small;
}	
</style>
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

<!-- The Scala code in Fig. 1b describes a program that runs the DFiant compiler on an identity function dataflow design, `ID`. Since DFiant is a Scala library some if its compilation process is done statically via the Scala compiler and the rest during the Scala runtime execution. 

!!! summary "Writing a DFiant compilation program – easy as 1-2-3!"

	1. `#!scala import DFiant._` to import all the required namespace fields
	2. `#!scala trait _design_name_ extends DFDesign {}` to define your dataflow design. Populate your design with the required dataflow functionality.
	3. `#!scala object _program_name_ extends DFApp.VHDLCompiler[_design_name_]` to create your compilation program entry point. -->

