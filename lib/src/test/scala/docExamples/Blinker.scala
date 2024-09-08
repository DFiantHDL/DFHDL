package docExamples.led_blinker
// scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
import dfhdl.* //import all the DFHDL goodness

/** This is a led blinker */
@top class Blinker(
    val CLK_FREQ_KHz: Int <> CONST = 50000,
    val LED_FREQ_Hz: Int <> CONST  = 1
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
// Select backend compiler:
given options.CompilerOptions.Backend = backends.verilog
// Uncomment to enable printing design code after elaboration (before compilation):
// given options.ElaborationOptions.PrintDFHDLCode = true
// Uncomment to enable printing design code after compilation:
// given options.CompilerOptions.PrintDFHDLCode = true
////////////////////////////////////////////////////////////////////////////////////////////////
