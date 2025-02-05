//format: off
package docExamples.ugdemos.demo1
import dfhdl.*
//optionally set the default backend configuration option
//(can be overridden by the top-app CLI)
given options.CompilerOptions.Backend = backends.verilog

/** A two-bits left shifter */
@top class LeftShift2 extends RTDesign:
  /** bits input */
  val iBits = Bits(8) <> IN
  /** bits output */
  val oBits = Bits(8) <> OUT
  oBits := iBits << 2
