//format: off
package docExamples.ugdemos.demo2
import dfhdl.*
given options.CompilerOptions.Backend = backends.verilog

/** A basic left shifter */
@top class LeftShiftBasic(
    val width: Int = 8
) extends RTDesign:
  /** bits input */
  val iBits = Bits(width) <> IN
  /** requested shift */
  val shift = UInt.until(width) <> IN
  /** bits output */
  val oBits = Bits(width) <> OUT
  oBits := iBits << shift
