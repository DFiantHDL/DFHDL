//format: off
package docExamples.ugdemos.demo3
import dfhdl.*
given options.CompilerOptions.Backend = backends.verilog

/** A generic left shifter 
  *   
  * @param width
  *   the width of the input and output bits
  */
@top class LeftShiftGen(
    val width: Int <> CONST = 8,
) extends RTDesign:
  /** bits input */
  val iBits = Bits(width)       <> IN
  /** requested shift */
  val shift = UInt.until(width) <> IN
  /** bits output */
  val oBits = Bits(width)       <> OUT
  oBits := iBits << shift
