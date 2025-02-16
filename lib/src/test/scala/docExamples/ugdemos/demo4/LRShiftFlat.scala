//format: off
package docExamples.ugdemos.demo4
import dfhdl.*
given options.CompilerOptions.Backend = backends.verilog

enum ShiftDir extends Encode:
  case Left, Right

/** A left-right bits shifter (flat version)
  *
  * @param width
  *   the width of the input and output bits
  */
@top class LRShiftFlat(
    val width: Int <> CONST = 8
) extends RTDesign:
  /** bits input */
  val iBits = Bits(width)       <> IN
  /** requested shift */
  val shift = UInt.until(width) <> IN
  /** direction of shift */
  val dir   = ShiftDir          <> IN
  /** bits output */
  val oBits = Bits(width)       <> OUT
  oBits := dir match
    case ShiftDir.Left  => iBits << shift
    case ShiftDir.Right => iBits >> shift
