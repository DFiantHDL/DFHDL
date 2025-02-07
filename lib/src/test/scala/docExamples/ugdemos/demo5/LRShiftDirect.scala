//format: off
package docExamples.ugdemos.demo5
import dfhdl.*
given options.CompilerOptions.Backend = backends.verilog

/** A generic left shifter 
  *   
  * @param width
  *   the width of the input and output bits
  */
class LeftShiftGen(
    val width: Int <> CONST,
) extends RTDesign:
  /** bits input */
  val iBits = Bits(width)       <> IN
  /** requested shift */
  val shift = UInt.until(width) <> IN
  /** bits output */
  val oBits = Bits(width)       <> OUT
  oBits := iBits << shift

/** A generic right shifter 
  *   
  * @param width
  *   the width of the input and output bits
  */
class RightShiftGen(
    val width: Int <> CONST,
) extends RTDesign:
  /** bits input */
  val iBits = Bits(width)       <> IN
  /** requested shift */
  val shift = UInt.until(width) <> IN
  /** bits output */
  val oBits = Bits(width)       <> OUT
  oBits := iBits >> shift

enum ShiftDir extends Encode:
  case Left, Right

/** A left-right bits shifter, direct composition
  *
  * @param width
  *   the width of the input and output bits
  */
@top class LRShiftDirect(
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
  val lshifter = LeftShiftGen(width)
  val rshifter = RightShiftGen(width)
  lshifter.iBits <> iBits
  lshifter.shift <> shift
  rshifter.iBits <> iBits
  rshifter.shift <> shift
  dir match
    case ShiftDir.Left  => oBits := lshifter.oBits
    case ShiftDir.Right => oBits := rshifter.oBits
end LRShiftDirect
