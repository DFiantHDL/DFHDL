//format: off
package docExamples.ugdemos.demo5
import dfhdl.*
given options.CompilerOptions.Backend = backends.verilog

/** A generic abstract shifter with only IOs */
abstract class ShiftGen extends RTDesign:
  /** the width of the input and output bits */
  val width: Int <> CONST //abstract
  /** bits input */
  val iBits = Bits(width) <> IN
  /** requested shift */
  val shift = UInt.until(width) <> IN
  /** bits output */
  val oBits = Bits(width) <> OUT

/** A generic left shifter 
  *   
  * @param width
  *   the width of the input and output bits
  */
class LeftShiftGen(
    val width: Int <> CONST
) extends ShiftGen:
  oBits := iBits << shift

/** A generic right shifter 
  *   
  * @param width
  *   the width of the input and output bits
  */
class RightShiftGen(
    val width: Int <> CONST
) extends ShiftGen:
  oBits := iBits >> shift

enum ShiftDir extends Encoded:
  case Left, Right

/** A left-right bits shifter, direct composition
  *
  * @param width
  *   the width of the input and output bits
  */
@top class LRShiftDirect(
    val width: Int <> CONST = 8
) extends ShiftGen:
  /** direction of shift */
  val dir   = ShiftDir <> IN
  val lshifter = LeftShiftGen(width)
  val rshifter = RightShiftGen(width)
  lshifter.iBits <> iBits
  lshifter.shift <> shift
  rshifter.iBits <> iBits
  rshifter.shift <> shift
  oBits := dir match
    case ShiftDir.Left  => lshifter.oBits
    case ShiftDir.Right => rshifter.oBits
end LRShiftDirect
