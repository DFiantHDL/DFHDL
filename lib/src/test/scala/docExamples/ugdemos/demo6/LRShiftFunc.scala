//format: off
package docExamples.ugdemos.demo6
import dfhdl.*
given options.CompilerOptions.Backend = backends.verilog

/** A generic left shifter */
def LeftShiftGen(
  iBits: Bits[Int] <> VAL,
  shift: UInt[Int] <> VAL
): Bits[Int] <> RTRET = iBits << shift

/** A generic right shifter */
def RightShiftGen(
  iBits: Bits[Int] <> VAL,
  shift: UInt[Int] <> VAL
): Bits[Int] <> RTRET = iBits >> shift

enum ShiftDir extends Encoded:
  case Left, Right

/** A left-right bits shifter, functional composition */
def LRShiftFunc(
  iBits: Bits[Int] <> VAL,
  shift: UInt[Int] <> VAL,
  dir: ShiftDir <> VAL
): Bits[Int] <> RTRET =
  dir match
    case ShiftDir.Left  => LeftShiftGen(iBits, shift)
    case ShiftDir.Right => RightShiftGen(iBits, shift)

/** A left-right bits shifter wrapper
  * (required as top-level design for functional composition)
  *
  * @param width
  *   the width of the input and output bits
  */
@top class LRShiftFuncWrapper(
    val width: Int <> CONST = 8
) extends RTDesign:
  /** bits input */
  val iBits = Bits(width) <> IN
  /** requested shift */
  val shift = UInt.until(width) <> IN
  /** bits output */
  val oBits = Bits(width) <> OUT
  /** direction of shift */
  val dir   = ShiftDir <> IN

  oBits <> LRShiftFunc(iBits, shift, dir)
end LRShiftFuncWrapper
