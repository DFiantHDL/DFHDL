import dfhdl.*
import scala.annotation.nowarn

given options.CompilerOptions.Backend = backends.verilog
given options.CompilerOptions.LogLevel = options.LogLevel.TRACE

/** A generic abstract shifter
  *
  * @param width
  *   the width of the input and output bits
  */
abstract class ShiftGen extends RTDesign:
  val width: Int <> CONST

  /** bits input */
  val iBits = Bits(width) <> IN

  /** requested shift */
  val shift = UInt.until(width) <> IN

  /** bits output */
  val oBits = Bits(width) <> OUT

class LeftShiftGen(
    val width: Int <> CONST
) extends ShiftGen:
  oBits := iBits << shift

class RightShiftGen(
    val width: Int <> CONST
) extends ShiftGen:
  oBits := iBits >> shift

enum ShiftDir extends Encode:
  case Left, Right

@top class LRShiftGen(
    val width: Int <> CONST = 8
) extends ShiftGen:
  /** direction of the shift */
  val dir = ShiftDir <> IN
  val lshifter = LeftShiftGen(width)
  val rshifter = RightShiftGen(width)
  lshifter.iBits <> iBits
  lshifter.shift <> shift
  rshifter.iBits <> iBits
  rshifter.shift <> shift
  dir match
    case ShiftDir.Left  => oBits := lshifter.oBits
    case ShiftDir.Right => oBits := rshifter.oBits
end LRShiftGen

@top class Counter(val width: Int <> CONST = 8) extends RTDesign:
  val cnt = UInt(width) <> OUT init 0
  cnt := cnt.reg + 1
