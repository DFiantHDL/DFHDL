//format: off
package docExamples.ugdemos.demo5
import dfhdl.*

/** A left-right bits shifter, via composition
  *
  * @param width
  *   the width of the input and output bits
  */
@top class LRShiftVia(
    val width: Int <> CONST = 8
) extends ShiftGen:
  parent => //parent design reference
  /** direction of shift */
  val dir = ShiftDir <> IN
  val lshifter_oBits = Bits(width) <> VAR
  val lshifter = new LeftShiftGen(width):
    iBits <> parent.iBits
    shift <> parent.shift
    oBits <> lshifter_oBits
  val rshifter_oBits = Bits(width) <> VAR
  val rshifter = new RightShiftGen(width):
    iBits <> parent.iBits
    shift <> parent.shift
    oBits <> rshifter_oBits
  oBits := dir match
    case ShiftDir.Left  => lshifter_oBits
    case ShiftDir.Right => rshifter_oBits
end LRShiftVia