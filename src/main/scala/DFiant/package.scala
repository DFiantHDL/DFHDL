//import DFiant.fixedpoint.DFUFix

/**
  * Created by soronpo on 08/04/2017.
  */

import DFiant.core.DFUInt

package object DFiant extends DFUInt.Implicits {
  trait DFDesign extends core.DFDesign
  val DFDesign = core.DFDesign
  val GlobalDesign = core.GlobalDesign
  type DFBits[W] = core.DFBits[W]
  val DFBits = core.DFBits
  type DFBool = core.DFBool
  val DFBool = core.DFBool
  type DFUInt[W] = core.DFUInt[W]
  val DFUInt = core.DFUInt
  val ifdf = core.ifdf

}