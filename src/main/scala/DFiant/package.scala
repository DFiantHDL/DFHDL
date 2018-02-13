//import DFiant.fixedpoint.DFUFix

/**
  * Created by soronpo on 08/04/2017.
  */

import DFiant.core.DFUInt

trait Implicits extends DFUInt.Op.Implicits

package object DFiant extends Implicits {
  trait DFDesign extends core.DFDesign
  val DFDesign = core.DFDesign
  object GlobalDesign {
    implicit object dsn extends DFDesign
  }
  type Φ = core.Bubble
  final val Φ = core.Bubble
  type DFBits[W] = core.DFBits[W]
  val DFBits = core.DFBits
  type DFBool = core.DFBool
  val DFBool = core.DFBool
  type DFUInt[W] = core.DFUInt[W]
  val DFUInt = core.DFUInt
  val ifdf = core.ifdf

}
