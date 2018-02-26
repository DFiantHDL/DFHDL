//import DFiant.fixedpoint.DFUFix

/**
  * Created by soronpo on 08/04/2017.
  */

import DFiant.core.DFUInt

trait Implicits extends DFUInt.Op.Implicits

package object DFiant extends {
  trait DFDesign extends core.DFDesign with Implicits
  val DFDesign = core.DFDesign
  object GlobalDesign extends Implicits {
    implicit object dsn extends DFDesign
  }
  trait DFBlackBox[Dsn <: core.DFDesign] extends core.DFBlackBox[Dsn]
  val DFBlackBox = core.DFBlackBox
  type Φ = core.Bubble
  final val Φ = core.Bubble
  type DFAny = core.DFAny
  val DFAny = core.DFAny
  type DFBits[W] = core.DFBits[W]
  val DFBits = core.DFBits
  type DFBool = core.DFBool
  val DFBool = core.DFBool
  type DFUInt[W] = core.DFUInt[W]
  val DFUInt = core.DFUInt
  val ifdf = core.ifdf

}
