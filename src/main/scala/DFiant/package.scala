//import DFiant.fixedpoint.DFUFix

/**
  * Created by soronpo on 08/04/2017.
  */

package object DFiant extends {
  type DFBits[W] = core.DFBits[W]
  val DFBits = core.DFBits
  type DFBool = core.DFBool
  val DFBool = core.DFBool
  type DFUInt[W] = core.DFUInt[W]
  val DFUInt = core.DFUInt
  val ifdf = core.ifdf


}