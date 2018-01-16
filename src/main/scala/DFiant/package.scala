//import DFiant.fixedpoint.DFUFix

/**
  * Created by soronpo on 08/04/2017.
  */
import DFiant.internals._
import DFiant.tokens._

package object DFiant {
  type DFBits[W] = core.DFBits[W]
  val DFBits = core.DFBits
  type DFBool = core.DFBool
  val DFBool = core.DFBool
  type DFUInt[W] = core.DFUInt[W]
  val DFUInt = core.DFUInt
  val ifdf = core.ifdf


  implicit class FromInt[L <: Int](value : L) { //(implicit g : AcceptNonLiteral[GetArg0])
    import DFUInt.Operations._
    //    final val left = DFUInt.const[LW](TokenUInt(w(value), value))
    type Extendable
    def extendable : FromInt[L] with DFUInt.Extendable = new FromInt[L](value) with DFUInt.Extendable
    def + [LW, RW](that : DFUInt[RW])(implicit w : BitsWidthOf.IntAux[L, LW], op: `Op+`.Builder[LW, Extendable, DFUInt[RW]]) = op(DFUInt.const[LW](TokenUInt(w(value), value)), that)
  }

}
