package ZFiant

import singleton.ops._
import singleton.twoface._
trait DFAnyMember
trait DFAny extends DFAnyMember {self =>
  type TVar
  type TToken <: DFAny.Token
  type Width
  val width : TwoFace.Int[Width]
}


object DFAny {
  trait Val[V <: DFAny, Var] extends DFAny {
    type TVar = Var
    final def bits : Val[DFBits[Width], Var] = DFAny.Alias.BitsWL(this)(width, 0)
    final def prev : Val[V, false] = DFAny.Alias.Prev(this)(1)
    final def as[MV <: DFAny](mold : Val[MV, _]) : Val[MV, Var] = DFAny.Alias.AsIs(mold)(this)
  }

  trait Token

  sealed abstract class Constructor[V <: DFAny, Var](val mold : DFAny.Val[V, _]) extends DFAny.Val[V, Var] {
    type Width = mold.Width
    val width = mold.width
  }

  case class Const[V <: DFAny](override val mold : DFAny.Val[V, _])(token : V#TToken) extends Constructor[V, false](mold)

  sealed abstract class Initializable[V <: DFAny, Var](override val mold : DFAny.Val[V, _])(externalInit : Seq[V#TToken]) extends Constructor[V, Var](mold)

  sealed abstract class Port[V <: DFAny, Var](override val mold : DFAny.Val[V, _])(dir : Port.Dir, externalInit : Seq[V#TToken]) extends Initializable[V, Var](mold)(externalInit)
  object Port {
    sealed trait Dir
    object Dir {
      case object IN extends Dir
      case object OUT extends Dir
    }
    case class In[V <: DFAny](override val mold : DFAny.Val[V, _])(externalInit : Seq[V#TToken]) extends Port[V, false](mold)(Dir.IN, externalInit)
    case class Out[V <: DFAny](override val mold : DFAny.Val[V, _])(externalInit : Seq[V#TToken]) extends Port[V, true](mold)(Dir.OUT, externalInit)
  }

  case class NewVar[V <: DFAny](override val mold : DFAny.Val[V, _])(externalInit : Seq[V#TToken]) extends Initializable[V, true](mold)(externalInit)

  sealed abstract class Alias[V <: DFAny, Var, RefV <: DFAny, RefVar](override val mold : DFAny.Val[V, _])(val refVal : DFAny.Val[RefV, RefVar]) extends Constructor[V, Var](mold)
  object Alias {
    case class AsIs[V <: DFAny, RefV <: DFAny, RefVar](override val mold : DFAny.Val[V, _])(override val refVal : DFAny.Val[RefV, RefVar]) extends Alias[V, RefVar, RefV, RefVar](mold)(refVal)
    case class BitsWL[W, L, RefV <: DFAny, RefVar](override val refVal : DFAny.Val[RefV, RefVar])(relWidth : TwoFace.Int[W], relBitLow : TwoFace.Int[L]) extends Alias[DFBits[W], RefVar, RefV, RefVar](DFBits(relWidth))(refVal)
    case class Prev[RefV <: DFAny, RefVar](override val refVal : DFAny.Val[RefV, RefVar])(val step : Int) extends Alias[RefV, false, RefV, RefVar](refVal)(refVal)
  }

}



object Test {
  val a = DFUInt(8)
  val aa = a.bits.as(DFUInt(8)).bits
  implicitly[aa.TVar =:= true]
}