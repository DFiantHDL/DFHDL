package ZFiant

import singleton.ops._
import singleton.twoface._
trait DFAnyMember
trait DFAny extends DFAnyMember {self =>
  type TVal <: DFAny
  type TAssignable
  type TToken <: DFAny.Token
  type Width
  val width : TwoFace.Int[Width]
}


object DFAny {
  trait Val[DF <: DFAny, Assignable] extends DFAny {
    type TVal = DF
    type TAssignable = Assignable
    def bits = DFAny.Alias.BitsWL(this)(width, 0)
    def prev = DFAny.Alias.Prev(this)(1)
  }

  trait Token

  sealed abstract class Constructor[DF <: DFAny, Assignable](val mold : DFAny.Val[DF, _]) extends DFAny.Val[DF, Assignable] {
    type Width = mold.Width
    val width = mold.width
  }

  case class Const[DF <: DFAny](override val mold : DFAny.Val[DF, _])(token : DF#TToken) extends Constructor[DF, false](mold)

  sealed abstract class Initializable[DF <: DFAny, Assignable](override val mold : DFAny.Val[DF, _])(externalInit : Seq[DF#TToken]) extends Constructor[DF, Assignable](mold)

  sealed abstract class Port[DF <: DFAny, Assignable](override val mold : DFAny.Val[DF, _])(dir : Port.Dir, externalInit : Seq[DF#TToken]) extends Initializable[DF, Assignable](mold)(externalInit)
  object Port {
    sealed trait Dir
    object Dir {
      case object IN extends Dir
      case object OUT extends Dir
    }
    case class In[DF <: DFAny](override val mold : DFAny.Val[DF, _])(externalInit : Seq[DF#TToken]) extends Port[DF, false](mold)(Dir.IN, externalInit)
    case class Out[DF <: DFAny](override val mold : DFAny.Val[DF, _])(externalInit : Seq[DF#TToken]) extends Port[DF, true](mold)(Dir.OUT, externalInit)
  }

  case class NewVar[DF <: DFAny](override val mold : DFAny.Val[DF, _])(externalInit : Seq[DF#TToken]) extends Initializable[DF, true](mold)(externalInit)

  sealed abstract class Alias[DF <: DFAny, Assignable, Ref <: DFAny, RefAssignable](override val mold : DFAny.Val[DF, _])(val refVal : DFAny.Val[Ref, RefAssignable]) extends Constructor[DF, Assignable](mold)
  object Alias {
    case class BitsWL[W, Ref <: DFAny, RefAssignable](override val refVal : DFAny.Val[Ref, RefAssignable])(relWidth : TwoFace.Int[W], relBitLow : Int) extends Alias[DFBits[W], RefAssignable, Ref, RefAssignable](DFBits(relWidth))(refVal)
    case class Prev[Ref <: DFAny, RefAssignable](override val refVal : DFAny.Val[Ref, RefAssignable])(val step : Int) extends Alias[Ref, false, Ref, RefAssignable](refVal)(refVal)
  }

}

case class DFBits[W] private (width : TwoFace.Int[W]) extends DFAny.Val[DFBits[W], false] {
  type Width = W
}

object DFBits {
  def mold[W](width : TwoFace.Int[W]) = new DFBits(width)
  def apply[W](width : TwoFace.Int[W]) = DFAny.NewVar(new DFBits(width))(Seq())

  case class Token(value : Int) extends DFAny.Token
}


case class DFUInt[W] private (width : TwoFace.Int[W]) extends DFAny.Val[DFUInt[W], false] with DFUInt.Unbounded {
  type Width = W
}

object DFUInt {
  trait Unbounded extends DFAny {
    type TToken = Token
  }
  def apply[W](width : TwoFace.Int[W]) = DFAny.NewVar(new DFUInt(width))(Seq())

  case class Token(value : Int) extends DFAny.Token
}


object Test {
  val a = DFUInt(8)
  val aa = a.bits.bits
  implicitly[aa.TAssignable =:= true]
}