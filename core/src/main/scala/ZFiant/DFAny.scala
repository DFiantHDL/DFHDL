package ZFiant

import singleton.ops._
import singleton.twoface._
trait DFAnyMember
trait DFAny extends DFAnyMember {self =>
  type TVal <: DFAny
  type TAssignable <: Boolean
  type TToken <: DFAny.Token
  type Width
  val width : TwoFace.Int[Width]
  val left = this.asInstanceOf[DFAny.Val[TVal]{type TAssignable = self.TAssignable}]
  def bits = DFAny.Alias.BitsWL(left)(width, 0)
  def prev = DFAny.Alias.Prev(left)(1)
}


object DFAny {
  trait Val[DF <: DFAny] extends DFAny {self : DFAny.Val[DF] =>
    type TVal = DF
  }

  trait Token

  sealed abstract class Constructor[DF <: DFAny](val mold : DF) extends DFAny {
    type Width = mold.Width
    val width = mold.width
  }

  case class Const[DF <: DFAny](override val mold : DF)(token : Token) extends Constructor(mold) {
    type TAssignable = false
  }

  sealed abstract class Initializable[DF <: DFAny](override val mold : DF)(externalInit : Seq[DF#TToken]) extends Constructor(mold)

  sealed abstract class Port[DF <: DFAny](override val mold : DF)(dir : Port.Dir, externalInit : Seq[DF#TToken]) extends Initializable[DF](mold)(externalInit)
  object Port {
    sealed trait Dir
    object Dir {
      case object IN extends Dir
      case object OUT extends Dir
    }
    case class In[DF <: DFAny](override val mold : DF)(externalInit : Seq[DF#TToken]) extends Port(mold)(Dir.IN, externalInit) {
      type TAssignable = false
    }
    case class Out[DF <: DFAny](override val mold : DF)(externalInit : Seq[DF#TToken]) extends Port(mold)(Dir.OUT, externalInit) {
      type TAssignable = true
    }
  }


  case class NewVar[DF <: DFAny](override val mold : DF)(externalInit : Seq[DF#TToken]) extends Initializable(mold)(externalInit) {
    type TAssignable = true
  }

  sealed abstract class Alias[DF <: DFAny, Ref <: DFAny](override val mold : DF)(val refVal : Ref) extends Constructor(mold)
  object Alias {
    case class BitsWL[W, Ref <: DFAny](override val refVal : Ref)(relWidth : TwoFace.Int[W], relBitLow : Int) extends Alias(DFBits(relWidth))(refVal) {
      override type TAssignable = refVal.TAssignable
    }
    case class Prev[DF <: DFAny, Ref <: DFAny](override val refVal : Ref)(val step : Int) extends Alias(refVal)(refVal) {
      override type TAssignable = false
    }
  }

}

case class DFBits[W] private (width : TwoFace.Int[W]) extends DFAny.Val[DFBits[W]] {
  type Width = W
}

object DFBits {
  def mold[W](width : TwoFace.Int[W]) = new DFBits(width)
  def apply[W](width : TwoFace.Int[W]) = DFAny.NewVar(new DFBits(width))(Seq())

  case class Token(value : Int) extends DFAny.Token
}


case class DFUInt[W] private (width : TwoFace.Int[W]) extends DFAny.Val[DFUInt[W]] with DFUInt.Unbounded {
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
  val aa = a.bits
  implicitly[aa.TAssignable =:= true]
}