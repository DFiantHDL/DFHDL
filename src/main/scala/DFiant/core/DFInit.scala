package DFiant.core
import DFiant.core
import DFiant.internals._
import singleton.ops._
import singleton.twoface._


trait DFInit {
//  val almanacInit : AlmanacInit = ???
}

trait DFInitOf[+Val <: DFAny] extends DFInit {
//  def bitsInit[W](relWidth : Int, relBitLow : Int) : DFInit[DFBits[W]]
}

object DFInitOf {
  case object Bubble extends DFInitOf[Nothing] {
    //  def relInit(relWidth : Int, relBitLow : Int) : DFInit = DFInitBubble
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Implicit configuration of when operation is possible
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  trait Able[L <: DFAny, R] {
    val right : R
  }

  object Able {
    implicit class DFBitsInt[LW](val right : Int) extends Able[DFBits[LW], Int]
    implicit class DFBitsXInt[LW, R <: XInt](val right : R) extends Able[DFBits[LW], R]
  }
  /////////////////////////////////////////////////////////////////////////////////////////////////////////////
  implicit def fromNone(none : None.type) = Bubble
//  implicit def fromPosInt[W, V <: XInt](value : V)(implicit require: RequireMsgSym[V > 0, "Shit", DFInit[DFAny]]) : DFInit[DFBits[W]] = DFInitVal[DFBits[W]](value)


  trait Builder[L <: DFAny, R] {
    def apply(left : L, right : Able[L, R]) : DFInitOf[L]
  }
//  abstract class DFBitsInit[W](orig: DFBits[W], newInit: DFInit[DFBits[W]]) extends core.DFAny.Alias(orig, orig.width, 0) with DFBits[W] {
//    override protected val protInit: DFInit[DFBits[W]] = ???
//  }
}



//final case class DFInitVal(value : BigInt) extends DFInit
//
//final case class DFInitSeq(value : Seq[BigInt]) extends DFInit