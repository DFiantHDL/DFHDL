package DFiant.core
import DFiant.core
import singleton.ops._
import singleton.twoface._

trait DFInit[+Val <: DFAny] {

}

object DFInit {
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
  implicit def fromNone(none : None.type) = DFInitBubble
  implicit def fromPosInt[W, V <: XInt](value : V)(implicit require: RequireMsgSym[V > 0, "Shit", DFInit[DFAny]]) : DFInit[DFBits[W]] = DFInitVal[DFBits[W]](value)


  abstract class DFBitsInit[W](orig: DFBits[W], newInit: DFInit[DFBits[W]]) extends core.DFAny.Alias(orig, orig.width, 0) with DFBits[W] {
    override protected val protInit: DFInit[DFBits[W]] = ???
  }
}

case object DFInitBubble extends DFInit[Nothing]


final case class DFInitVal[+Val <: DFAny](value : BigInt) extends DFInit[Val]

final case class DFInitSeq[+Val <: DFAny](value : Seq[BigInt]) extends DFInit[Val]