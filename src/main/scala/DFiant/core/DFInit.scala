package DFiant.core
import singleton.ops._
import singleton.twoface._

trait DFInit[+Val <: DFAny] {

}

object DFInit {
  implicit def fromNone(none : None.type) = DFInitBubble
  implicit def fromPosInt[W, V <: XInt](value : V)(implicit require: RequireMsgSym[V > 0, "Shit", DFInit[DFAny]]) : DFInit[DFBits[W]] = DFInitVal[DFBits[W]](value)
}

case object DFInitBubble extends DFInit[Nothing]


final case class DFInitVal[+Val <: DFAny](value : BigInt) extends DFInit[Val]

final case class DFInitSeq[+Val <: DFAny](value : Seq[BigInt]) extends DFInit[Val]