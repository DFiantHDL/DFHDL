package DFiant
package lib.ompss
import lib.bus.AXI4

case object INOUT

@df final class OmpssAXI(dir : Either[PortDir, INOUT.type]) extends DFInterface(OmpssAXI.PrefixNameFlatten) {
  lazy val flipped : Boolean = __ctx.dir match {
    case dir: PortDir => false
    case DFiant.VAR => false
    case DFiant.FLIP => true
    case DFiant.ASIS => false
  }
  println(flipped)
  private val axiDir = if (flipped) AXI4.Slave else AXI4.Master
  private val axiNode = dir match {
    case Left(IN) => AXI4.SRO
    case Left(OUT) => AXI4.SWO
    case Right(_) => AXI4.SRW
  }
  final val axi = axiNode <> axiDir //setNameFlatten(DFOwner.NameFlatten.IgnoreOwnerName)
  final val offset  = DFBits(64) <> (if (flipped) OUT else IN)
//  def <> (flip : FLIP.type) : OmpssAXI = {
//    val updated = new OmpssAXI(dir, !flipped)
//    this.replaceOwnerAndMembers(this.owner, updated.owner)
//    updated
//  }
}

object OmpssAXI {
  protected object PrefixNameFlatten extends DFOwner.NameFlatten {
    def apply(memberName : String, ownerName : String) : String =
      if (memberName == "offset") s"${ownerName}_$memberName" //the offset is special cases because there is no prefix here
      else s"m_axi_${ownerName}_$memberName" //Vivado adds "m_axi_" to the AXI interface signals
  }
  def <> (dir : PortDir)(implicit ctx : ContextOf[OmpssAXI]) : OmpssAXI = new OmpssAXI(Left(dir))
  def <> (dir : INOUT.type)(implicit ctx : ContextOf[OmpssAXI]) : OmpssAXI = new OmpssAXI(Right(dir))
  implicit def axiRef(ompssVal: OmpssAXI) : AXI4 = ompssVal.axi
}
