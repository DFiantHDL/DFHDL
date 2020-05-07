package DFiant
package lib.ompss
import lib.bus.AXI4

sealed trait OmpssDir
case object RO  extends OmpssDir
case object WO extends OmpssDir

@df final class OmpssAXI(dir : OmpssDir, flipped : Boolean) extends DFInterface(OmpssAXI.PrefixNameFlatten) {
  private val axiDir = if (flipped) AXI4.Slave else AXI4.Master
  private val axiNode = dir match {
    case RO => AXI4.SRO
    case WO => AXI4.SWO
  }
  final val axi = axiNode <> axiDir setNameFlatten(DFInterface.NameFlatten.IgnoreOwnerName)
  final val offset  = DFBits(64) <> (if (flipped) OUT else IN)
  def <> (flip : FLIP.type) : OmpssAXI = {
    val updated = new OmpssAXI(dir, !flipped)
    this.replaceOwnerAndMembers(this.owner, updated.owner)
    updated
  }
}

object OmpssAXI {
  protected object PrefixNameFlatten extends DFInterface.NameFlatten {
    def apply(memberName : String, ownerName : String) : String =
      if (memberName == "offset") s"${ownerName}_$memberName" //the offset is special cases because there is no prefix here
      else s"m_axi_${ownerName}_$memberName" //Vivado adds "m_axi_" to the AXI interface signals
  }
  def <> (dir : OmpssDir)(implicit ctx : ContextOf[OmpssAXI]) : OmpssAXI = new OmpssAXI(dir, false)
  implicit def axiRef(ompssVal: OmpssAXI) : AXI4 = ompssVal.axi
}
