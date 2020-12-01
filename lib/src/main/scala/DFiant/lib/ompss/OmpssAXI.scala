package DFiant
package lib.ompss
import lib.bus.AXI4

case object INOUT

/**
  * Ompss AXI Interface
  *
  * Construct using `OmpssAXI <>`
  */
@df final class OmpssAXI private (dir : Either[PortDir, INOUT.type], flipped : Boolean) extends DFInterface(OmpssAXI.PrefixNameFlatten) {
  private val axiDir = if (flipped) AXI4.Slave else AXI4.Master
  private val axiNode = dir match {
    case Left(IN) => AXI4.SRO
    case Left(OUT) => AXI4.SWO
    case Right(_) => AXI4.SRW
  }
  final val axi = axiNode <> axiDir setNameFlatten(DFOwner.NameFlatten.IgnoreOwnerName)
  final val offset  = DFBits(64) <> (if (flipped) OUT else IN)
}

object OmpssAXI {
  protected object PrefixNameFlatten extends DFOwner.NameFlatten {
    def apply(memberName : String, ownerName : String) : String =
      if (memberName == "offset") s"${ownerName}_$memberName" //the offset is special cases because there is no prefix here
      else s"m_axi_${ownerName}_$memberName" //Vivado adds "m_axi_" to the AXI interface signals
  }

  /**
    * Construct an input-only or output-only OmpssAXI interface
    * @param dir [[IN]] or [[OUT]]
    * @return the constructed interface
    */
  def <> (dir : PortDir)(implicit ctx : ContextOf[OmpssAXI]) : OmpssAXI = {
    val flipped = ctx.dir match {
      case DFiant.FLIP => true
      case _ => false
    }
    new OmpssAXI(Left(dir), flipped)(ctx.updateDir(ASIS))
  }
  /**
    * Construct an INOUT OmpssAXI interface
    * @param dir Must be INOUT
    * @return the constructed interface
    */
  def <> (dir : INOUT.type)(implicit ctx : ContextOf[OmpssAXI]) : OmpssAXI = new OmpssAXI(Right(dir), false)
  implicit def axiRef(ompssVal: OmpssAXI) : AXI4 = ompssVal.axi
}
