package DFiant
package xilinx

import DFiant._
import compiler.sync._
protected sealed trait DedicatedTags {
  case object AXI_Stream extends DFAny.CustomTag {
    override def toString: String = "AXI_Stream"
  }
}

abstract class VivadoHLSDesign(config : VivadoHLSDesign.Config)(
  implicit ctx : ContextOf[VivadoHLSDesign]
) extends DFDesign with DedicatedTags {
  val ap = new DFInterface.Pure {
    final val start = DFBit() <> IN
    final val done  = DFBit() <> OUT
    final val idle  = DFBit() <> OUT
    final val ready = DFBit() <> OUT
  }

  this !! ClockParams("ap_clk", ClockParams.Edge.Rising)
  this !! ResetParams("ap_rst", ResetParams.Mode.Sync, ResetParams.Active.High)
}
object VivadoHLSDesign {
  sealed trait Config extends Product with Serializable
}

abstract class AXI(name : String)(implicit ctx : ContextOf[AXI]) extends MetaDesign() {
  val AWVALID = DFBit() <> OUT
  val AWREADY = DFBit() <> IN
  val AWADDR = DFBits(32) <> OUT
  val AWID = DFBits(1) <> OUT
  val AWLEN = DFBits(32) <> OUT
  val AWSIZE = DFBits(3) <> OUT
  val AWBURST = DFBits(2) <> OUT
  val AWLOCK = DFBits(2) <> OUT
  val AWCACHE = DFBits(4) <> OUT
  val AWPROT = DFBits(3) <> OUT
  val AWQOS = DFBits(4) <> OUT
  val AWREGION = DFBits(4) <> OUT
  val AWUSER = DFBits(1) <> OUT
  val WVALID = DFBit() <> OUT
  val WREADY = DFBit() <> IN
  val WDATA = DFBits(32) <> OUT
  val WSTRB = DFBits(4) <> OUT
  val WLAST = DFBit() <> OUT
  val WID = DFBits(1) <> OUT
  val WUSER = DFBits(1) <> OUT
  val ARVALID = DFBit() <> OUT
  val ARREADY = DFBit() <> IN
  val ARADDR = DFBits(32) <> OUT
  val ARID = DFBits(1) <> OUT
  val ARLEN = DFBits(32) <> OUT
  val ARSIZE = DFBits(3) <> OUT
  val ARBURST = DFBits(2) <> OUT
  val ARLOCK = DFBits(2) <> OUT
  val ARCACHE = DFBits(4) <> OUT
  val ARPROT = DFBits(3) <> OUT
  val ARQOS = DFBits(4) <> OUT
  val ARREGION = DFBits(4) <> OUT
  val ARUSER = DFBits(1) <> OUT
  val RVALID = DFBit() <> IN
  val RREADY = DFBit() <> OUT
  val RDATA = DFBits(32) <> IN
  val RLAST = DFBit() <> IN
  val RID = DFBits(1) <> IN
  val RUSER = DFBits(1) <> IN
  val RRESP = DFBits(2) <> IN
  val BVALID = DFBit() <> IN
  val BREADY = DFBit() <> OUT
  val BRESP = DFBits(2) <> IN
  val BID = DFBits(1) <> IN
  val BUSER = DFBits(1) <> IN

  __db.getMembers.foreach(m => m.setName(s"${name}_${m.name}"))

}