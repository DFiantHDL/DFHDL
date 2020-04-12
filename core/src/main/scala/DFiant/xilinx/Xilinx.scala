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


class AXI4()(implicit ctx : ContextOf[AXI4]) extends DFInterface.Pure {
  final val AW = new AXI4.WriteAddressChannel()
  final val W  = new AXI4.WriteDataChannel()
  final val AR = new AXI4.WriteResponseChannel()
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
}
object AXI4 {
  class WriteAddressChannel()(implicit ctx : ContextOf[WriteAddressChannel]) extends DFInterface.Pure("", "") {
    final val VALID   = DFBit()     <> OUT
    final val READY   = DFBit()     <> IN
    final val ADDR    = DFBits(32)  <> OUT
    final val ID      = DFBits(1)   <> OUT
    final val LEN     = DFBits(32)  <> OUT
    final val SIZE    = DFBits(3)   <> OUT
    final val BURST   = DFBits(2)   <> OUT
    final val LOCK    = DFBits(2)   <> OUT
    final val CACHE   = DFBits(4)   <> OUT
    final val PROT    = DFBits(3)   <> OUT
    final val QOS     = DFBits(4)   <> OUT
    final val REGION  = DFBits(4)   <> OUT
    final val USER    = DFBits(1)   <> OUT
  }
  class WriteDataChannel()(implicit ctx : ContextOf[WriteDataChannel]) extends DFInterface.Pure("", "") {
    final val VALID   = DFBit()     <> OUT
    final val READY   = DFBit()     <> IN
    final val DATA    = DFBits(32)  <> OUT
    final val STRB    = DFBits(4)   <> OUT
    final val LAST    = DFBit()     <> OUT
    final val ID      = DFBits(1)   <> OUT
    final val USER    = DFBits(1)   <> OUT
  }
  class WriteResponseChannel()(implicit ctx : ContextOf[WriteResponseChannel]) extends DFInterface.Pure("", "") {
    final val VALID   = DFBit()     <> OUT
    final val READY   = DFBit()     <> IN
    final val ADDR    = DFBits(32)  <> OUT
    final val ID      = DFBits(1)   <> OUT
    final val LEN     = DFBits(32)  <> OUT
    final val SIZE    = DFBits(3)   <> OUT
    final val BURST   = DFBits(2)   <> OUT
    final val LOCK    = DFBits(2)   <> OUT
    final val CACHE   = DFBits(4)   <> OUT
    final val PROT    = DFBits(3)   <> OUT
    final val QOS     = DFBits(4)   <> OUT
    final val REGION  = DFBits(4)   <> OUT
    final val USER    = DFBits(1)   <> OUT
  }
}