package DFiant
package xilinx

import DFiant._
import compiler.sync._
protected sealed trait DedicatedTags {
  case object AXI_Stream extends DFAny.CustomTag {
    override def toString: String = "AXI_Stream"
  }
}

abstract class VivadoHLSDesign(
  implicit ctx : ContextOf[VivadoHLSDesign]
) extends DFDesign with DedicatedTags {
  final val ap = new DFInterface {
    final val start   = DFBit() <> IN
    final val done    = DFBit() <> OUT
    final val idle    = DFBit() <> OUT
    final val ready   = DFBit() <> OUT
  }
}
object VivadoHLSDesign {
  sealed trait Config extends Product with Serializable
}


class AXI4()(implicit ctx : ContextOf[AXI4]) extends DFInterface("m_axi_") {
  final val AW = new AXI4.WriteAddressChannel()
  final val W  = new AXI4.WriteDataChannel()
  final val AR = new AXI4.ReadAddressChannel()
  final val R  = new AXI4.ReadDataChannel()
  final val B  = new AXI4.WriteResponseChannel()
}
object AXI4 {
  class WriteAddressChannel()(implicit ctx : ContextOf[WriteAddressChannel]) extends DFInterface("", "") {
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
  class WriteDataChannel()(implicit ctx : ContextOf[WriteDataChannel]) extends DFInterface("", "") {
    final val VALID   = DFBit()     <> OUT
    final val READY   = DFBit()     <> IN
    final val DATA    = DFBits(32)  <> OUT
    final val STRB    = DFBits(4)   <> OUT
    final val LAST    = DFBit()     <> OUT
    final val ID      = DFBits(1)   <> OUT
    final val USER    = DFBits(1)   <> OUT
  }
  class WriteResponseChannel()(implicit ctx : ContextOf[WriteResponseChannel]) extends DFInterface("", "") {
    final val VALID   = DFBit()     <> IN
    final val READY   = DFBit()     <> OUT
    final val RESP    = DFBits(2)   <> IN
    final val ID      = DFBits(1)   <> IN
    final val USER    = DFBits(1)   <> IN
  }
  class ReadAddressChannel()(implicit ctx : ContextOf[ReadAddressChannel]) extends DFInterface("", "") {
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
    def notUsed(implicit ctx : DFBlock.Context) : Unit = {
      ADDR := b0s
      BURST := b0s
      CACHE := b0s
      ID := b0s
      LEN := b0s
      LOCK := b0s
      PROT := b0s
      QOS := b0s
      REGION := b0s
      SIZE := b0s
      USER := b0s
      VALID := 0
    }
  }
  class ReadDataChannel()(implicit ctx : ContextOf[ReadDataChannel]) extends DFInterface("", "") {
    final val VALID   = DFBit()     <> IN
    final val READY   = DFBit()     <> OUT
    final val DATA    = DFBits(32)  <> IN
    final val LAST    = DFBit()     <> IN
    final val ID      = DFBits(1)   <> IN
    final val USER    = DFBits(1)   <> IN
    final val RESP    = DFBits(2)   <> IN
  }
}