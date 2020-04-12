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
  //need to replace DF-AXI inputs => AXI4(AXI4.Config(false, true)) + an input <name>_offset(64)
  //need to replace DF-AXI outputs => AXI4(AXI4.Config(true, false)) + an input <name>_offset(64)
  //need to have size 32-bit input
  //inputs may be should be cast to/from DFBits.
}
object VivadoHLSDesign {
  sealed trait Config extends Product with Serializable
}


class AXI4(config : AXI4.Config)(implicit ctx : ContextOf[AXI4]) extends DFInterface("m_axi_") {
  final val AW = new AXI4.AddressChannel(config.wrEnabled, config.simple)
  final val W  = new AXI4.WriteDataChannel(config.wrEnabled, config.simple)
  final val AR = new AXI4.AddressChannel(config.rdEnabled, config.simple)
  final val R  = new AXI4.ReadDataChannel(config.rdEnabled, config.simple)
  final val B  = new AXI4.WriteResponseChannel(config.wrEnabled, config.simple)
}
object AXI4 {
  final case class Config(rdEnabled : Boolean, wrEnabled : Boolean, simple : Boolean) extends Product with Serializable
  class AddressChannel(enabled : Boolean, simple : Boolean)(implicit ctx : ContextOf[AddressChannel]) extends DFInterface("", "") {
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
    if (!enabled) {
      VALID := 0
      ADDR := b0s
      ID := b0s
      LEN := b0s
      SIZE := b0s
      BURST := b0s
      LOCK := b0s
      CACHE := b0s
      PROT := b0s
      QOS := b0s
      REGION := b0s
      USER := b0s
    } else if (simple) {
      ID := b0s
      SIZE := b0s
      BURST := b0s
      LOCK := b0s
      CACHE := b0s
      PROT := b0s
      QOS := b0s
      REGION := b0s
      USER := b0s
    }
  }
  class WriteDataChannel(enabled : Boolean, simple : Boolean)(implicit ctx : ContextOf[WriteDataChannel]) extends DFInterface("", "") {
    final val VALID   = DFBit()     <> OUT
    final val READY   = DFBit()     <> IN
    final val DATA    = DFBits(32)  <> OUT
    final val STRB    = DFBits(4)   <> OUT
    final val LAST    = DFBit()     <> OUT
    final val ID      = DFBits(1)   <> OUT
    final val USER    = DFBits(1)   <> OUT
    if (!enabled) {
      VALID := 0
      DATA := b0s
      STRB := b0s
      LAST := 0
      ID := b0s
      USER := b0s
    } else if (simple) {
      STRB := b1s
      LAST := 0
      ID := b0s
      USER := b0s
    }
  }
  class WriteResponseChannel(enabled : Boolean, simple : Boolean)(implicit ctx : ContextOf[WriteResponseChannel]) extends DFInterface("", "") {
    final val VALID   = DFBit()     <> IN
    final val READY   = DFBit()     <> OUT
    final val RESP    = DFBits(2)   <> IN
    final val ID      = DFBits(1)   <> IN
    final val USER    = DFBits(1)   <> IN
    if (!enabled || simple) {
      READY := 0
    }
  }
  class ReadDataChannel(enabled : Boolean, simple : Boolean)(implicit ctx : ContextOf[ReadDataChannel]) extends DFInterface("", "") {
    final val VALID   = DFBit()     <> IN
    final val READY   = DFBit()     <> OUT
    final val DATA    = DFBits(32)  <> IN
    final val LAST    = DFBit()     <> IN
    final val ID      = DFBits(1)   <> IN
    final val USER    = DFBits(1)   <> IN
    final val RESP    = DFBits(2)   <> IN
    if (!enabled) {
      READY := 0
    }
  }
}