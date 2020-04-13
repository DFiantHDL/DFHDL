package DFiant
package xilinx

import DFiant._
import compiler.sync._
protected sealed trait DedicatedTags {
  case object AXI_Stream extends DFAny.CustomTag {
    override def toString: String = "AXI_Stream"
  }
}

//trait AP extends DFInterface {
//  final val start   = DFBit() <> IN
//  final val done    = DFBit() <> OUT
//  final val idle    = DFBit() <> OUT
//  final val ready   = DFBit() <> OUT
//}

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


final class AXI4 private (axiDir : AXI4.Dir)(config : AXI4.Config)(implicit ctx : ContextOf[AXI4]) extends DFInterface("m_axi_") {
  final val AW = new AXI4.AddressChannel(axiDir)(config.wrEnabled, config.simple)
  final val W  = new AXI4.WriteDataChannel(axiDir)(config.wrEnabled, config.simple)
  final val AR = new AXI4.AddressChannel(axiDir)(config.rdEnabled, config.simple)
  final val R  = new AXI4.ReadDataChannel(axiDir)(config.rdEnabled, config.simple)
  final val B  = new AXI4.WriteResponseChannel(axiDir)(config.wrEnabled, config.simple)
  def <> (that : AXI4)(implicit ctx : DFBlock.Context) : Unit = {
    AW  <> that.AW
    W   <> that.W
    AR  <> that.AR
    R   <> that.R
    B   <> that.B
  }
}
object AXI4 {
  protected final class ConfigNode(config : Config) {
    def <> (axiDir : Dir)(implicit ctx : ContextOf[AXI4]) : AXI4 = new AXI4(axiDir)(config)
  }
  def apply(config: Config) : ConfigNode = new ConfigNode(config)
  final val SRO = AXI4(Config(rdEnabled = true, wrEnabled = false, simple = true)) //simple read-only
  final val SWO = AXI4(Config(rdEnabled = false, wrEnabled = true, simple = true)) //simple write-only

  abstract class Interface(axiDir : Dir)(implicit ctx : ContextOf[Interface]) extends DFInterface("", "") {
    def MasterDir(portDir : DFDir) : DFDir = axiDir match {
      case Master => portDir
      case Slave => portDir match {
        case IN => OUT
        case OUT => IN
      }
    }
  }

  sealed trait Dir extends Product with Serializable
  case object Master extends Dir
  case object Slave extends Dir

  final case class Config(
    rdEnabled : Boolean,
    wrEnabled : Boolean,
    simple : Boolean
  ) extends Product with Serializable

  final protected class AddressChannel(axiDir : Dir)(enabled : Boolean, simple : Boolean)(implicit ctx : ContextOf[AddressChannel]) extends Interface(axiDir) {
    final val VALID   = DFBit()     <> MasterDir(OUT)
    final val READY   = DFBit()     <> MasterDir(IN)
    final val ADDR    = DFBits(32)  <> MasterDir(OUT)
    final val ID      = DFBits(1)   <> MasterDir(OUT)
    final val LEN     = DFBits(32)  <> MasterDir(OUT)
    final val SIZE    = DFBits(3)   <> MasterDir(OUT)
    final val BURST   = DFBits(2)   <> MasterDir(OUT)
    final val LOCK    = DFBits(2)   <> MasterDir(OUT)
    final val CACHE   = DFBits(4)   <> MasterDir(OUT)
    final val PROT    = DFBits(3)   <> MasterDir(OUT)
    final val QOS     = DFBits(4)   <> MasterDir(OUT)
    final val REGION  = DFBits(4)   <> MasterDir(OUT)
    final val USER    = DFBits(1)   <> MasterDir(OUT)
    axiDir match {
      case Master =>
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
      case Slave =>
        if (!enabled) {
          READY := 0
        }
    }
    def <> (that : AddressChannel)(implicit ctx : DFBlock.Context) : Unit = {
      VALID    <> that.VALID
      READY    <> that.READY
      ADDR     <> that.ADDR
      ID       <> that.ID
      LEN      <> that.LEN
      SIZE     <> that.SIZE
      BURST    <> that.BURST
      LOCK     <> that.LOCK
      CACHE    <> that.CACHE
      PROT     <> that.PROT
      QOS      <> that.QOS
      REGION   <> that.REGION
      USER     <> that.USER
    }
  }
  final protected class WriteDataChannel(axiDir : Dir)(enabled : Boolean, simple : Boolean)(implicit ctx : ContextOf[WriteDataChannel]) extends Interface(axiDir) {
    final val VALID   = DFBit()     <> MasterDir(OUT)
    final val READY   = DFBit()     <> MasterDir(IN)
    final val DATA    = DFBits(32)  <> MasterDir(OUT)
    final val STRB    = DFBits(4)   <> MasterDir(OUT)
    final val LAST    = DFBit()     <> MasterDir(OUT)
    final val ID      = DFBits(1)   <> MasterDir(OUT)
    final val USER    = DFBits(1)   <> MasterDir(OUT)
    axiDir match {
      case Master =>
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
      case Slave =>
        if (!enabled) {
          READY := 0
        }
    }
    def <> (that : WriteDataChannel)(implicit ctx : DFBlock.Context) : Unit = {
      VALID  <> that.VALID
      READY  <> that.READY
      DATA   <> that.DATA
      STRB   <> that.STRB
      LAST   <> that.LAST
      ID     <> that.ID
      USER   <> that.USER
    }
  }
  final protected class WriteResponseChannel(axiDir : Dir)(enabled : Boolean, simple : Boolean)(implicit ctx : ContextOf[WriteResponseChannel]) extends Interface(axiDir) {
    final val VALID   = DFBit()     <> MasterDir(IN)
    final val READY   = DFBit()     <> MasterDir(OUT)
    final val RESP    = DFBits(2)   <> MasterDir(IN)
    final val ID      = DFBits(1)   <> MasterDir(IN)
    final val USER    = DFBits(1)   <> MasterDir(IN)
    axiDir match {
      case Master =>
        if (!enabled || simple) {
          READY := 0
        }
      case Slave =>
        if (!enabled || simple) {
          VALID := 0
          RESP := b0s
          ID := b0s
          USER := b0s
        }
    }
    def <> (that : WriteResponseChannel)(implicit ctx : DFBlock.Context) : Unit = {
      VALID    <> that.VALID
      READY    <> that.READY
      RESP     <> that.RESP
      ID       <> that.ID
      USER     <> that.USER
    }
  }
  final protected class ReadDataChannel(axiDir : Dir)(enabled : Boolean, simple : Boolean)(implicit ctx : ContextOf[ReadDataChannel]) extends Interface(axiDir) {
    final val VALID   = DFBit()     <> MasterDir(IN)
    final val READY   = DFBit()     <> MasterDir(OUT)
    final val DATA    = DFBits(32)  <> MasterDir(IN)
    final val LAST    = DFBit()     <> MasterDir(IN)
    final val ID      = DFBits(1)   <> MasterDir(IN)
    final val USER    = DFBits(1)   <> MasterDir(IN)
    final val RESP    = DFBits(2)   <> MasterDir(IN)
    axiDir match {
      case Master =>
        if (!enabled) {
          READY := 0
        }
      case Slave =>
        if (!enabled) {
          VALID := 0
          DATA := b0s
          LAST := 0
          ID := b0s
          USER := b0s
          RESP := b0s
        } else if (simple) {
          LAST := 0
          ID := b0s
          USER := b0s
          RESP := b0s
        }
    }
    def <> (that : ReadDataChannel)(implicit ctx : DFBlock.Context) : Unit = {
      VALID    <> that.VALID
      READY    <> that.READY
      DATA     <> that.DATA
      LAST     <> that.LAST
      ID       <> that.ID
      USER     <> that.USER
      RESP     <> that.RESP
    }
  }
}