package DFiant.lib.bus

import DFiant._
import DFDesign.Frontend._
import DFiant.DFOwner.NameFlatten
import DFiant.lib.stream._

/**
  * AXI-4 bus interface.
  * @param axiDir The directionality (Master/Slave)
  * @param config The configuration (simple RO/WO/RW)
  */
@df final class AXI4 private (axiDir : AXI4.Dir)(config : AXI4.Config) extends DFInterface {
  private val streamDir : StreamDir = axiDir match {
    case AXI4.Master => SOURCE
    case AXI4.Slave => SINK
  }
  final val AW = new AXI4.AddressChannel(streamDir)
  final val W  = new AXI4.WriteDataChannel(streamDir)
  final val AR = new AXI4.AddressChannel(streamDir)
  final val R  = new AXI4.ReadDataChannel(streamDir.flip)
  final val B  = new AXI4.WriteResponseChannel(streamDir.flip)
}
object AXI4 {
  protected final class ConfigNode(config : Config) {
    /**
      * Constructs an AXI-4 interface according the directionality chosen [[Master]]/[[Slave]]
      * @param axiDir
      * @return an AXI-4 interface with configuration `config` and directionality `axiDir`
      */
    def <> (axiDir : Dir)(implicit ctx : ContextOf[AXI4]) : AXI4 = new AXI4(axiDir)(config)
  }

  /**
    * Creates an configuration node that is used to construct an AXI-4 interface.
    * @param config The chosen AXI-4 usage configuration [[SRO]]/[[SWO]]/[[SRW]].
    * @return a configuration node that can constructs an AXI-4 interface with [[ConfigNode.<>]]
    */
  def apply(config: Config) : ConfigNode = new ConfigNode(config)

  /**
    * Simple Read-Only
    */
  final val SRO = AXI4(Config(rdEnabled = true, wrEnabled = false, simple = true))
  /**
    * Simple Write-Only
    */
  final val SWO = AXI4(Config(rdEnabled = false, wrEnabled = true, simple = true))
  /**
    * Simple Read-Write
    */
  final val SRW = AXI4(Config(rdEnabled = true, wrEnabled = true, simple = true))

  trait Fire {
    val valid : DFBool <> VAR
    val ready : DFBool <> VAR
    @df final def fireFSM : FSM = FSM {
      val ctrl = DFBit <> VAR init 0
      ctrl := 1
      ifdf(!ctrl.prev) {
        ctrl := ready
        valid := 1
      }
      ifdf(ctrl) {
        ctrl := 0
        nextStep.goto()
      }.elseifdf(ready) {
        ctrl := 1
      }
    }
  }

  object NoSuffixCapitalize extends NameFlatten {
    def apply(memberName : String, ownerName : String) : String = s"${ownerName}${memberName.toUpperCase}"
  }

  @df abstract class Interface(streamDir : StreamDir) extends RVStream(streamDir, NoSuffixCapitalize)

  sealed trait Dir extends Product with Serializable
  case object Master extends Dir
  case object Slave extends Dir

  final case class Config(
    rdEnabled : Boolean,
    wrEnabled : Boolean,
    simple : Boolean
  ) extends Product with Serializable

  @df final protected class AddressChannel(streamDir : StreamDir) extends Interface(streamDir) with Fire {
    final val addr    = DFBits(32)  <> DEFAULT_DIR init b0s
    final val id      = DFBits(1)   <> DEFAULT_DIR init b0s
    final val len     = DFBits(32)  <> DEFAULT_DIR init b0s
    final val size    = DFBits(3)   <> DEFAULT_DIR init b0s
    final val burst   = DFBits(2)   <> DEFAULT_DIR init b0s
    final val lock    = DFBits(2)   <> DEFAULT_DIR init b0s
    final val cache   = DFBits(4)   <> DEFAULT_DIR init b0s
    final val prot    = DFBits(3)   <> DEFAULT_DIR init b0s
    final val qos     = DFBits(4)   <> DEFAULT_DIR init b0s
    final val region  = DFBits(4)   <> DEFAULT_DIR init b0s
    final val user    = DFBits(1)   <> DEFAULT_DIR init b0s
    streamDir match {
      case SOURCE =>
        if (hasNativeDir) {
          valid := 0
        }
      case _ =>
    }
  }
  @df final protected class WriteDataChannel(streamDir : StreamDir) extends Interface(streamDir) with Fire {
    final val data    = DFBits(32)  <> DEFAULT_DIR init b0s
    final val strb    = DFBits(4)   <> DEFAULT_DIR init b0s
    final val last    = DFBit       <> DEFAULT_DIR init 0
    final val id      = DFBits(1)   <> DEFAULT_DIR init b0s
    final val user    = DFBits(1)   <> DEFAULT_DIR init b0s
    streamDir match {
      case SOURCE =>
        if (hasNativeDir) {
          valid := 0
        }
      case _ =>
    }
  }
  @df final protected class WriteResponseChannel(streamDir : StreamDir) extends Interface(streamDir) {
    final val resp    = DFBits(2)   <> DEFAULT_DIR init b0s
    final val id      = DFBits(1)   <> DEFAULT_DIR init b0s
    final val user    = DFBits(1)   <> DEFAULT_DIR init b0s
    streamDir match {
      case SINK =>
        if (hasNativeDir) {
          ready := 0
        }
      case _ =>
    }
  }
  @df final protected class ReadDataChannel(streamDir : StreamDir) extends Interface(streamDir) {
    final val data    = DFBits(32)  <> DEFAULT_DIR init b0s
    final val last    = DFBit       <> DEFAULT_DIR init 0
    final val id      = DFBits(1)   <> DEFAULT_DIR init b0s
    final val user    = DFBits(1)   <> DEFAULT_DIR init b0s
    final val resp    = DFBits(2)   <> DEFAULT_DIR init b0s
    streamDir match {
      case SINK =>
        if (hasNativeDir) {
          ready := 0
        }
      case _ =>
    }
  }
}
