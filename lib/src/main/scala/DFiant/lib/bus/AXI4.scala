package DFiant.lib.bus

import DFiant._
import DFDesign.Implicits._
import DFiant.DFOwner.NameFlatten
import DFiant.lib.stream._

@df final class AXI4 (axiDir : AXI4.Dir)(config : AXI4.Config) extends DFInterface {
  private val streamDir : StreamDir = axiDir match {
    case AXI4.Master => SOURCE
    case AXI4.Slave => SINK
  }
  final val AW = new AXI4.AddressChannel(streamDir)(config.wrEnabled, config.simple)
  final val W  = new AXI4.WriteDataChannel(streamDir)(config.wrEnabled, config.simple)
  final val AR = new AXI4.AddressChannel(streamDir)(config.rdEnabled, config.simple)
  final val R  = new AXI4.ReadDataChannel(streamDir.flip)(config.rdEnabled, config.simple)
  final val B  = new AXI4.WriteResponseChannel(streamDir.flip)(config.wrEnabled, config.simple)
  def readRequest(address : DFBits[32], size : DFUInt[32])(implicit ctx : DFBlock.Context) : Unit = {}
  def writeRequest(address : DFBits[32], size : DFUInt[32])(implicit ctx : DFBlock.Context) : Unit = {}
}
object AXI4 {
  protected final class ConfigNode(config : Config) {
    def <> (axiDir : Dir)(implicit ctx : ContextOf[AXI4]) : AXI4 = new AXI4(axiDir)(config)
  }
  def apply(config: Config) : ConfigNode = new ConfigNode(config)
  final val SRO = AXI4(Config(rdEnabled = true, wrEnabled = false, simple = true)) //simple read-only
  final val SWO = AXI4(Config(rdEnabled = false, wrEnabled = true, simple = true)) //simple write-only
  final val SRW = AXI4(Config(rdEnabled = true, wrEnabled = true, simple = true)) //simple read-write

  trait Fire {
    val valid : DFBool <> VAR
    val ready : DFBool <> VAR
    import fsm._
    final def fireFSM(onExit : => Unit)(implicit ctx : DFBlock.Context) = {
      stepR {
        val ctrl = DFBit() init 0
        ctrl := 1
        ifdf(!ctrl.prev) {
          ctrl := ready
          valid := 1
        }
        ctrl
      } =?> {ctrl => ctrl} =^> {ctrl => ctrl := 0; onExit} =!> nextStep =?> ready =^> {ctrl => ctrl := 1} =!> thisStep
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

  @df final protected class AddressChannel(streamDir : StreamDir)(enabled : Boolean, simple : Boolean) extends Interface(streamDir) with Fire{
    final val addr    = DFBits(32)
    final val id      = DFBits(1)
    final val len     = DFBits(32)
    final val size    = DFBits(3)
    final val burst   = DFBits(2)
    final val lock    = DFBits(2)
    final val cache   = DFBits(4)
    final val prot    = DFBits(3)
    final val qos     = DFBits(4)
    final val region  = DFBits(4)
    final val user    = DFBits(1)
    streamDir match {
      case SOURCE =>
        if (!enabled) {
          valid := 0
          addr := b0s
          id := b0s
          len := b0s
          size := b0s
          burst := b0s
          lock := b0s
          cache := b0s
          prot := b0s
          qos := b0s
          region := b0s
          user := b0s
        } else if (simple) {
          id := b0s
          size := b0s
          burst := b0s
          lock := b0s
          cache := b0s
          prot := b0s
          qos := b0s
          region := b0s
          user := b0s
        }
        if (hasNativeDir) {
          valid := 0
        }
      case SINK =>
        if (!enabled) {
          ready := 0
        }
      case FLOW =>
    }
  }
  @df final protected class WriteDataChannel(streamDir : StreamDir)(enabled : Boolean, simple : Boolean) extends Interface(streamDir) with Fire {
    final val data    = DFBits(32)
    final val strb    = DFBits(4)
    final val last    = DFBit()
    final val id      = DFBits(1)
    final val user    = DFBits(1)
    streamDir match {
      case SOURCE =>
        if (!enabled) {
          valid := 0
          data := b0s
          strb := b0s
          last := 0
          id := b0s
          user := b0s
        } else if (simple) {
          strb := b1s
          last := 0
          id := b0s
          user := b0s
        }
        if (hasNativeDir) {
          valid := 0
        }
      case SINK =>
        if (!enabled) {
          ready := 0
        }
      case FLOW =>
    }
  }
  @df final protected class WriteResponseChannel(streamDir : StreamDir)(enabled : Boolean, simple : Boolean) extends Interface(streamDir) {
    final val resp    = DFBits(2)
    final val id      = DFBits(1)
    final val user    = DFBits(1)
    streamDir match {
      case SOURCE =>
        if (!enabled) {
          valid := 0
          resp := b0s
          id := b0s
          user := b0s
        } else if (simple) {
          resp := b0s
          id := b0s
          user := b0s
        }
      case SINK =>
        if (!enabled) {
          ready := 0
        }
        if (hasNativeDir) {
          ready := 0
        }
      case FLOW =>
    }
  }
  @df final protected class ReadDataChannel(streamDir : StreamDir)(enabled : Boolean, simple : Boolean) extends Interface(streamDir) {
    final val data    = DFBits(32)
    final val last    = DFBit()
    final val id      = DFBits(1)
    final val user    = DFBits(1)
    final val resp    = DFBits(2)
    streamDir match {
      case SOURCE =>
        if (!enabled) {
          valid := 0
          data := b0s
          last := 0
          id := b0s
          user := b0s
          resp := b0s
        } else if (simple) {
          last := 0
          id := b0s
          user := b0s
          resp := b0s
        }
      case SINK =>
        if (!enabled) {
          ready := 0
        }
        if (hasNativeDir) {
          ready := 0
        }
      case FLOW =>
    }
  }
}
