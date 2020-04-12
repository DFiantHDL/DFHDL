package DFiant

import xilinx._
import compiler.backend.vhdl._
import compiler.sync._
trait Loopback extends VivadoHLSDesign {
  final val d         = new AXI4(AXI4.Config(rdEnabled = false, wrEnabled = true, simple = true))
  final val d_offset  = DFBits(64) <> IN
  final val o         = new AXI4(AXI4.Config(rdEnabled = true, wrEnabled = false, simple = true))
  final val o_offset  = DFBits(64) <> IN
  final val size      = DFBits(32) <> IN

  d.AW.LEN := size
  d.AW.ADDR := d_offset.resize(32)
  o.AR.LEN := size
  o.AR.ADDR := o_offset.resize(32)

}

object LoopbackApp extends App {
  object loopback_moved extends Loopback {
    this !! ClockParams("ap_clk", ClockParams.Edge.Rising)
    this !! ResetParams("ap_rst", ResetParams.Mode.Sync, ResetParams.Active.High)

  }
  loopback_moved.printCodeString().compile.toFolder("loopback")
}
