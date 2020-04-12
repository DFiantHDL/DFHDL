package DFiant

import xilinx._
import compiler.backend.vhdl._
import compiler.sync._

object AP_FSM extends EnumType.Auto {
  val IDLE, ST2, ST3, ST4, ST5, ST6, ST7, ST8, ST9, ST10, ST11, ST12, ST13 = Entry()
}

trait Loopback extends VivadoHLSDesign {
  final val d         = new AXI4(AXI4.Config(rdEnabled = false, wrEnabled = true, simple = true))
  final val d_offset  = DFBits(64) <> IN
  final val o         = new AXI4(AXI4.Config(rdEnabled = true, wrEnabled = false, simple = true))
  final val o_offset  = DFBits(64) <> IN
  final val size      = DFBits(32) <> IN
  final val ap_fsm    = DFEnum(AP_FSM) init AP_FSM.IDLE
  val d_blk_n_B = DFBit()
  val o_blk_n_AR = DFBit()
  val ap_reg_ioackin_m_axi_o_ARREADY = DFBit() init 0
  val ap_sig_ioackin_m_axi_o_ARREADY = DFBit()

  d.AW.LEN := size
  d.AW.ADDR := d_offset.resize(32)
  o.AR.LEN := size
  o.AR.ADDR := o_offset.resize(32)

  ap.done := 0
  ap.ready := 0
  ap.idle := 0
  o.AR.VALID := 0

  d_blk_n_B := 1
  o_blk_n_AR := 1
  ap_sig_ioackin_m_axi_o_ARREADY := 1
  ifdf(!ap_reg_ioackin_m_axi_o_ARREADY) {
    ap_sig_ioackin_m_axi_o_ARREADY := o.AR.READY
  }
  matchdf(ap_fsm)
    .casedf(AP_FSM.IDLE){
      ifdf(ap.start) {
        o_blk_n_AR := o.AR.READY
        ifdf(!ap_reg_ioackin_m_axi_o_ARREADY){
          o.AR.VALID := 1
        }
        ifdf(ap_sig_ioackin_m_axi_o_ARREADY) {
          ap_reg_ioackin_m_axi_o_ARREADY := 0
          ap_fsm := AP_FSM.ST2
        }.elseifdf(o.AR.READY) {
          ap_reg_ioackin_m_axi_o_ARREADY := 1
        }
      }.elsedf {
        ap.done := 1
        ap.idle := 1
      }
    }
    .casedf(AP_FSM.ST2){ap_fsm := AP_FSM.ST3}
    .casedf(AP_FSM.ST3){ap_fsm := AP_FSM.ST4}
    .casedf(AP_FSM.ST4){ap_fsm := AP_FSM.ST5}
    .casedf(AP_FSM.ST5){ap_fsm := AP_FSM.ST6}
    .casedf(AP_FSM.ST6){ap_fsm := AP_FSM.ST7}
    .casedf(AP_FSM.ST7){ap_fsm := AP_FSM.ST8}
    .casedf(AP_FSM.ST8){ap_fsm := AP_FSM.ST9}
    .casedf(AP_FSM.ST9){ap_fsm := AP_FSM.ST10}
    .casedf(AP_FSM.ST10){ap_fsm := AP_FSM.ST11}
    .casedf(AP_FSM.ST11){ap_fsm := AP_FSM.ST12}
    .casedf(AP_FSM.ST12){ap_fsm := AP_FSM.ST13}
    .casedf(AP_FSM.ST13){
      d_blk_n_B := d.B.VALID
      ifdf(d.B.VALID){
        ap_fsm := AP_FSM.IDLE
        ap.done := 1
        ap.ready := 1
      }
    }

}

object LoopbackApp extends App {
  object loopback_moved extends Loopback {
    this !! ClockParams("ap_clk", ClockParams.Edge.Rising)
    this !! ResetParams("ap_rst", ResetParams.Mode.Sync, ResetParams.Active.High)

  }
  loopback_moved.printCodeString().compile.toFolder("loopback")
}
