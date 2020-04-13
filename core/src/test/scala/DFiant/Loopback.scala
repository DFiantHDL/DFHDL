package DFiant

import xilinx._
import compiler.backend.vhdl._
import compiler.sync._

object AP_FSM extends EnumType.Auto {
  val IDLE, ST2, ST3, ST4, ST5, ST6, ST7, ST8, ST9, ST10, ST11, ST12, ST13 = Entry()
}

trait Loopback extends VivadoHLSDesign {
  //d is output
  final val d         = new AXI4(AXI4.Config(rdEnabled = false, wrEnabled = true, simple = true))
  final val d_offset  = DFBits(64) <> IN
  //o is input
  final val o         = new AXI4(AXI4.Config(rdEnabled = true, wrEnabled = false, simple = true))
  final val o_offset  = DFBits(64) <> IN
  final val size      = DFBits(32) <> IN
  private val ap_fsm    = DFEnum(AP_FSM) init AP_FSM.IDLE
  private val ap_reg_ioackin_m_axi_d_AWREADY = DFBit() init 0
  private val ap_sig_ioackin_m_axi_d_AWREADY = DFBit()
  private val ap_reg_ioackin_m_axi_d_WREADY  = DFBit() init 0
  private val ap_sig_ioackin_m_axi_d_WREADY  = DFBit()
  private val ap_reg_ioackin_m_axi_o_ARREADY = DFBit() init 0
  private val ap_sig_ioackin_m_axi_o_ARREADY = DFBit()

  private val i_reg_85 = DFBits(31) //i.prev
  private val i_1_fu_119_p2 = DFBits(31) //i+1
  private val i_1_reg_147 = DFBits(31) //(i+1).prev
  private val o_addr_read_reg_152 = DFBits(32)
  private val tmp_fu_114_p2 = DFBit()

  //optionals
  private val d_blk_n_B = DFBit()
  private val d_blk_n_AW = DFBit()
  private val d_blk_n_W = DFBit()
  private val o_blk_n_AR = DFBit()
  private val o_blk_n_R = DFBit()

  d.AW.LEN := size
  d.AW.ADDR := d_offset.resize(32)
  o.AR.LEN := size
  o.AR.ADDR := o_offset.resize(32)
  d.W.DATA := o_addr_read_reg_152

  ap.done := 0
  ap.ready := 0
  ap.idle := 0
  d.AW.VALID := 0
  d.W.VALID := 0
  o.AR.VALID := 0
  o.R.READY := 0

  d_blk_n_B := 1
  d_blk_n_AW := 1
  d_blk_n_W := 1
  o_blk_n_AR := 1
  o_blk_n_R := 1

  ap_sig_ioackin_m_axi_o_ARREADY := 1
  ifdf(!ap_reg_ioackin_m_axi_o_ARREADY.prev) {
    ap_sig_ioackin_m_axi_o_ARREADY := o.AR.READY
  }
  ap_sig_ioackin_m_axi_d_AWREADY := 1
  ifdf(!ap_reg_ioackin_m_axi_d_AWREADY.prev) {
    ap_sig_ioackin_m_axi_d_AWREADY := d.AW.READY
  }
  ap_sig_ioackin_m_axi_d_WREADY := 1
  ifdf(!ap_reg_ioackin_m_axi_d_WREADY.prev) {
    ap_sig_ioackin_m_axi_d_WREADY := d.W.READY
  }
  i_1_fu_119_p2 := (i_reg_85.uint + 1).bits
  tmp_fu_114_p2 := i_reg_85.resize(32).sint < size.sint

  matchdf(ap_fsm)
    .casedf(AP_FSM.IDLE){
      ifdf(ap.start) {
        o_blk_n_AR := o.AR.READY
        ifdf(!ap_reg_ioackin_m_axi_o_ARREADY.prev){
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
    .casedf(AP_FSM.ST7){
      d_blk_n_AW := d.AW.READY
      ifdf(!ap_reg_ioackin_m_axi_d_AWREADY.prev) {
        d.AW.VALID := 1
      }
      ifdf(ap_sig_ioackin_m_axi_d_AWREADY) {
        ap_reg_ioackin_m_axi_d_AWREADY := 0
        i_reg_85 := b0s
        ap_fsm := AP_FSM.ST8
      }.elseifdf(d.AW.READY) {
        ap_reg_ioackin_m_axi_d_AWREADY := 1
      }
    }
    .casedf(AP_FSM.ST8){
      o_blk_n_R := o.R.VALID
      ifdf(o.R.VALID || !tmp_fu_114_p2) {
        ifdf(tmp_fu_114_p2) {
          o.R.READY := 1
          i_1_reg_147 := i_1_fu_119_p2
          o_addr_read_reg_152 := o.R.DATA
          ap_fsm := AP_FSM.ST9
        }.elsedf {
          ap_fsm := AP_FSM.ST10
        }
      }
    }
    .casedf(AP_FSM.ST9){
      d_blk_n_W := d.W.READY
      ifdf(!ap_reg_ioackin_m_axi_d_WREADY.prev) {
        d.W.VALID := 1
      }
      ifdf(ap_sig_ioackin_m_axi_d_WREADY) {
        ap_reg_ioackin_m_axi_d_WREADY := 0
        i_reg_85 := i_1_reg_147
        ap_fsm := AP_FSM.ST8
      }.elseifdf(d.W.READY) {
        ap_reg_ioackin_m_axi_d_WREADY := 1
      }
    }
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


object LoopbackTest extends DFSimulator  {
  final val lb = new Loopback {}
//  final val VALID   = DFBit()     <> OUT
//  final val READY   = DFBit()     <> IN
//  final val ADDR    = DFBits(32)  <> OUT
//  final val ID      = DFBits(1)   <> OUT
//  final val LEN     = DFBits(32)  <> OUT
//  val d = lb.d.R
}
object LoopbackApp extends App {
  object loopback_moved extends Loopback {
    this !! ClockParams("ap_clk", ClockParams.Edge.Rising)
    this !! ResetParams("ap_rst", ResetParams.Mode.Sync, ResetParams.Active.High)

  }
  loopback_moved.printCodeString().compile.toFolder("loopback")
}
