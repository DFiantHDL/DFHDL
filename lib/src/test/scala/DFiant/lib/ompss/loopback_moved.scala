package DFiant.lib.ompss

import DFiant.compiler.sync._
import DFiant.compiler.backend.vhdl._
import DFiant._
import DFiant.internals.BitVectorExtras

@df class loopback_moved extends OmpssKernelDesign {
  //d is output
  final val d         = OmpssAXI <> OUT
  //o is input
  final val o         = OmpssAXI <> IN


  /////////////////////////////////////////////////////////////////////////////
  // All this code will be automatically generated for final integration
  /////////////////////////////////////////////////////////////////////////////
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

  d.AW.LEN := size
  d.AW.ADDR := d.offset.resize(32)
  o.AR.LEN := size
  o.AR.ADDR := o.offset.resize(32)
  d.W.DATA := o_addr_read_reg_152

  ap.done := 0
  ap.ready := 0
  ap.idle := 0
  d.AW.VALID := 0
  d.W.VALID := 0
  d.B.READY := 0
  o.AR.VALID := 0
  o.R.READY := 0

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

  import dfsm._
  final val IDLE : FSM = step {
    ifdf(ap.start) {
      ifdf(!ap_reg_ioackin_m_axi_o_ARREADY.prev){
        o.AR.VALID := 1
      }
      ifdf(ap_sig_ioackin_m_axi_o_ARREADY) {
        ap_reg_ioackin_m_axi_o_ARREADY := 0
        ST7.goto()
      }.elseifdf(o.AR.READY) {
        ap_reg_ioackin_m_axi_o_ARREADY := 1
      }
    }.elsedf {
      ap.done := 1
      ap.idle := 1
    }
  }
  final val ST7 : FSM = step {
    ifdf(!ap_reg_ioackin_m_axi_d_AWREADY.prev) {
      d.AW.VALID := 1
    }
    ifdf(ap_sig_ioackin_m_axi_d_AWREADY) {
      ap_reg_ioackin_m_axi_d_AWREADY := 0
      i_reg_85 := b0s
      ST8.goto()
    }.elseifdf(d.AW.READY) {
      ap_reg_ioackin_m_axi_d_AWREADY := 1
    }
  }
  final val ST8 : FSM = step {
    ifdf(o.R.VALID || !tmp_fu_114_p2) {
      ifdf(tmp_fu_114_p2) {
        o.R.READY := 1
        i_1_reg_147 := i_1_fu_119_p2
        o_addr_read_reg_152 := o.R.DATA
        ST9.goto()
      }.elsedf {
        ST13.goto()
      }
    }
  }
  final val ST9 : FSM = step {
    ifdf(!ap_reg_ioackin_m_axi_d_WREADY.prev) {
      d.W.VALID := 1
    }
    ifdf(ap_sig_ioackin_m_axi_d_WREADY) {
      ap_reg_ioackin_m_axi_d_WREADY := 0
      i_reg_85 := i_1_reg_147
      ST8.goto()
    }.elseifdf(d.W.READY) {
      ap_reg_ioackin_m_axi_d_WREADY := 1
    }
  }
  final val ST13 : FSM = waitUntil(d.B.VALID).onExit {
    d.B.READY := 1
    ap.done := 1
    ap.ready := 1
  } ==> IDLE

  val myfsm = IDLE ++ ST7 ++ ST8 ++ ST9 ++ ST13

  myfsm.elaborate
}



@df class LoopbackDriver extends DFSimulator {
  final val ap        = new AP_Interface <> FLIP
  final val d         = OmpssAXI <> OUT <> FLIP
  final val o         = OmpssAXI <> IN <> FLIP
  final val size      = DFBits(32) <> OUT
  val c_READ_BUF_ADDR   = h"00001000"
  val c_WRITE_BUF_ADDR  = h"00020000"
  val c_SIZE            = h"00000020"

  import dfsm._
  private val ap_drv_fsm =
    step {
      ap.start := 0
      d.offset := b0s
      o.offset := b0s
      size := b0s
    } ==> step {
      d.offset := c_WRITE_BUF_ADDR.resize(64)
      o.offset := c_READ_BUF_ADDR.resize(64)
      size := c_SIZE
      ap.start := 1
    } ==> waitUntil(ap.ready) ==> step {
      sim.report(msg"Got ap_ready")
    } ==> waitForever()

  ap_drv_fsm.elaborate

  private val o_addr_fsm =
    step {
      o.AR.READY := 0
    } ==> waitUntil(ap.start) ==> step {
      o.AR.READY := 1
    } ==> waitUntil(o.AR.VALID) ==> waitForever()

  o_addr_fsm.elaborate

  private val read_flag = DFBool() init false
  private val first = DFBool() init true
  private val read_addr_checker =
    doUntil(o.AR.READY && o.AR.VALID) {
      ifdf (ap.done === 1 && !read_flag && !first) {
        sim.report(msg"No READ address given until ap_done", sim.Error)
      }
      ifdf(ap.done === 1) {
        read_flag := false
      }
    } ==> step {
      first := false
      sim.assert(o.AR.ADDR === c_READ_BUF_ADDR, msg"Bad read address")
      sim.assert(o.AR.LEN === c_SIZE, msg"Bad read size")
      sim.assert(!read_flag, msg"Unexpected address read")
      read_flag := true
    } ==> firstStep

  read_addr_checker.elaborate

  private def dataFunc(cnt : DFUInt[32])(implicit __blockContext : DFBlock.Context) : DFBits[32] = {
    cnt.bits
  }
  private val read_cnt = DFUInt(32) init 0
  private val read_size = DFUInt(32)
  private val o_data_fsm =
    step {
      o.R.VALID := 0
    } ==> waitUntil(o.AR.READY && o.AR.VALID) ==> step {
      read_size := o.AR.LEN.uint
      read_cnt := 0
    } ==> doUntil(read_cnt === read_size) {
      o.R.DATA := dataFunc(read_cnt)
      o.R.VALID := 1
      ifdf(o.R.READY) {
        read_cnt := read_cnt + 1
      }
    } ==> step{} ==> firstStep

  o_data_fsm.elaborate

  private val d_addr_fsm =
    step {
      d.AW.READY := 0
    } ==> waitUntil(ap.start) ==> step {
      d.AW.READY := 1
    } ==> waitUntil(d.AW.VALID) ==> waitForever

  d_addr_fsm.elaborate

  private val write_cnt = DFUInt(32) init 0
  private val write_size = DFUInt(32)
  private val d_data_fsm = new DFSM2 {
    step {
      d.W.READY := 0
      d.B.VALID := 0
    }
    waitUntil(d.AW.READY && d.AW.VALID)
    step {
      write_size := d.AW.LEN.uint
      write_cnt := 0
    }
    doUntil(write_cnt === write_size) {
      d.W.READY := 1
      ifdf(d.W.VALID) {
        sim.assert(dataFunc(write_cnt) === d.W.DATA, msg"Bad write data")
        write_cnt := write_cnt + 1
      }
    }
    State {
      d.W.READY := 0
      d.B.VALID := 1
      ifdf(d.B.READY) {
        gotoStart()
      }
    }
  }

  private val write_flag = DFBool() init false
  private val write_addr_checker =
    doUntil(d.AW.READY && d.AW.VALID) {
      ifdf (ap.done === 1 && !write_flag) {
        sim.report(msg"No WRITE address given until ap_done", sim.Error)
      }
      ifdf (ap.done === 1) {
        write_flag := false
      }
    } ==> step {
      sim.assert(d.AW.ADDR === c_WRITE_BUF_ADDR, msg"Bad write address")
      sim.assert(d.AW.LEN === c_SIZE, msg"Bad write size")
      sim.assert(!write_flag, msg"Unexpected address write")
      write_flag := true
    } ==> firstStep

  write_addr_checker.elaborate
}

trait LoopbackTest extends DFSimulator  {
  final val lb = new loopback_moved {}
  final val lb_drv = new LoopbackDriver {}
  lb.ap <> lb_drv.ap
  lb.d <> lb_drv.d
  lb.o <> lb_drv.o
  lb.size <> lb_drv.size
}

object LoopbackApp extends App {
  object loopback_test extends LoopbackTest {
    this !! ClockParams("ap_clk", ClockParams.Edge.Rising)
    this !! ResetParams("ap_rst", ResetParams.Mode.Sync, ResetParams.Active.High)
  }
  loopback_test.compile.printCodeString().toFolder("loopback")//("/media/soronpo/loopback/zedboard/loopback_ait/xilinx/HLS/loopback/solution1/impl/ip/hdl/vhdl/")
}

