package DFiant.lib.ompss

import DFiant.compiler.sync._
import DFiant.compiler.backend.vhdl._
import DFiant._
import DFiant.internals.BitVectorExtras
import DFiant.sim.DFSimulator

@df class loopback_moved extends OmpssKernelDesign {
  final val d         = OmpssAXI    <> IN
  final val o         = OmpssAXI    <> OUT
  final val size      = DFBits(32)  <> IN

  /////////////////////////////////////////////////////////////////////////////
  // All this code will be automatically generated for final integration
  /////////////////////////////////////////////////////////////////////////////
  private val o_AWREADY_ack_reg = DFBit() init 0
  private val o_AWREADY_ack_sig = DFBit()
  private val o_WREADY_ack_reg  = DFBit() init 0
  private val o_WREADY_ack_sig  = DFBit()
  private val d_ARREADY_ack_reg = DFBit() init 0
  private val d_ARREADY_ack_sig = DFBit()

  private val i = DFBits(31) //i.prev
  private val i_plus1 = DFBits(31) //i+1
  private val i_plus1_reg = DFBits(31) //(i+1).prev
  private val d_addr_read_reg = DFBits(32)
  private val notDataEnd = DFBit()

  o.AW.LEN := size
  o.AW.ADDR := o.offset.resize(32)
  d.AR.LEN := size
  d.AR.ADDR := d.offset.resize(32)
  o.W.DATA := d_addr_read_reg

  ap.done := 0
  ap.ready := 0
  ap.idle := 0
  o.AW.VALID := 0
  o.W.VALID := 0
  o.B.READY := 0
  d.AR.VALID := 0
  d.R.READY := 0

  d_ARREADY_ack_sig := 1
  ifdf(!d_ARREADY_ack_reg.prev) {
    d_ARREADY_ack_sig := d.AR.READY
  }
  o_AWREADY_ack_sig := 1
  ifdf(!o_AWREADY_ack_reg.prev) {
    o_AWREADY_ack_sig := o.AW.READY
  }
  o_WREADY_ack_sig := 1
  ifdf(!o_WREADY_ack_reg.prev) {
    o_WREADY_ack_sig := o.W.READY
  }
  i_plus1 := (i.uint + 1).bits
  notDataEnd := i.resize(32).sint < size.sint

  import fsm._
  final val IDLE : FSM = step {
    ifdf(ap.start) {
      ifdf(!d_ARREADY_ack_reg.prev){
        d.AR.VALID := 1
      }
      ifdf(d_ARREADY_ack_sig) {
        d_ARREADY_ack_reg := 0
        ST7.goto()
      }.elseifdf(d.AR.READY) {
        d_ARREADY_ack_reg := 1
      }
    }.elsedf {
      ap.done := 1
      ap.idle := 1
    }
  }
  final val ST7 : FSM = step {
    ifdf(!o_AWREADY_ack_reg.prev) {
      o.AW.VALID := 1
    }
    ifdf(o_AWREADY_ack_sig) {
      o_AWREADY_ack_reg := 0
      i := b0s
      ST8.goto()
    }.elseifdf(o.AW.READY) {
      o_AWREADY_ack_reg := 1
    }
  }
  final val ST8 : FSM = step {
    ifdf(d.R.VALID || !notDataEnd) {
      ifdf(notDataEnd) {
        d.R.READY := 1
        i_plus1_reg := i_plus1
        d_addr_read_reg := d.R.DATA
        ST9.goto()
      }.elsedf {
        ST13.goto()
      }
    }
  }
  final val ST9 : FSM = step {
    ifdf(!o_WREADY_ack_reg.prev) {
      o.W.VALID := 1
    }
    ifdf(o_WREADY_ack_sig) {
      o_WREADY_ack_reg := 0
      i := i_plus1_reg
      ST8.goto()
    }.elseifdf(o.W.READY) {
      o_WREADY_ack_reg := 1
    }
  }
  final val ST13 : FSM = waitUntil(o.B.VALID).onExit {
    o.B.READY := 1
    ap.done := 1
    ap.ready := 1
  } ==> IDLE

  val myfsm = IDLE ++ ST7 ++ ST8 ++ ST9 ++ ST13

  myfsm.elaborate
}



@df class LoopbackDriver extends DFSimulator {
  final val ap        = new AP_Interface <> FLIP
  final val d         = OmpssAXI <> IN <> FLIP
  final val o         = OmpssAXI <> OUT <> FLIP
  final val size      = DFBits(32) <> OUT
  val c_READ_BUF_ADDR   = h"00001000"
  val c_WRITE_BUF_ADDR  = h"00020000"
  val c_SIZE            = h"00000020"

  import fsm._
  private val ap_drv_fsm =
    step {
      ap.start := 0
      o.offset := b0s
      d.offset := b0s
      size := b0s
    } ==> step {
      o.offset := c_WRITE_BUF_ADDR.resize(64)
      d.offset := c_READ_BUF_ADDR.resize(64)
      size := c_SIZE
      ap.start := 1
    } ==> waitUntil(ap.ready) ==> step {
      sim.report(msg"Got ap_ready")
    } ==> waitForever()

  ap_drv_fsm.elaborate

  private val d_addr_fsm =
    step {
      d.AR.READY := 0
    } ==> waitUntil(ap.start) ==> step {
      d.AR.READY := 1
    } ==> waitUntil(d.AR.VALID) ==> waitForever()

  d_addr_fsm.elaborate

  private val read_flag = DFBool() init false
  private val read_first = DFBool() init true
  private val read_addr_checker =
    doUntil(d.AR.READY && d.AR.VALID) {
      ifdf (ap.done === 1 && !read_flag && !read_first) {
        sim.report(msg"No READ address given until ap_done", sim.Error)
      }
      ifdf(ap.done === 1) {
        read_flag := false
      }
    } ==> step {
      read_first := false
      sim.assert(d.AR.ADDR === c_READ_BUF_ADDR, msg"Bad read address")
      sim.assert(d.AR.LEN === c_SIZE, msg"Bad read size")
      sim.assert(!read_flag, msg"Unexpected address read")
      read_flag := true
    } ==> firstStep

  read_addr_checker.elaborate

  private def dataFunc(cnt : DFUInt[32])(implicit __blockContext : DFBlock.Context) : DFBits[32] = {
    cnt.bits
  }
  private val read_cnt = DFUInt(32) init 0
  private val read_size = DFUInt(32)
  private val d_data_fsm =
    step {
      d.R.VALID := 0
    } ==> waitUntil(d.AR.READY && d.AR.VALID) ==> step {
      read_size := d.AR.LEN.uint
      read_cnt := 0
    } ==> doUntil(read_cnt === read_size) {
      d.R.DATA := dataFunc(read_cnt)
      d.R.VALID := 1
      ifdf(d.R.READY) {
        read_cnt := read_cnt + 1
      }
    } ==> step{} ==> firstStep

  d_data_fsm.elaborate

  private val o_addr_fsm =
    step {
      o.AW.READY := 0
    } ==> waitUntil(ap.start) ==> step {
      o.AW.READY := 1
    } ==> waitUntil(o.AW.VALID) ==> waitForever

  o_addr_fsm.elaborate

  private val write_cnt = DFUInt(32) init 0
  private val write_size = DFUInt(32)
  private val o_data_fsm =
    step {
      o.W.READY := 0
      o.B.VALID := 0
    } ==> waitUntil(o.AW.READY && o.AW.VALID) ==> step {
      write_size := o.AW.LEN.uint
      write_cnt := 0
    } ==> doUntil(write_cnt === write_size) {
      o.W.READY := 1
      ifdf(o.W.VALID) {
        sim.assert(dataFunc(write_cnt) === o.W.DATA, msg"Bad write data")
        write_cnt := write_cnt + 1
      }
    } ==> doUntil(o.B.READY) {
      o.W.READY := 0
      o.B.VALID := 1
    } ==> firstStep

  o_data_fsm.elaborate

  private val write_flag = DFBool() init false
  private val write_first = DFBool() init true
  private val write_addr_checker =
    doUntil(o.AW.READY && o.AW.VALID) {
      ifdf (ap.done === 1 && !write_flag && !write_first) {
        sim.report(msg"No WRITE address given until ap_done", sim.Error)
      }
      ifdf (ap.done === 1) {
        write_flag := false
      }
    } ==> step {
      write_first := false
      sim.assert(o.AW.ADDR === c_WRITE_BUF_ADDR, msg"Bad write address")
      sim.assert(o.AW.LEN === c_SIZE, msg"Bad write size")
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
  loopback_test.printCodeString().compile.toFolder("loopback")//("/media/soronpo/loopback/zedboard/loopback_ait/xilinx/HLS/loopback/solution1/impl/ip/hdl/vhdl/")
}

