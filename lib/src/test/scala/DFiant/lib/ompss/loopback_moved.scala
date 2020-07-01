import DFiant._
import compiler.sync._
import compiler.backend.vhdl.v93
import lib.ompss._
import internals.BitVectorExtras
import sim.DFSimDesign

@df class loopback_ifc extends OmpssIfc {
  final val d         = OmpssAXI    <> IN
  final val o         = OmpssAXI    <> OUT
  final val size      = DFBits(32)  <> IN
}

@df class loopback_moved extends DFDesign {
  final val io = new loopback_ifc
  import io._

  /////////////////////////////////////////////////////////////////////////////
  // Further optimization will be done
  /////////////////////////////////////////////////////////////////////////////
  private val i = DFBits(31) //i.prev
  private val i_plus1 = DFBits(31) //i+1
  private val i_plus1_reg = DFBits(31) //(i+1).prev
  private val d_addr_read_reg = DFBits(32)
  private val notDataEnd = DFBit()

  o.AW.len := size
  o.AW.addr := o.offset.resize(32)
  d.AR.len := size
  d.AR.addr := d.offset.resize(32)
  o.W.data := d_addr_read_reg

  i_plus1 := (i.uint + 1).bits
  notDataEnd := i.resize(32).sint < size.sint

  import fsm._

  final val loopbackFSM : FSM = ap.startFSM ==> READ_BLOCK_REQ
  final val READ_BLOCK_REQ : FSM = d.AR.fireFSM{} ==> WRITE_BLOCK_REQ
  final val WRITE_BLOCK_REQ : FSM = o.AW.fireFSM{i := b0s} ==> READ_DATA
  final val READ_DATA : FSM = step {} =?> (!d.R.valid && notDataEnd) =!> thisStep =?> notDataEnd =^> {
    d.R.ready := 1
    i_plus1_reg := i_plus1
    d_addr_read_reg := d.R.data
  } =!> WRITE_DATA =!> FINISH
  final val WRITE_DATA : FSM = o.W.fireFSM{i := i_plus1_reg} ==> READ_DATA
  final val FINISH : FSM = ap.finishFSM(o.B.valid){o.B.ready := 1} ==> loopbackFSM
  //final val FINISH : FSM = waitUntil(o.B.VALID) =^> {o.B.READY := 1} ==> ap.finishFSM ==> loopbackFSM
}



@df class LoopbackDriver extends DFSimDesign {
  final val io = new loopback_ifc <> FLIP
  import io._
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
      sim.report(msg"Got first ap_ready")
    } ==> waitForever()

  private val d_addr_fsm =
    step {
      d.AR.ready := 0
    } ==> waitUntil(ap.start) ==> step {
      d.AR.ready := 1
    } ==> waitUntil(d.AR.valid) ==> waitForever()

  private val read_flag = DFBool() init false
  private val read_first = DFBool() init true
  private val read_addr_checker : FSM =
    doUntil(d.AR.ready && d.AR.valid) {
      ifdf (ap.done && !read_flag && !read_first) {
        sim.report(msg"No READ address given until ap_done", sim.Error)
      }
      ifdf(ap.done) {
        read_flag := false
      }
    } ==> step {
      read_first := false
      sim.assert(d.AR.addr === c_READ_BUF_ADDR, msg"Bad read address")
      sim.assert(d.AR.len === c_SIZE, msg"Bad read size")
      sim.assert(!read_flag, msg"Unexpected address read")
      read_flag := true
    } ==> read_addr_checker

  private def dataFunc(cnt : DFUInt[32])(implicit ctx : DFBlock.Context) : DFBits[32] = {
    cnt.bits
  }
  private val read_cnt = DFUInt(32) init 0
  private val read_size = DFUInt(32)
  private val d_data_fsm : FSM =
    step {
      d.R.valid := 0
    } ==> waitUntil(d.AR.ready && d.AR.valid) ==> step {
      read_size := d.AR.len.uint
      read_cnt := 0
    } ==> doUntil(read_cnt === read_size) {
      d.R.data := dataFunc(read_cnt)
      d.R.valid := 1
      ifdf(d.R.ready) {
        read_cnt := read_cnt + 1
      }
    } ==> step{} ==> d_data_fsm

  private val o_addr_fsm =
    step {
      o.AW.ready := 0
    } ==> waitUntil(ap.start) ==> step {
      o.AW.ready := 1
    } ==> waitUntil(o.AW.valid) ==> waitForever()

  private val write_cnt = DFUInt(32) init 0
  private val write_size = DFUInt(32)
  private val o_data_fsm : FSM =
    step {
      o.W.ready := 0
      o.B.valid := 0
    } ==> waitUntil(o.AW.ready && o.AW.valid) ==> step {
      write_size := o.AW.len.uint
      write_cnt := 0
    } ==> doUntil(write_cnt === write_size) {
      o.W.ready := 1
      ifdf(o.W.valid) {
        sim.assert(dataFunc(write_cnt) === o.W.data, msg"Bad write data")
        write_cnt := write_cnt + 1
      }
    } ==> doUntil(o.B.ready) {
      o.W.ready := 0
      o.B.valid := 1
    } ==> o_data_fsm

  private val write_flag = DFBool() init false
  private val write_first = DFBool() init true
  private val write_addr_checker : FSM =
    doUntil(o.AW.ready && o.AW.valid) {
      ifdf (ap.done && !write_flag && !write_first) {
        sim.report(msg"No WRITE address given until ap_done", sim.Error)
      }
      ifdf (ap.done) {
        write_flag := false
      }
    } ==> step {
      write_first := false
      sim.assert(o.AW.addr === c_WRITE_BUF_ADDR, msg"Bad write address")
      sim.assert(o.AW.len === c_SIZE, msg"Bad write size")
      sim.assert(!write_flag, msg"Unexpected address write")
      write_flag := true
    } =^> {
      sim.report(msg"completed loopback")
    } ==> write_addr_checker
}

@df class LoopbackTest extends DFSimDesign  {
  final val lb = new loopback_moved {}
  final val lb_drv = new LoopbackDriver {}
  lb.io <> lb_drv.io
}

object LoopbackTestApp extends App {
  object loopback_test extends LoopbackTest {
    this !! ClockParams("ap_clk", ClockParams.Edge.Rising)
    this !! ResetParams("ap_rst", ResetParams.Mode.Sync, ResetParams.Active.High)
  }
  import sim.tools.ghdl
  loopback_test.printCodeString.compile.printCodeString.toFolder("loopback").simulation.run()
}

object LoopbackApp extends App {
  object loopback_moved extends loopback_moved {
    this !! ClockParams("ap_clk", ClockParams.Edge.Rising)
    this !! ResetParams("ap_rst", ResetParams.Mode.Sync, ResetParams.Active.High)
  }
  loopback_moved.printCodeString.compile.toFile("/media/soronpo/loopback/zedboard/loopback_ait/xilinx/HLS/loopback/solution1/impl/ip/hdl/vhdl/loopback_moved.vhd")
}
