import DFiant._
import lib.ompss._
import internals.BitVectorExtras
import sim.DFSimDesign

@df class loopback_ifc extends OmpssIfc {
  final val d    = OmpssAXI   <> IN
  final val o    = OmpssAXI   <> OUT
  final val size = DFBits(32) <> IN
}

@df class loopback extends DFDesign {
  final val io = new loopback_ifc
  import io._

  private val i = DFBits(31) <> VAR //i.prev
  private val i_plus1 = DFBits(31) <> VAR //i+1
  private val i_plus1_reg = DFBits(31) <> VAR //(i+1).prev
  private val d_addr_read_reg = DFBits(32) <> VAR
  private val notDataEnd = DFBit <> VAR

  o.AW.len  := size
  o.AW.addr := o.offset.resize(32)
  d.AR.len  := size
  d.AR.addr := d.offset.resize(32)
  o.W.data  := d_addr_read_reg

  i_plus1    := (i.uint + 1).bits
  notDataEnd := i.resize(32).sint < size.sint

  final val loopbackFSM: FSM     = ap.startFSM ==> READ_BLOCK_REQ
  final val READ_BLOCK_REQ: FSM  = d.AR.fireFSM ==> WRITE_BLOCK_REQ
  final val WRITE_BLOCK_REQ: FSM = o.AW.fireFSM =^> { i := b0s } ==> READ_DATA
  final val READ_DATA: FSM = FSM {
    ifdf(!d.R.valid && notDataEnd) {
      //Do nothing
    }.elseifdf(notDataEnd) {
        d.R.ready       := 1
        i_plus1_reg     := i_plus1
        d_addr_read_reg := d.R.data
        WRITE_DATA.goto()
      }
      .elsedf {
        FINISH.goto()
      }
  }
  final val WRITE_DATA: FSM = o.W.fireFSM =^> { i := i_plus1_reg } ==> READ_DATA
  final val FINISH: FSM = ap.finishFSM(o.B.valid) =^> {
    o.B.ready := 1
  } ==> loopbackFSM
}

@df class LoopbackDriver extends DFSimDesign {
  final val io = new loopback_ifc <> FLIP
  import io._
  val c_READ_BUF_ADDR  = h"00001000"
  val c_WRITE_BUF_ADDR = h"00020000"
  val c_SIZE           = h"00000020"

  import lib.sequential._
  private val ap_drv_fsm : FSM =
    {
      ap.start := 0
      o.offset := b0s
      d.offset := b0s
      size := b0s
    } ==> {
      o.offset := c_WRITE_BUF_ADDR.resize(64)
      d.offset := c_READ_BUF_ADDR.resize(64)
      size := c_SIZE
      ap.start := 1
    } ==> waitUntil(ap.ready) ==> {
      sim.report(msg"Got first ap_ready")
    } ==> waitForever()

  private val d_addr_fsm : FSM =
    {
      d.AR.ready := 0
    } ==> waitUntil(ap.start) ==> {
      d.AR.ready := 1
    } ==> waitUntil(d.AR.valid) ==> waitForever()

  private val read_flag = DFBool <> VAR init false
  private val read_first = DFBool <> VAR init true
  private val read_addr_checker : FSM =
    doUntil(d.AR.ready && d.AR.valid) {
      ifdf(ap.done && !read_flag && !read_first) {
        sim.report(msg"No READ address given until ap_done", sim.Error)
      }
      ifdf(ap.done) {
        read_flag := false
      }
    } ==> {
      read_first := false
      sim.assert(d.AR.addr === c_READ_BUF_ADDR, msg"Bad read address")
      sim.assert(d.AR.len === c_SIZE, msg"Bad read size")
      sim.assert(!read_flag, msg"Unexpected address read")
      read_flag := true
    } ==> read_addr_checker

  def dataFunc(cnt: DFUInt[32])(implicit ctx: DFBlock.Context): DFBits[32] = {
    cnt.bits
  }
  private val read_cnt = DFUInt(32) <> VAR init 0
  private val read_size = DFUInt(32) <> VAR
  private val d_data_fsm : FSM =
    {
      d.R.valid := 0
    } ==> waitUntil(d.AR.ready && d.AR.valid) ==> {
      read_size := d.AR.len.uint
      read_cnt := 0
    } ==> doUntil(read_cnt === read_size) {
      d.R.data := dataFunc(read_cnt)
      d.R.valid := 1
      ifdf(d.R.ready) {
        read_cnt := read_cnt + 1
      }
    } ==> FSM{} ==> d_data_fsm

  private val o_addr_fsm: FSM = {
    o.AW.ready := 0
  } ==> waitUntil(ap.start) ==> {
    o.AW.ready := 1
  } ==> waitUntil(o.AW.valid) ==> waitForever()

  private val write_cnt = DFUInt(32) <> VAR init 0
  private val write_size = DFUInt(32) <> VAR
  private val o_data_fsm : FSM =
    {
      o.W.ready := 0
      o.B.valid := 0
    } ==> waitUntil(o.AW.ready && o.AW.valid) ==> {
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

  private val write_flag = DFBool <> VAR init false
  private val write_first = DFBool <> VAR init true
  private val write_addr_checker : FSM =
    doUntil(o.AW.ready && o.AW.valid) {
      ifdf(ap.done && !write_flag && !write_first) {
        sim.report(msg"No WRITE address given until ap_done", sim.Error)
      }
      ifdf(ap.done) {
        write_flag := false
      }
    } ==> {
      write_first := false
      sim.assert(o.AW.addr === c_WRITE_BUF_ADDR, msg"Bad write address")
      sim.assert(o.AW.len === c_SIZE, msg"Bad write size")
      sim.assert(!write_flag, msg"Unexpected address write")
      write_flag := true
      sim.report(msg"completed loopback")
    } ==> write_addr_checker
}

@df class LoopbackTest extends DFSimDesign {
  final val lb     = new loopback {}
  final val lb_drv = new LoopbackDriver {}
  lb.io <> lb_drv.io
}

object LoopbackTestApp extends App {
  val top_test = new LoopbackTest
  import sim.tools.ghdl
  top_test.printCodeString.compile.printCodeString
    .toFolder("loopback")
    .simulation
    .run()
}

object LoopbackApp extends App {
  val top = new loopback
  top.printCodeString.compile.printCodeString.toFile(
    "loopback/zedboard/loopback_ait/xilinx/HLS/loopback/solution1/impl/ip/hdl/vhdl/loopback_moved.vhd"
  )
}
