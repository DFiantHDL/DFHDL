package DFiant

import DFiant.sim.DFSimDesign

@df class MemConn extends DFInterface {
  val addr    = DFBits(10) <> OUT
  val rdData  = DFBits(8)  <> IN
  val wrData  = DFBits(8)  <> OUT
  val wrEn    = DFBit()    <> OUT

  import fsm._
  def read(fromAddr : DFBits[10], dataOut : DFBits[8] <> VAR)(implicit __blockContext : DFBlock.Context) : FSM = {
    step {
      addr := fromAddr
      wrEn := 0
    } ==> step {
      dataOut := rdData
    }
  }
  def write(toAddr : DFBits[10], data : DFBits[8])(implicit __blockContext : DFBlock.Context) : FSM = {
    step {
      addr := toAddr
      wrData := data
      wrEn := 1
    } ==> step {
      wrEn := 0
    }
  }
}

@df class Dut extends DFDesign {
  val memConn = new MemConn
  memConn.wrEn := 0

//  class CopyFsm(fromAddr : BitVector, toAddr : BitVector, size : Int) extends DFSM2 {
//    step {
//      memConn.addr := fromAddr
//      memConn.wrEn := 0
//    }
//
//  }
////  val copy_fsm = new DFSM() {
//    
//    memConn.addr
//  }
}
@df class Driver extends DFSimDesign {
  val memConn = new MemConn <> FLIP
}
@df class Monitor extends DFSimDesign {
  val memConn = new MemConn <> IN
}

@df class Simulation extends DFSimDesign {
  val dut     = new Dut
  val driver  = new Driver
  val monitor = new Monitor
  val memConn = new MemConn <> VAR

  memConn <> dut.memConn
  memConn <> driver.memConn
  memConn <> monitor.memConn
}

