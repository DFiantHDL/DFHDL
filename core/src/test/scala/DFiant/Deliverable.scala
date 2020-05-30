package DFiant

import DFiant.sim.DFSimulator

@df class MemConn extends DFInterface {
  val addr    = DFBits(10) <> OUT
  val rdData  = DFBits(8)  <> IN
  val wrData  = DFBits(8)  <> OUT
  val wrEn    = DFBit()    <> OUT
}

@df class Dut extends DFDesign {
  val memConn = new MemConn
  memConn.wrEn := 0

  class CopyCellFsm(fromAddr : DFBits[10], toAddr : DFBits[10]) extends DFSM2 {
    step {
      memConn.addr := fromAddr
      memConn.wrEn := 0
    }
    step {
      memConn.addr := toAddr
      memConn.wrEn := 1
      memConn.wrData := memConn.rdData
    }
  }
  class CopyFsm(fromAddr : BitVector, toAddr : BitVector, size : Int) extends DFSM2 {
    step {
      memConn.addr := fromAddr
      memConn.wrEn := 0
    }

  }
//  val copy_fsm = new DFSM() {
//    
//    memConn.addr
//  }
}
@df class Driver extends DFSimulator {
  val memConn = new MemConn <> FLIP
}
@df class Monitor extends DFSimulator {
  val memConn = new MemConn <> IN
}

@df class Simulation extends DFSimulator {
  val dut     = new Dut
  val driver  = new Driver
  val monitor = new Monitor
  val memConn = new MemConn <> VAR

  memConn <> dut.memConn
  memConn <> driver.memConn
  memConn <> monitor.memConn
}

