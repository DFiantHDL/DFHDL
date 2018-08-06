package Proc
import DFiant._
import DFiant.internals._
import singleton.ops._

trait Proc extends DFDesign {
  type MEM_ADDRW = 32
  type MEM_DATA_W = 32
  type XLEN = 32

  val PC_INIT_ADDR : Long with Singleton = 0x0L

  //IOs
  val imem_addrToMem     : DFUInt[XLEN]        <> OUT
  val imem_dataFromMem   : DFUInt[XLEN]        <> IN
  val dmem_addrToMem     : DFBits[MEM_ADDRW]   <> OUT
  val dmem_dataToMem     : DFBits[MEM_DATA_W]  <> OUT
  val dmem_dataFromMem   : DFBits[MEM_DATA_W]  <> IN


  //Register file
  val regsNum = 32
  val regs = Array.fill(regsNum)(DFBits[XLEN].init(0.toBitVector(valueOf[XLEN])))


  val pc = DFUInt[XLEN].init(0)

//  ifdf (instr.isRType) {
//  }.elsedf {
//
//  }
  pc := pc + 4

  imem_addrToMem := pc.prev
//  if (inst)
}
