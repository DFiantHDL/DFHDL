package Proc
import DFiant._
import psuedoVendor.family.device._

trait Proc extends DFDesign {
  type MEM_ADDRW = 32
  type MEM_DATA_W = 32

  type XLEN = 32

  //IOs
  val pc            : DFUInt[XLEN]        <> OUT
  val instr         : Instr               <> IN
  val mem_addr      : DFBits[MEM_ADDRW]   <> OUT
  val mem_dataTo    : DFBits[MEM_DATA_W]  <> OUT
  val mem_dataFrom  : DFBits[MEM_DATA_W]  <> IN


  //Register file
  val regsNum = 32
  val regs = Array.fill(regsNum)(DFBits[XLEN].init(0))


  pc := pc + 1
//  if (inst)
}
