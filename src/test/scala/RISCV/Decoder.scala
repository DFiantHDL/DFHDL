package RISCV

import DFiant._

trait Decoder extends DFDesign {
  final val inst      = DFBits(32)            <> IN

  //Register File Addresses & Control
  final val rs1_addr  = DFBits(5)             <> OUT
  final val rs2_addr  = DFBits(5)             <> OUT
  final val rd_addr   = DFBits(5)             <> OUT
  final val rd_wren   = DFBool()              <> OUT

  //Immediate values for ALU execution
  final val imm       = DFBits(32)            <> OUT
  final val shamt     = DFUInt(5)             <> OUT

  //Control Signals
  final val branchSel = DFEnum(BranchSel)     <> OUT
  final val rs1OpSel  = DFEnum(RS1OpSel)      <> OUT
  final val rs2OpSel  = DFEnum(RS2OpSel)      <> OUT
  final val aluSel    = DFEnum(ALUSel)        <> OUT
  final val wbSel     = DFEnum(WriteBackSel)  <> OUT
  final val mem_wren  = DFBool()              <> OUT


}
