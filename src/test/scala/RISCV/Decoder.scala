package RISCV

import DFiant._

trait Decoder extends DFDesign {
  final val inst      = DFBits[32]            <> IN

  //Register File Addresses & Control
  final val rs1_addr  = DFBits[5]             <> OUT
  final val rs2_addr  = DFBits[5]             <> OUT
  final val rd_addr   = DFBits[5]             <> OUT
  final val rd_wren   = DFBool()              <> OUT

  //Immediate values for ALU execution
  final val imm       = DFBits[32]            <> OUT
  final val shamt     = DFUInt[5]             <> OUT

  //Control Signals
  final val branchSel = DFEnum(BranchSel)     <> OUT
  final val rs1OpSel  = DFEnum(RS1OpSel)      <> OUT
  final val rs2OpSel  = DFEnum(RS2OpSel)      <> OUT
  final val aluSel    = DFEnum(ALUSel)        <> OUT
  final val wbSel     = DFEnum(WriteBackSel)  <> OUT
  final val mem_wren  = DFBool()              <> OUT

  def decodeConn(inst : DFBits[32])(implicit ctx : DFDesign.Context) : DecodedInst = {
    this.inst <> inst
    new DecodedInst(rs1_addr = rs1_addr, rs2_addr = rs2_addr, rd_addr = rd_addr, rd_wren = rd_wren,
      imm = imm, shamt = shamt, branchSel = branchSel, rs1OpSel = rs1OpSel, rs2OpSel = rs2OpSel,
      aluSel = aluSel, wbSel = wbSel, mem_wren = mem_wren)
  }
}


class DecodedInst(
  val rs1_addr  : DFBits[5],
  val rs2_addr  : DFBits[5],
  val rd_addr   : DFBits[5],
  val rd_wren   : DFBool,

//Immediate values for ALU execution
  val imm       : DFBits[32],
  val shamt     : DFUInt[5],

//Control Signals
  val branchSel : DFEnum[BranchSel],
  val rs1OpSel  : DFEnum[RS1OpSel],
  val rs2OpSel  : DFEnum[RS2OpSel],
  val aluSel    : DFEnum[ALUSel],
  val wbSel     : DFEnum[WriteBackSel],
  val mem_wren  : DFBool
)


trait Foo[T]
object Bug {
  def foo() : Foo[32] = ???
  val f = foo()
  def bar(f : Foo[32]) : Unit = {}
  bar(f)
}
