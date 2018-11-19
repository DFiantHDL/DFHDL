package RISCV

import DFiant._

trait Execute extends DFDesign {
  private val branchSel = DFEnum[BranchSel] <> IN
  private val rs1OpSel  = DFEnum[RS1OpSel]  <> IN
  private val rs2OpSel  = DFEnum[RS2OpSel]  <> IN
  private val aluSel    = DFEnum[ALUSel]    <> IN
  private val rs1_data  = DFBits[XLEN]      <> IN
  private val rs2_data  = DFBits[XLEN]      <> IN
  private val pc        = DFBits[32]        <> IN
  private val imm       = DFBits[32]        <> IN
  private val shamt     = DFUInt[5]         <> IN
  private val pcNext    = DFBits[32]        <> OUT
  private val pcPlus4   = DFBits[32]        <> OUT
  private val aluOut    = DFBits[32]        <> OUT

  private val aluOp1 = DFBits[32].matchdf(rs1OpSel)
    .casedf(RS1OpSel.RegSource) {rs1_data}
    .casedf_                    {imm}
  private val aluOp2 = DFBits[32].matchdf(rs2OpSel)
    .casedf(RS2OpSel.RegSource) {rs2_data}
    .casedf(RS2OpSel.PC)        {pc}
    .casedf_                    {imm}

  private val alu = new ALU {}
  private val aluCalc = alu.calcConn(aluOp1, aluOp2, shamt, aluSel)
  private val pcGen = new PCGen {}
  private val pcCalc = pcGen.genPCConn(pc, branchSel, rs1_data, rs2_data, imm)
  pcNext <> pcCalc.pcNext
  pcPlus4 <> pcCalc.pcPlus4
  aluOut <> aluCalc

  def exConn(pc : DFBits[32], decodedInst: DecodedInst, rs1_data : DFBits[XLEN], rs2_data : DFBits[XLEN])(
    implicit ctx : DFDesign.Context
  ) : (PCCalc, ExecuteInst) = {
    this.branchSel <> decodedInst.branchSel
    this.rs1OpSel <> decodedInst.rs1OpSel
    this.rs2OpSel <> decodedInst.rs2OpSel
    this.aluSel <> decodedInst.aluSel
    this.rs1_data <> rs1_data
    this.rs2_data <> rs2_data
    this.pc <> pc
    this.imm <> decodedInst.imm
    this.shamt <> decodedInst.shamt
    val pcCalc = new PCCalc(this.pcNext, this.pcPlus4)
    val executeInst = new ExecuteInst(decodedInst.rd_addr, decodedInst.rd_wren, this.aluOut, rs2_data, decodedInst.dmem_wren, decodedInst.wbSel, this.aluOut)
    (pcCalc, executeInst)
  }
}

class ExecuteInst(
  val rd_addr   : DFBits[5],
  val rd_wren   : DFBool,

  val dmem_addr : DFBits[32],
  val dataToMem : DFBits[32],
  val dmem_wren : DFBool,

  val wbSel     : DFEnum[WriteBackSel],
  val aluOut    : DFBits[32]
)