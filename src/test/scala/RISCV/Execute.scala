package RISCV

import DFiant._

class Execute(regFileInst: RegFileInst)(implicit ctx : DFDesign.ContextOf[Execute]) extends DFDesign {
  private val branchSel = DFEnum[BranchSel] <> IN
  private val rs1OpSel  = DFEnum[RS1OpSel]  <> IN
  private val rs2OpSel  = DFEnum[RS2OpSel]  <> IN
  private val aluSel    = DFEnum[ALUSel]    <> IN
  private val rs1_data  = DFBits[XLEN]      <> IN
  private val rs2_data  = DFBits[XLEN]      <> IN
  private val pc        = DFBits[32]        <> IN
  private val imm       = DFBits[32]        <> IN
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
  private val aluCalc = alu.calcConn(aluOp1, aluOp2, aluSel)
  private val pcGen = new PCGen(pc, branchSel, rs1_data, rs2_data, imm)
  pcNext <> pcGen.pcNext
  pcPlus4 <> pcGen.pcPlus4
  aluOut <> aluCalc
//  sim.report(msg"rs1_data: $rs1_data, rs2_data: $rs2_data, imm: $imm, rs1OpSel: $rs1OpSel, aluOp1: $aluOp1, rs2OpSel: $rs2OpSel, aluOp2: $aluOp2, aluSel: $aluSel, aluCalc: $aluCalc")

  final val inst = {
    import regFileInst._
    ExecuteInst(
      //IMem
      pc = regFileInst.pc, instRaw = instRaw,
      //Decoder
      rs1_addr = rs1_addr, rs2_addr = rs2_addr, rd_addr = rd_addr, rd_wren = rd_wren,
      imm = regFileInst.imm, branchSel = regFileInst.branchSel,
      rs1OpSel = regFileInst.rs1OpSel, rs2OpSel = regFileInst.rs2OpSel,
      aluSel = regFileInst.aluSel, wbSel = wbSel, dmemSel = dmemSel, debugOp = debugOp,
      //RegFile
      rs1_data = regFileInst.rs1_data, rs2_data = regFileInst.rs2_data,
      //Execute
      dmem_addr = aluOut, dataToMem = regFileInst.rs2_data, aluOut = aluOut, pcNext = pcNext, pcPlus4 = pcPlus4
    )
  }

  atOwnerDo {
    this.branchSel <> regFileInst.branchSel
    this.rs1OpSel <> regFileInst.rs1OpSel
    this.rs2OpSel <> regFileInst.rs2OpSel
    this.aluSel <> regFileInst.aluSel
    this.rs1_data <> regFileInst.rs1_data
    this.rs2_data <> regFileInst.rs2_data
    this.pc <> regFileInst.pc
    this.imm <> regFileInst.imm
  }
}

case class ExecuteInst(
  //IMem
  pc        : DFBits[32],
  instRaw   : DFBits[32],

  //Decoder
  rs1_addr  : DFBits[5],
  rs2_addr  : DFBits[5],
  rd_addr   : DFBits[5],
  rd_wren   : DFBool,
  imm       : DFBits[32],
  branchSel : DFEnum[BranchSel],
  rs1OpSel  : DFEnum[RS1OpSel],
  rs2OpSel  : DFEnum[RS2OpSel],
  aluSel    : DFEnum[ALUSel],
  wbSel     : DFEnum[WriteBackSel],
  dmemSel   : DFEnum[DMemSel],
  debugOp   : DFEnum[DebugOp],

  //RegFile
  rs1_data  : DFBits[XLEN],
  rs2_data  : DFBits[XLEN],

  //Execute
  dmem_addr : DFBits[32],
  dataToMem : DFBits[32],
  aluOut    : DFBits[32],
  pcNext    : DFBits[32],
  pcPlus4   : DFBits[32]
)
