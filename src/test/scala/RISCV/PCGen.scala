package RISCV

import DFiant._

trait PCGen extends DFDesign {
  final val pcCurrent = DFBits[32]        <> OUT
  final val pcPlus4   = DFBits[32]        <> OUT
  final val branchSel = DFEnum(BranchSel) <> IN
  final val rs1_data  = DFBits[XLEN]      <> IN
  final val rs2_data  = DFBits[XLEN]      <> IN
  final val imm       = DFBits[32]        <> IN

  private val pc = DFUInt[32] init StartAddress
  private val pcp4 = pc + 4
  pcCurrent := pc.bits
  pcPlus4 := pcp4.bits

  private val pcOrReg1 = DFUInt[32].matchdf(branchSel)
    .casedf(BranchSel.JumpReg)  {rs1_data.uint}
    .casedf_                    {pc}
  private val pcBrJmp = pcOrReg1 + imm.uint

  private val r1s = rs1_data.sint
  private val r2s = rs2_data.sint
  private val r1u = rs1_data.uint
  private val r2u = rs2_data.uint
  private val r1_EQ_r2 = rs1_data == rs2_data
  private val r1_NE_r2 = !r1_EQ_r2
  private val r1_LT_r2 = r1s < r2s
  private val r1_LTU_r2 = r1u < r2u
  private val r1_GE_r2 = !r1_LT_r2
  private val r1_GEU_r2 = !r1_LTU_r2

  private val brTaken = DFBool().matchdf(branchSel)
    .casedf(BranchSel.JumpReg, BranchSel.Jump)  {true}
    .casedf(BranchSel.Equal)                    {r1_EQ_r2}
    .casedf(BranchSel.NotEqual)                 {r1_NE_r2}
    .casedf(BranchSel.GreaterEqual)             {r1_GE_r2}
    .casedf(BranchSel.GreaterEqualUnsigned)     {r1_GEU_r2}
    .casedf(BranchSel.LessThan)                 {r1_LT_r2}
    .casedf(BranchSel.LessThanUnsigned)         {r1_LTU_r2}
    .casedf_                                    {false}

  private val pcNext = DFUInt[32].ifdf(brTaken){pcBrJmp}.elsedf{pcp4}

  pc := pcNext
  
  def getPCConn()(implicit ctx : DFDesign.Context) : DFBits[32] = pcCurrent
  def getPCPlus4Conn()(implicit ctx : DFDesign.Context) : DFBits[32] = pcPlus4
  def updatePC(branchSel : DFEnum[BranchSel], rs1_data : DFBits[XLEN], rs2_data : DFBits[XLEN], imm : DFBits[32])(
    implicit ctx : DFDesign.Context
  ) : Unit = {
    this.branchSel <> branchSel
    this.rs1_data <> rs1_data
    this.rs2_data <> rs2_data
    this.imm <> imm
  }
}
