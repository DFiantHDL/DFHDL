package RISCV2

import DFiant._

trait PCGen extends DFDesign {
  private val pc        = DFBits[32]        <> IN
  private val branchSel = DFEnum(BranchSel) <> IN
  private val rs1_data  = DFBits[XLEN]      <> IN
  private val rs2_data  = DFBits[XLEN]      <> IN
  private val imm       = DFBits[32]        <> IN
  private val pcNext    = DFBits[32]        <> OUT
  private val pcPlus4   = DFBits[32]        <> OUT

  private val pcu = pc.uint
  private val pcPlus4U = pcu + 4
  pcPlus4 := pcPlus4U.bits

  private val pcOrReg1 = DFUInt[32].matchdf(branchSel)
    .casedf(BranchSel.JALR)  {rs1_data.uint}
    .casedf_                    {pcu}
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
    .casedf(BranchSel.JALR, BranchSel.JAL)  {true}
    .casedf(BranchSel.BEQ)                  {r1_EQ_r2}
    .casedf(BranchSel.BNE)                  {r1_NE_r2}
    .casedf(BranchSel.BGE)                  {r1_GE_r2}
    .casedf(BranchSel.BGEU)                 {r1_GEU_r2}
    .casedf(BranchSel.BLT)                  {r1_LT_r2}
    .casedf(BranchSel.BLTU)                 {r1_LTU_r2}
    .casedf_                                {false}

  private val pcNextU = DFUInt[32].ifdf(brTaken){pcBrJmp}.elsedf{pcPlus4U}

  pcNext := pcNextU.bits
  
  def genPCConn(pc : DFBits[32], branchSel : DFEnum[BranchSel], rs1_data : DFBits[XLEN], rs2_data : DFBits[XLEN], imm : DFBits[32])(
    implicit ctx : DFDesign.Context
  ) : PCCalc = {
    this.pc <> pc
    this.branchSel <> branchSel
    this.rs1_data <> rs1_data
    this.rs2_data <> rs2_data
    this.imm <> imm
    new PCCalc(this.pcNext, this.pcPlus4)
  }
}

class PCCalc (
  val pcNext    : DFBits[32],
  val pcPlus4   : DFBits[32]
)