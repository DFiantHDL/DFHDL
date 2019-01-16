package RISCV

import DFiant._

class PCGen(pc0 : DFBits[32], branchSel0 : DFEnum[BranchSel], rs1_data0 : DFBits[XLEN], rs2_data0 : DFBits[XLEN],
  imm0 : DFBits[32])(implicit ctx : DFDesign.ContextOf[PCGen]) extends DFDesign {
  private val pc          = DFBits[32]        <> IN
  private val branchSel   = DFEnum(BranchSel) <> IN
  private val rs1_data    = DFBits[XLEN]      <> IN
  private val rs2_data    = DFBits[XLEN]      <> IN
  private val imm         = DFBits[32]        <> IN
  final val   pcNext      = DFBits[32]        <> OUT
  final val   pcPlus4     = DFBits[32]        <> OUT
  final val   mispredict  = DFBool()          <> OUT init true

  private val pcu = pc.uint

  private val pcPlus4U = pcu + 4
  private val prevPCPlus4U = pcPlus4U.prev()
  private val prevPCU = pcu.prev
  private val pcuSel = microArchitecture match {
    case OneCycle => pcu
    case TwoCycle => prevPCU
  }
  pcPlus4 := pcPlus4U.bits

  private val pcOrReg1 = DFUInt[32].matchdf(branchSel)
    .casedf(BranchSel.JALR)  {rs1_data.uint}
    .casedf_                    {pcuSel}
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

  private val pcNextU = microArchitecture match {
    case OneCycle =>
      DFUInt[32].ifdf(brTaken){pcBrJmp}.elsedf{pcPlus4U}
    case TwoCycle =>
      val actualPC = DFUInt[32].ifdf(brTaken){pcBrJmp}.elsedf{prevPCPlus4U}
      val predictedPC = pcPlus4U
      mispredict := predictedPC.prev != actualPC
      ifdf (mispredict.prev) {mispredict := false}
      DFUInt[32].ifdf(mispredict){actualPC}.elsedf{predictedPC}
  }

  pcNext := pcNextU.bits

//  sim.report(msg"PCGen~~>$branchSel, pcNext:$pcNext, imm: $imm, pcOrReg1: $pcOrReg1, pcBrJmp: $pcBrJmp, brTaken: $brTaken, rs1_data: $rs1_data, rs2_data: $rs2_data")

  final val pcCalc = new PCCalc(this.pcNext, this.pcPlus4)

  atOwnerDo {
    this.pc <> pc0
    this.branchSel <> branchSel0
    this.rs1_data <> rs1_data0
    this.rs2_data <> rs2_data0
    this.imm <> imm0
  }
}

class PCCalc (
  val pcNext    : DFBits[32],
  val pcPlus4   : DFBits[32]
)