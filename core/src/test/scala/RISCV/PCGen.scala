/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package RISCV

import ZFiant._

class PCGen(pc0 : DFBits[32], branchSel0 : DFEnum[BranchSel], rs1_data0 : DFBits[XLEN], rs2_data0 : DFBits[XLEN],
  imm0 : DFBits[32])(implicit ctx : ContextOf[PCGen]) extends DFDesign {
  private val pc          = DFBits[32]        <> IN
  private val branchSel   = DFEnum(BranchSel) <> IN
  private val rs1_data    = DFBits[XLEN]      <> IN
  private val rs2_data    = DFBits[XLEN]      <> IN
  private val imm         = DFBits[32]        <> IN
  final val   pcNext      = DFBits[32]        <> OUT
  final val   pcPlus4     = DFBits[32]        <> OUT
  final val   mispredict  = DFBit()           <> OUT
  mispredict := true
  private val pcu = pc.uint
  private val pcPlus4U = pcu + 4
  private val pcuSel = pcu
  private val pcPlus4B = pcPlus4U.bits
  private val pcPlus4Sel = pcPlus4B

  pcPlus4 := pcPlus4Sel

  private val pcOrReg1 = DFUInt[32].matchdf(branchSel)
    .casedf(BranchSel.JALR)  {rs1_data.uint}
    .casedf_                 {pcuSel}
  private val pcBrJmp = pcOrReg1 + imm.uint

  private val r1s = rs1_data.sint
  private val r2s = rs2_data.sint
  private val r1u = rs1_data.uint
  private val r2u = rs2_data.uint
  private val r1_EQ_r2 = rs1_data === rs2_data
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

//  sim.report(msg"PCGen++>$branchSel, pcNext:$pcNext, imm: $imm, pcOrReg1: $pcOrReg1, pcBrJmp: $pcBrJmp, brTaken: $brTaken, rs1_data: $rs1_data, rs2_data: $rs2_data")

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