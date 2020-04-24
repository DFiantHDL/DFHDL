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

import DFiant._

@df class Execute(regFileInst: RegFileInst) extends DFDesign {
  private val instIn    = new RegFileInst <> IN
  final val instOut     = new ExecuteInst <> OUT
  final val mispredict  = DFBit()         <> OUT

  instIn <> instOut

  import instIn.{rs1OpSel, rs2OpSel, rs1_data, rs2_data, imm, pc, aluSel, branchSel}
  import instOut.{pcNext, pcPlus4, aluOut, dataToMem, dmem_addr}
  private val aluOp1 = DFBits[32].matchdf(rs1OpSel)
    .casedf(RS1OpSel.RegSource) {rs1_data}
    .casedf_                    {imm}
  private val aluOp2 = DFBits[32].matchdf(rs2OpSel)
    .casedf(RS2OpSel.RegSource) {rs2_data}
    .casedf(RS2OpSel.PC)        {pc}
    .casedf_                    {imm}

  private val alu = new ALU
  private val aluCalc = alu.calcConn(aluOp1, aluOp2, aluSel)
  private val pcGen = new PCGen(pc, branchSel, rs1_data, rs2_data, imm)
  pcNext <> pcGen.pcNext
  pcPlus4 <> pcGen.pcPlus4
  aluOut <> aluCalc
  dataToMem <> rs2_data
  dmem_addr <> aluCalc
  mispredict <> pcGen.mispredict
//  sim.report(msg"rs1_data: $rs1_data, rs2_data: $rs2_data, imm: $imm, rs1OpSel: $rs1OpSel, aluOp1: $aluOp1, rs2OpSel: $rs2OpSel, aluOp2: $aluOp2, aluSel: $aluSel, aluCalc: $aluCalc")

  atOwnerDo {
    this.instIn <> regFileInst
  }
}

@df class ExecuteInst extends RegFileInst {
  final val dmem_addr = DFBits[32]
  final val dataToMem = DFBits[32]
  final val aluOut    = DFBits[32]
  final val pcNext    = DFBits[32]
  final val pcPlus4   = DFBits[32]
}
