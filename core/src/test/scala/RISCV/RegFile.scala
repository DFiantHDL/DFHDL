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

@df class RegFile(decodedInst : DecodedInst) extends DFDesign {
  private val instIn = new DecodedInst <> IN
  final val instOut  = new RegFileInst <> OUT
  final val wb       = new RegFileWB   <> IN

  instIn <> instOut

  import instIn.{rs1_addr, rs2_addr}
  import instOut.{rs1_data, rs2_data}
  import wb.{rd_addr, rd_wren, rd_data}

  private val regs = List.tabulate(32)(ri => DFBits[XLEN].init(b0s).setName(s"x$ri"))
  private val regsWithIndex = regs.zipWithIndex
  regsWithIndex.foreachdf(rs1_addr) {case (r, _) => rs1_data := r}
  regsWithIndex.foreachdf(rs2_addr) {case (r, _) => rs2_data := r}

//  sim.report(msg"RFile++>rs1_addr: $rs1_addr, rs1_data: $rs1_data, rs2_addr: $rs2_addr, rs2_data: $rs2_data, rd_addr: $rd_addr, rd_data: $rd_data, rd_wren: $rd_wren")
//  sim.report(msg"rd_addr: $rd_addr, rd_data: $rd_data, rd_wren: $rd_wren")

  regsWithIndex.foreachdf(rd_addr) {
    case (_, 0) => //No write for X0
    case (r, _) =>
      ifdf (rd_wren) {
        r := rd_data
      }
  }

  atOwnerDo {
    this.instIn <> decodedInst
  }

  def writeBack(dmemInst : DMemInst) : Unit = atOwnerDo {
    val wbData = DFBits[32].matchdf(dmemInst.wbSel)
      .casedf(WriteBackSel.ALU)     {dmemInst.aluOut}
      .casedf(WriteBackSel.PCPlus4) {dmemInst.pcPlus4}
      .casedf_                      {dmemInst.dataFromMem}

    rd_addr <> dmemInst.rd_addr
    rd_data <> wbData
    rd_wren <> dmemInst.rd_wren
  }
}


@df class RegFileInst extends DecodedInst {
  final val rs1_data = DFBits[XLEN]
  final val rs2_data = DFBits[XLEN]
}
@df class RegFileWB extends DFInterface {
  final val rd_addr   = DFBits[5]
  final val rd_data   = DFBits[XLEN]
  final val rd_wren   = DFBit()
}
