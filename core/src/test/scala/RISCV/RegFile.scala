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

import DFiant.internals.LateConstructionConfig
import ZFiant._

//class RegFile(decodedInst : DecodedInst)(implicit ctx : ContextOf[RegFile]) extends DFDesign {
abstract class RegFile()(implicit ctx : ContextOf[RegFile]) extends DFDesign {
  private val rs1_addr  = DFBits[5]      <> IN
  private val rs1_data  = DFBits[XLEN]   <> OUT
  private val rs2_addr  = DFBits[5]      <> IN
  private val rs2_data  = DFBits[XLEN]   <> OUT
  private val rd_addr   = DFBits[5]      <> IN
  private val rd_data   = DFBits[XLEN]   <> IN
  private val rd_wren   = DFBit()        <> IN

  private val regs = List.tabulate(32)(ri => DFBits[XLEN].init(b0s).setName(s"x$ri"))
  private val regsWithIndex = regs.zipWithIndex
  regsWithIndex.foreachdf(rs1_addr) {case (r, ri) => rs1_data := r}
  regsWithIndex.foreachdf(rs2_addr) {case (r, ri) => rs2_data := r}

//  sim.report(msg"RFile~~>rs1_addr: $rs1_addr, rs1_data: $rs1_data, rs2_addr: $rs2_addr, rs2_data: $rs2_data, rd_addr: $rd_addr, rd_data: $rd_data, rd_wren: $rd_wren")
//  sim.report(msg"rd_addr: $rd_addr, rd_data: $rd_data, rd_wren: $rd_wren")

  regsWithIndex.foreachdf(rd_addr) {
    case (r, 0) => //No write for X0
    case (r, ri) =>
      ifdf (rd_wren) {
        r := rd_data
      }
  }


//
//  final val inst = {
//    import decodedInst._
//    RegFileInst(
//      //IMem
//      pc = pc, instRaw = instRaw,
//      //Decoder
//      rs1_addr = decodedInst.rs1_addr, rs2_addr = decodedInst.rs2_addr, rd_addr = decodedInst.rd_addr, rd_wren = decodedInst.rd_wren,
//      imm = imm, branchSel = branchSel, rs1OpSel = rs1OpSel, rs2OpSel = rs2OpSel,
//      aluSel = aluSel, wbSel = wbSel, dmemSel = dmemSel, debugOp = debugOp,
//      //RegFile
//      rs1_data = rs1_data, rs2_data = rs2_data
//    )
//  }
//
//  atOwnerDo {
//    this.rs1_addr <> decodedInst.rs1_addr
//    this.rs2_addr <> decodedInst.rs2_addr
//  }
//
//  def writeBack(dmemInst : DMemInst) : Unit = atOwnerDo {
//    val wbData = DFBits[32].matchdf(dmemInst.wbSel)
//      .casedf(WriteBackSel.ALU)     {dmemInst.aluOut}
//      .casedf(WriteBackSel.PCPlus4) {dmemInst.pcPlus4}
//      .casedf_                      {dmemInst.dataFromMem}
//
//    this.rd_addr <> dmemInst.rd_addr
//    this.rd_data <> wbData
//    this.rd_wren <> dmemInst.rd_wren
//  }
}


case class RegFileInst(
  //IMem
  pc        : DFBits[32],
  instRaw   : DFBits[32],

  //Decoder
  rs1_addr  : DFBits[5],
  rs2_addr  : DFBits[5],
  rd_addr   : DFBits[5],
  rd_wren   : DFBit,
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
  rs2_data  : DFBits[XLEN]
)

object RegFileApp extends App {
  val dec = new RegFile() {}
  import compiler.backend.vhdl._
  dec.compile.printCodeString().printGenFiles().toFolder("testProc")
}