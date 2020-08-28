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
import sim._
/*
create_ip -name blk_mem_gen -vendor xilinx.com -library ip -version 8.4 -module_name dmem_bram
set_property -dict [list CONFIG.Component_Name {dmem_bram} CONFIG.Use_Byte_Write_Enable {true} CONFIG.Byte_Size {8} CONFIG.Write_Width_A {32} CONFIG.Write_Depth_A {4096} CONFIG.Read_Width_A {32} CONFIG.Enable_A {Always_Enabled} CONFIG.Write_Width_B {32} CONFIG.Read_Width_B {32} CONFIG.Register_PortA_Output_of_Memory_Primitives {false}] [get_ips dmem_bram]
 */
trait DMem_Bram_Ifc extends DFDesign.Abstract {
  final val wea   = DFBits[4] <> IN
  final val addra = DFBits[12] <> IN
  final val dina  = DFBits[32] <> IN
  final val douta = DFBits[32] <> OUT
}

@df class DMem_Bram_Sim(programDMem : ProgramDMem) extends DFDesign with DMem_Bram_Ifc {
  private val cellNum = 256
  private val cellRange = 0 until cellNum
  private val initArr = programDMem.toInitArr(cellNum)
  private val cells = cellRange.map(ci => DFBits[32].init(initArr(ci)).setName(s"cell$ci"))
  private val subAddr = addra(7, 0)
  cells.foreachdf(subAddr) {
    case cell =>
      douta := cell
      ifdf (wea(0)) {cell( 7,  0) := dina( 7,  0)}
      ifdf (wea(1)) {cell(15,  8) := dina(15,  8)}
      ifdf (wea(2)) {cell(23, 16) := dina(23, 16)}
      ifdf (wea(3)) {cell(31, 24) := dina(31, 24)}
  }
}


@df class DMem_Bram(programDMem : ProgramDMem) extends DFDesign with DMem_Bram_Ifc {
  final val clka  = DFBit() //!! compiler.sync.RTL.Tag.Clk
}

@df class DMem(programDMem : ProgramDMem)(executeInst : ExecuteInst) extends DFDesign {
  private val instIn      = new ExecuteInst <> IN
  final val instOut       = new DMemInst <> OUT
  private val wrEnToMem   = DFBits[4]
  private val dataToMemBH = DFBits[32] //Data to memory modified for byte and half-word writes

  instIn <> instOut

  import instIn.{dataToMem, dmemSel, dmem_addr}
  import instOut.dataFromMem
  private val bram = if (inSimulation || caseDMem) new DMem_Bram_Sim(programDMem) else new DMem_Bram(programDMem)

  wrEnToMem := b"0000"
  dataToMemBH := dataToMem
  dataFromMem := bram.douta
  val byteSel = dmem_addr(1, 0)
  val wordSel = dmem_addr(1, 1)
  matchdf(dmemSel)
    .casedf(DMemSel.LB) {
      matchdf(byteSel)
        .casedf(b"00")    {dataFromMem := bram.douta( 7,  0).sint.resize(32).bits}
        .casedf(b"01")    {dataFromMem := bram.douta(15,  8).sint.resize(32).bits}
        .casedf(b"10")    {dataFromMem := bram.douta(23, 16).sint.resize(32).bits}
        .casedf(b"11")    {dataFromMem := bram.douta(31, 24).sint.resize(32).bits}
        .casedf_{}
    }
    .casedf(DMemSel.LH) {
      matchdf(wordSel)
        .casedf(b"0")     {dataFromMem := bram.douta(15,  0).sint.resize(32).bits}
        .casedf(b"1")     {dataFromMem := bram.douta(31, 16).sint.resize(32).bits}
        .casedf_{}
    }
    .casedf(DMemSel.LW)   {dataFromMem := bram.douta}
    .casedf(DMemSel.LBU) {
      matchdf(byteSel)
        .casedf(b"00")    {dataFromMem := bram.douta( 7,  0).resize(32)}
        .casedf(b"01")    {dataFromMem := bram.douta(15,  8).resize(32)}
        .casedf(b"10")    {dataFromMem := bram.douta(23, 16).resize(32)}
        .casedf(b"11")    {dataFromMem := bram.douta(31, 24).resize(32)}
        .casedf_{}
    }
    .casedf(DMemSel.LHU) {
      matchdf(wordSel)
        .casedf(b"0")    {dataFromMem := bram.douta(15,  0).resize(32)}
        .casedf(b"1")    {dataFromMem := bram.douta(31, 16).resize(32)}
        .casedf_{}
    }
    .casedf(DMemSel.SB) {
      dataToMemBH := (dataToMem(7,0), dataToMem(7,0), dataToMem(7,0), dataToMem(7,0)).bits
      matchdf(byteSel)
        .casedf(b"00")    {wrEnToMem := b"0001"}
        .casedf(b"01")    {wrEnToMem := b"0010"}
        .casedf(b"10")    {wrEnToMem := b"0100"}
        .casedf(b"11")    {wrEnToMem := b"1000"}
        .casedf_{}
    }
    .casedf(DMemSel.SH) {
      dataToMemBH := dataToMem(15,0) ++ dataToMem(15,0)
      matchdf(wordSel)
        .casedf(b"0")     {wrEnToMem := b"0011"}
        .casedf(b"1")     {wrEnToMem := b"1100"}
        .casedf_{}
    }
    .casedf(DMemSel.SW)   {wrEnToMem := b"1111"}
    .casedf_{}

  bram.addra <> dmem_addr(13, 2)
  bram.wea <> wrEnToMem
  bram.dina <> dataToMemBH

//  sim.report(msg"DMem~~~>addr: $addr, dmemSel: $dmemSel, dataToMem: $dataToMem, dataToMemBH: $dataToMemBH, wrEnToMem: $wrEnToMem, dataFromMem: $dataFromMem, bram.douta: ${bram.douta}")

  atOwnerDo {
    this.instIn <> executeInst
  }
}

@df class DMemInst extends ExecuteInst {
  final val dataFromMem = DFBits[32]
}
