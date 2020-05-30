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
import DFiant.internals.BitVectorExtras
import sim._
/* TCL example
create_ip -name blk_mem_gen -vendor xilinx.com -library ip -version 8.4 -module_name imem_bram
set_property -dict [list CONFIG.Component_Name {imem_bram} CONFIG.Memory_Type {Single_Port_ROM} CONFIG.Write_Width_A {32} CONFIG.Write_Depth_A {4096} CONFIG.Read_Width_A {32} CONFIG.Enable_A {Always_Enabled} CONFIG.Write_Width_B {32} CONFIG.Read_Width_B {32} CONFIG.Register_PortA_Output_of_Memory_Primitives {false} CONFIG.Load_Init_File {true} CONFIG.Coe_File {/home/soronpo/XilinxProjects/ZedBoardEst/imem.coe} CONFIG.Fill_Remaining_Memory_Locations {true} CONFIG.Port_A_Write_Rate {0}] [get_ips imem_bram]
 */

trait IMem_Bram_Ifc extends DFDesign.Abstract {
  final val addra = DFBits[12] <> IN
  final val douta = DFBits[32] <> OUT
}

@df class IMem_Bram(programIMem : ProgramIMem) extends DFDesign with IMem_Bram_Ifc {
  final val clka = DFBit() //!! compiler.sync.Sync.Tag.Clk
  //need to generate COE file
}

@df class IMem_Bram_Sim(programIMem : ProgramIMem) extends DFDesign with IMem_Bram_Ifc {
  private val temp = DFBits[32] init b0s
  temp := b0s
  programIMem.list.map(e => (e.addr.bits(13, 2), e.inst)).matchdf(addra, temp)
  douta := temp
}

@df class IMem(programIMem : ProgramIMem)(incomingPC : DFBits[32]) extends DFDesign {
  private val pc     = DFBits[32] <> IN
  final val instOut  = new IMemInst <> OUT


  private val bram = if (inSimulation || caseIMem) new IMem_Bram_Sim(programIMem) else new IMem_Bram(programIMem)

  bram.addra <> pc(13, 2)
  bram.douta <> instOut.raw
  pc <> instOut.pc


  atOwnerDo {
    pc <> incomingPC
  }
}

@df class IMemInst extends DFInterface {
  final val pc   = DFBits[32]
  final val raw  = DFBits[32]
}


