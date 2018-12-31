package RISCV
import DFiant._
import internals.BitVectorExtras

/* TCL example
create_ip -name blk_mem_gen -vendor xilinx.com -library ip -version 8.4 -module_name imem_bram
set_property -dict [list CONFIG.Component_Name {imem_bram} CONFIG.Memory_Type {Single_Port_ROM} CONFIG.Write_Width_A {32} CONFIG.Write_Depth_A {4096} CONFIG.Read_Width_A {32} CONFIG.Enable_A {Always_Enabled} CONFIG.Write_Width_B {32} CONFIG.Read_Width_B {32} CONFIG.Register_PortA_Output_of_Memory_Primitives {false} CONFIG.Load_Init_File {true} CONFIG.Coe_File {/home/soronpo/XilinxProjects/ZedBoardEst/imem.coe} CONFIG.Fill_Remaining_Memory_Locations {true} CONFIG.Port_A_Write_Rate {0}] [get_ips imem_bram]
 */

class IMem_Bram(programMem : ProgramMem)(implicit ctx : RTComponent.Context) extends RTComponent {
  final val clka = Clock()
  final val addra = DFBits[12] <> IN
  final val douta = DFBits[32] <> OUT
  //  setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(A), getInit(B)))
  //need to generate COE file
}

class IMem_Bram_Sim(programMem : ProgramMem)(implicit ctx : DFDesign.ContextOf[IMem_Bram_Sim]) extends DFDesign {
  final val addra = DFBits[12] <> IN
  final val douta = DFBits[32] <> OUT

  douta := b0s
  programMem.list.map(e => (e.addr.bits(13, 2), e.inst)).matchdf(addra, douta)
}

class IMem(programMem : ProgramMem)(incomingPC : DFBits[32])(implicit ctx : DFDesign.ContextOf[IMem]) extends DFDesign {
  private val pc      = DFBits[32] <> IN
  private val instRaw = DFBits[32] <> OUT

  if (inSimulation) {
    val bram_sim = new IMem_Bram_Sim(programMem)
    bram_sim.addra <> pc(13, 2)
    bram_sim.douta <> instRaw
  } else {
    val bram = new IMem_Bram(programMem)
    bram.addra <> pc(13, 2)
    bram.douta <> instRaw
  }

  final val inst = IMemInst(pc = incomingPC, instRaw = instRaw)

  atOwnerDo {
    pc <> incomingPC
  }
}

case class IMemInst(
  pc      : DFBits[32],
  instRaw : DFBits[32]
)



