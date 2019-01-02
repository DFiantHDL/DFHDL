package RISCV
import DFiant._
import internals.BitVectorExtras

/* TCL example
create_ip -name blk_mem_gen -vendor xilinx.com -library ip -version 8.4 -module_name imem_bram
set_property -dict [list CONFIG.Component_Name {imem_bram} CONFIG.Memory_Type {Single_Port_ROM} CONFIG.Write_Width_A {32} CONFIG.Write_Depth_A {4096} CONFIG.Read_Width_A {32} CONFIG.Enable_A {Always_Enabled} CONFIG.Write_Width_B {32} CONFIG.Read_Width_B {32} CONFIG.Register_PortA_Output_of_Memory_Primitives {false} CONFIG.Load_Init_File {true} CONFIG.Coe_File {/home/soronpo/XilinxProjects/ZedBoardEst/imem.coe} CONFIG.Fill_Remaining_Memory_Locations {true} CONFIG.Port_A_Write_Rate {0}] [get_ips imem_bram]
 */

trait IMem_Bram_Ifc extends DFInterface {
  final val addra = DFBits[12] <> IN
  final val douta = DFBits[32] <> OUT
}

class IMem_Bram(programIMem : ProgramIMem)(implicit ctx : RTComponent.Context) extends RTComponent with IMem_Bram_Ifc {
  final val clka = Clock()
  //  setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(A), getInit(B)))
  //need to generate COE file
}

class IMem_Bram_Sim(programIMem : ProgramIMem)(implicit ctx : DFDesign.ContextOf[IMem_Bram_Sim]) extends DFDesign with IMem_Bram_Ifc {
  douta := b0s
  programIMem.list.map(e => (e.addr.bits(13, 2), e.inst)).matchdf(addra, douta)
}

class IMem(programIMem : ProgramIMem)(incomingPC : DFBits[32])(implicit ctx : DFDesign.ContextOf[IMem]) extends DFDesign {
  private val pc      = DFBits[32] <> IN
  private val instRaw = DFBits[32] <> OUT

  private val bram = if (inSimulation) new IMem_Bram_Sim(programIMem) else new IMem_Bram(programIMem)

  bram.addra <> pc(13, 2)
  bram.douta <> instRaw

  programIMem.failAddress match {
    case Some(failPC) => ifdf(pc == failPC){
      sim.report(msg"Test failed")
      sim.finish()
    }
    case None =>
  }
  ifdf (pc == programIMem.finishAddress) {
    sim.report(msg"Program execution finished")
    sim.finish()
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



