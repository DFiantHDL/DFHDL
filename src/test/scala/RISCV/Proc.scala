package RISCV
import DFiant._

trait Proc extends DFDesign {
  private val pc = DFBits[32] init StartAddress
  pc.keep

  private val imem = new IMem(pc)
  private val decoder = new Decoder(imem.inst)
  private val regFile = new RegFile(decoder.inst)
  private val execute = new Execute(regFile.inst)
  private val dmem = new DMem(execute.inst)

  ////////////////////////////////////////////////////////////////////////
  // Write Back
  ////////////////////////////////////////////////////////////////////////
  private val wbData = DFBits[32].matchdf(dmem.inst.wbSel)
    .casedf(WriteBackSel.ALU)     {dmem.inst.aluOut}
    .casedf(WriteBackSel.PCPlus4) {dmem.inst.pcPlus4}
    .casedf_                      {dmem.inst.dataFromMem}

  regFile.writeConn(dmem.inst.rd_addr, wbData, dmem.inst.rd_wren)
  ////////////////////////////////////////////////////////////////////////

  pc := execute.inst.pcNext
}

object ProcTest extends App {
  val riscv = new Proc {}.compileToVHDL.print().toFile("test.vhd")
}