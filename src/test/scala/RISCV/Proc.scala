package RISCV
import DFiant._

trait Proc extends DFDesign {
  private val pc = DFBits[32] init StartAddress
  pc.keep

  private val imem = new IMem(pc)
  private val decoder = new Decoder(imem.inst)
  private val regFile = new RegFile(decoder.inst)
  private val execute = new Execute(regFile.inst)
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Memory
  ////////////////////////////////////////////////////////////////////////
  private val dmem = new DMem {}
  private val dmem_dataFromMem = dmem.readWriteConn(execute.inst.dmem_addr, execute.inst.dataToMem, execute.inst.dmemSel)
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Write Back
  ////////////////////////////////////////////////////////////////////////
  private val wbData = DFBits[32].matchdf(execute.inst.wbSel)
    .casedf(WriteBackSel.ALU)     {execute.inst.aluOut}
    .casedf(WriteBackSel.PCPlus4) {execute.inst.pcPlus4}
    .casedf_                      {dmem_dataFromMem}

  regFile.writeConn(decoder.inst.rd_addr, wbData, decoder.inst.rd_wren)
  ////////////////////////////////////////////////////////////////////////

  pc := execute.inst.pcNext
}

object ProcTest extends App {
  val riscv = new Proc {}.compileToVHDL.print().toFile("test.vhd")
}