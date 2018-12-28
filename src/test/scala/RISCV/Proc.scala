package RISCV
import DFiant._

trait Proc extends DFDesign {
  private val pc = DFBits[32] init StartAddress
  pc.keep

  private val imem = new IMem(pc)
  private val decoder = new Decoder(imem.inst)
  private val regFile = new RegFile(decoder.inst)
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // ALU (Execute)
  ////////////////////////////////////////////////////////////////////////
  private val execute = new Execute {}
  private val (pcCalc, executeInst) = execute.exConn(pc, decoder.inst, regFile.inst.rs1_data, regFile.inst.rs2_data)
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Memory
  ////////////////////////////////////////////////////////////////////////
  private val dmem = new DMem {}
  private val dmem_dataFromMem = dmem.readWriteConn(executeInst.dmem_addr, executeInst.dataToMem, executeInst.dmemSel)
  ////////////////////////////////////////////////////////////////////////

  ////////////////////////////////////////////////////////////////////////
  // Write Back
  ////////////////////////////////////////////////////////////////////////
  private val wbData = DFBits[32].matchdf(executeInst.wbSel)
    .casedf(WriteBackSel.ALU)     {executeInst.aluOut}
    .casedf(WriteBackSel.PCPlus4) {pcCalc.pcPlus4}
    .casedf_                      {dmem_dataFromMem}

  regFile.writeConn(decoder.inst.rd_addr, wbData, decoder.inst.rd_wren)
  ////////////////////////////////////////////////////////////////////////

  pc := pcCalc.pcNext
}

object ProcTest extends App {
  val riscv = new Proc {}.compileToVHDL.print().toFile("test.vhd")
}