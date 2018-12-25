//package RISCV2
//
//import DFiant._
//
//trait Proc extends DFDesign {
//  private val pc = DFBits[32] init StartAddress
//  private val sb = new ScoreBoard(1) //<-------
//  private val fEpoch = DFBool() init false
//  private val eEpoch = DFBool() init false
//  ////////////////////////////////////////////////////////////////////////
//  // Fetch
//  ////////////////////////////////////////////////////////////////////////
//  private val imem = new IMem {}
//  private val inst = imem.readConn(pc)
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Decode
//  ////////////////////////////////////////////////////////////////////////
//  private val decoder = new Decoder {}
//  private val decodedInst0 = decoder.decodeConn(inst).pipe //<-------(also pipelines PC, fEpoch)
//  private val stallFetchDecode = sb.searchConn(decodedInst0.rs1_addr, decodedInst0.rs2_addr)
//
//  //Optionally stalled decoded instruction
//  private val decodedInst = decodedInst0.copy(
//    rd_wren = DFBool().selectdf(stallFetchDecode)(false, decodedInst0.rd_wren),
//    dmemSel = DFEnum(DMemSel).selectdf(stallFetchDecode)(DMemSel.DontWrite, decodedInst0.dmemSel)
//  )
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Register File (Read)
//  ////////////////////////////////////////////////////////////////////////
//  private val regFile = new RegFile {}
//  private val rs1_data = regFile.readConn1(decodedInst.rs1_addr)
//  private val rs2_data = regFile.readConn2(decodedInst.rs2_addr)
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // ALU (Execute)
//  ////////////////////////////////////////////////////////////////////////
//  private val execute = new Execute {}
//  private val (pcCalc, executeInst0) = execute.exConn(pc, decodedInst, rs1_data, rs2_data)
//  private val stallEx = eEpoch.prev != fEpoch.prev
//  private val executeInst = executeInst0.copy(
//    rd_wren = DFBool().selectdf(stallEx)(false, executeInst0.rd_wren),
//    dmemSel = DFEnum(DMemSel).selectdf(stallEx)(DMemSel.DontWrite, executeInst0.dmemSel)
//  )
//  ifdf(!stallEx && pcCalc.brTaken) {
//    eEpoch := !eEpoch
//  }
//  sb.insertConn(executeInst.rd_addr, executeInst.rd_wren)
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Memory
//  ////////////////////////////////////////////////////////////////////////
//  private val dmem = new DMem {}
//  private val dmem_dataFromMem = dmem.readWriteConn(executeInst.dmem_addr, executeInst.dataToMem, executeInst.dmemSel)
//  ////////////////////////////////////////////////////////////////////////
//
//  ////////////////////////////////////////////////////////////////////////
//  // Write Back
//  ////////////////////////////////////////////////////////////////////////
//  private val wbData = DFBits[32].matchdf(executeInst.wbSel)
//    .casedf(WriteBackSel.ALU)     {executeInst.aluOut}
//    .casedf(WriteBackSel.PCPlus4) {pcCalc.pcPlus4}
//    .casedf_                      {dmem_dataFromMem}
//
//  regFile.writeConn(decodedInst.rd_addr, wbData, decodedInst.rd_wren)
//  ////////////////////////////////////////////////////////////////////////
//
//  ifdf(!stallFetchDecode && !stallEx) {
//    fEpoch := eEpoch.prev.pipeBreak //<---------Pipe break to bypass pipeline loop in feedback
//    pc := pcCalc.pcNext.pipeBreak //<---------
//  }
//}
//
//object ProcTest extends App {
//  val riscv = new Proc {}.compileToVHDL.print().toFile("test.vhd")
//}