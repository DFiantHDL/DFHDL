package RISCV

import DFiant._

/*
create_ip -name blk_mem_gen -vendor xilinx.com -library ip -version 8.4 -module_name dmem_bram
set_property -dict [list CONFIG.Component_Name {dmem_bram} CONFIG.Use_Byte_Write_Enable {true} CONFIG.Byte_Size {8} CONFIG.Write_Width_A {32} CONFIG.Write_Depth_A {4096} CONFIG.Read_Width_A {32} CONFIG.Enable_A {Always_Enabled} CONFIG.Write_Width_B {32} CONFIG.Read_Width_B {32} CONFIG.Register_PortA_Output_of_Memory_Primitives {false}] [get_ips dmem_bram]
 */
trait DMem_Bram_Ifc extends DFInterface {
  final val wea   = DFBits[4] <> IN
  final val addra = DFBits[12] <> IN
  final val dina  = DFBits[32] <> IN
  final val douta = DFBits[32] <> OUT
}

class DMem_Bram_Sim(programDMem : ProgramDMem)(implicit ctx : DFDesign.ContextOf[DMem_Bram_Sim]) extends DFDesign with DMem_Bram_Ifc {
  private val cells = (0 until 256).map(ci => DFBits[32].setName(s"cell$ci"))
  cells.foreachdf(addra(7, 0)) {
    case cell =>
      douta := cell
      ifdf (wea(0)) {cell( 7,  0) := dina( 7,  0)}
      ifdf (wea(1)) {cell(15,  8) := dina(15,  8)}
      ifdf (wea(2)) {cell(23, 16) := dina(23, 16)}
      ifdf (wea(3)) {cell(31, 24) := dina(31, 24)}
  }
}

class DMem_Bram(programDMem : ProgramDMem)(implicit ctx : RTComponent.Context) extends RTComponent with DMem_Bram_Ifc {
  final val clka  = Clock()
  //  setInitFunc(S)(LazyBox.Args2(this)(DFUInt.Token.+, getInit(A), getInit(B)))
}

class DMem(programDMem : ProgramDMem)(executeInst : ExecuteInst)(implicit ctx : DFDesign.ContextOf[DMem]) extends DFDesign {
  private val addr        = DFBits[32] <> IN
  private val dataToMem   = DFBits[32] <> IN
  private val dmemSel     = DFEnum[DMemSel] <> IN
  private val dataFromMem = DFBits[32] <> OUT
  private val wrEnToMem   = DFBits[4]
  private val dataToMemBH = DFBits[32] //Data to memory modified for byte and half-word writes

  private val bram = if (inSimulation) new DMem_Bram_Sim(programDMem) else new DMem_Bram(programDMem)

  wrEnToMem := b"0000"
  dataToMemBH := dataToMem
  dataFromMem := bram.douta
  matchdf(dmemSel)
    .casedf(DMemSel.LB) {
      matchdf(addr(1, 0))
        .casedf(b"00")    {dataFromMem := bram.douta( 7,  0).sint.extendTo(32).bits}
        .casedf(b"01")    {dataFromMem := bram.douta(15,  8).sint.extendTo(32).bits}
        .casedf(b"10")    {dataFromMem := bram.douta(23, 16).sint.extendTo(32).bits}
        .casedf(b"11")    {dataFromMem := bram.douta(31, 24).sint.extendTo(32).bits}
    }
    .casedf(DMemSel.LH) {
      matchdf(addr(1, 1))
        .casedf(b"0")     {dataFromMem := bram.douta(15,  0).sint.extendTo(32).bits}
        .casedf(b"1")     {dataFromMem := bram.douta(31, 16).sint.extendTo(32).bits}
    }
    .casedf(DMemSel.LW)   {dataFromMem := bram.douta}
    .casedf(DMemSel.LBU) {
      matchdf(addr(1, 0))
        .casedf(b"00")    {dataFromMem := bram.douta( 7,  0).extendLeftTo(32)}
        .casedf(b"01")    {dataFromMem := bram.douta(15,  8).extendLeftTo(32)}
        .casedf(b"10")    {dataFromMem := bram.douta(23, 16).extendLeftTo(32)}
        .casedf(b"11")    {dataFromMem := bram.douta(31, 24).extendLeftTo(32)}
    }
    .casedf(DMemSel.LHU) {
      matchdf(addr(1, 1))
        .casedf(b"0")    {dataFromMem := bram.douta(15,  0).extendLeftTo(32)}
        .casedf(b"1")    {dataFromMem := bram.douta(31, 16).extendLeftTo(32)}
    }
    .casedf(DMemSel.SB) {
      dataToMemBH := (dataToMem(7,0), dataToMem(7,0), dataToMem(7,0), dataToMem(7,0)).bits
      matchdf(addr(1, 0))
        .casedf(b"00")    {wrEnToMem := b"0001"}
        .casedf(b"01")    {wrEnToMem := b"0010"}
        .casedf(b"10")    {wrEnToMem := b"0100"}
        .casedf(b"11")    {wrEnToMem := b"1000"}
    }
    .casedf(DMemSel.SH) {
      dataToMemBH := dataToMem(15,0) ## dataToMem(15,0)
      matchdf(addr(1, 1))
        .casedf(b"0")     {wrEnToMem := b"0011"}
        .casedf(b"1")     {wrEnToMem := b"1100"}
    }
    .casedf(DMemSel.SW)   {wrEnToMem := b"1111"}

  bram.addra <> addr(13, 2)
  bram.wea <> wrEnToMem
  bram.dina <> dataToMemBH

//  sim.report(msg"DMem~~~>addr: $addr, dmemSel: $dmemSel, dataToMem: $dataToMem, dataToMemBH: $dataToMemBH, wrEnToMem: $wrEnToMem, dataFromMem: $dataFromMem, bram.douta: ${bram.douta}")

  final val inst = {
    import executeInst._
    DMemInst(
      //IMem
      pc = pc, instRaw = instRaw,
      //Decoder
      rs1_addr = rs1_addr, rs2_addr = rs2_addr, rd_addr = rd_addr, rd_wren = rd_wren,
      imm = imm, shamt = shamt, branchSel = branchSel,
      rs1OpSel = rs1OpSel, rs2OpSel = rs2OpSel,
      aluSel = aluSel, wbSel = wbSel, dmemSel = executeInst.dmemSel, debugOp = debugOp,
      //RegFile
      rs1_data = rs1_data, rs2_data = rs2_data,
      //Execute
      dmem_addr = executeInst.dmem_addr, dataToMem = executeInst.dataToMem, aluOut = aluOut,
      pcNext = pcNext, pcPlus4 = pcPlus4,
      //DMem
      dataFromMem = dataFromMem
    )
  }

  atOwnerDo {
    this.addr <> executeInst.dmem_addr
    this.dataToMem <> executeInst.dataToMem
    this.dmemSel <> executeInst.dmemSel
  }
}


case class DMemInst(
  //IMem
  pc          : DFBits[32],
  instRaw     : DFBits[32],

  //Decoder
  rs1_addr    : DFBits[5],
  rs2_addr    : DFBits[5],
  rd_addr     : DFBits[5],
  rd_wren     : DFBool,
  imm         : DFBits[32],
  shamt       : DFUInt[5],
  branchSel   : DFEnum[BranchSel],
  rs1OpSel    : DFEnum[RS1OpSel],
  rs2OpSel    : DFEnum[RS2OpSel],
  aluSel      : DFEnum[ALUSel],
  wbSel       : DFEnum[WriteBackSel],
  dmemSel     : DFEnum[DMemSel],
  debugOp     : DFEnum[DebugOp],

  //RegFile
  rs1_data    : DFBits[XLEN],
  rs2_data    : DFBits[XLEN],

  //Execute
  dmem_addr   : DFBits[32],
  dataToMem   : DFBits[32],
  aluOut      : DFBits[32],
  pcNext      : DFBits[32],
  pcPlus4     : DFBits[32],

  //DMem
  dataFromMem : DFBits[32]
)
