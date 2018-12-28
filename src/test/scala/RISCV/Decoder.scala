package RISCV

import DFiant._

class Decoder(fetchInst : IMemInst)(implicit ctx : DFDesign.ContextOf[Decoder]) extends DFDesign {
  private val instRaw   = DFBits[32]            <> IN

  //Register File Addresses & Control
  private val rs1_addr  = DFBits[5]             <> OUT
  private val rs2_addr  = DFBits[5]             <> OUT
  private val rd_addr   = DFBits[5]             <> OUT
  private val rd_wren   = DFBool()              <> OUT

  //Immediate values for ALU execution
  private val imm       = DFBits[32]            <> OUT
  private val shamt     = DFUInt[5]             <> OUT

  //Control Signals
  private val branchSel = DFEnum(BranchSel)     <> OUT
  private val rs1OpSel  = DFEnum(RS1OpSel)      <> OUT
  private val rs2OpSel  = DFEnum(RS2OpSel)      <> OUT
  private val aluSel    = DFEnum(ALUSel)        <> OUT
  private val wbSel     = DFEnum(WriteBackSel)  <> OUT
  private val dmemSel   = DFEnum(DMemSel)       <> OUT

  private val opcode    = instRaw(6, 0)
  private val func7     = instRaw(31, 25)
  private val func3     = instRaw(14, 12)
  private val immIType  = instRaw(31, 20).sint.extendTo(32).bits
  private val immSType  = (instRaw(31, 25), instRaw(11, 7)).bits.sint.extendTo(32).bits
  private val immBType  = (instRaw(31), instRaw(7), instRaw(30, 25), instRaw(11, 8), b"0").bits.sint.extendTo(32).bits
  private val immUType  = instRaw(31, 12).extendRightTo(32).sint.bits
  private val immJType  = (instRaw(31), instRaw(19, 12), instRaw(20), instRaw(30, 21), b"0").bits.sint.extendTo(32).bits
  rs1_addr := instRaw(19, 15)
  rs2_addr := instRaw(24, 20)
  shamt := instRaw(24, 20).uint
  rd_addr := instRaw(11, 7)

  val op = DFEnum(Op)
  op := Op.Unsupported //Default op is not supported unless selected otherwise
  imm := immIType //Default immediate is IType unless selected otherwise
  branchSel := BranchSel.Next
  rs1OpSel := RS1OpSel.DontCare
  rs2OpSel := RS2OpSel.DontCare
  aluSel := ALUSel.DontCare
  wbSel := WriteBackSel.DontCare
  dmemSel := DMemSel.DontCare

  matchdf(opcode)
    //////////////////////////////////////////////
    // LUI
    //////////////////////////////////////////////
    .casedf(b"0110111") {
      imm := immUType
      rs1OpSel := RS1OpSel.Immediate
      rs2OpSel := RS2OpSel.DontCare
      wbSel := WriteBackSel.ALU
      aluSel := ALUSel.COPY1
      dmemSel := DMemSel.DontCare
      branchSel := BranchSel.Next
      op := Op.LUI
    }
    //////////////////////////////////////////////
    // AUIPC
    //////////////////////////////////////////////
    .casedf(b"0010111") {
      imm := immUType
      rs1OpSel := RS1OpSel.Immediate
      rs2OpSel := RS2OpSel.PC
      wbSel := WriteBackSel.ALU
      aluSel := ALUSel.ADD
      dmemSel := DMemSel.DontCare
      branchSel := BranchSel.Next
      op := Op.AUIPC
    }
    //////////////////////////////////////////////
    // JAL
    //////////////////////////////////////////////
    .casedf(b"1101111") {
      imm := immJType
      rs1OpSel := RS1OpSel.DontCare
      rs2OpSel := RS2OpSel.DontCare
      wbSel := WriteBackSel.PCPlus4
      aluSel := ALUSel.DontCare
      dmemSel := DMemSel.DontCare
      branchSel := BranchSel.JAL
      op := Op.JAL
    }
    //////////////////////////////////////////////
    // JALR
    //////////////////////////////////////////////
    .casedf(b"1100111") {
      matchdf(func3)
        .casedf(b"000") {
          imm := immUType
          rs1OpSel := RS1OpSel.RegSource
          rs2OpSel := RS2OpSel.Immediate
          wbSel := WriteBackSel.PCPlus4
          aluSel := ALUSel.DontCare
          dmemSel := DMemSel.DontCare
          branchSel := BranchSel.JALR
          op := Op.JALR
        }
    }
    //////////////////////////////////////////////
    // Branch
    //////////////////////////////////////////////
    .casedf(b"1100011") {
      imm := immBType
      rs1OpSel := RS1OpSel.DontCare
      rs2OpSel := RS2OpSel.DontCare
      wbSel := WriteBackSel.DontCare
      aluSel := ALUSel.DontCare
      dmemSel := DMemSel.DontCare
      matchdf(func3)
        .casedf(b"000")               {op := Op.BEQ;    branchSel := BranchSel.BEQ}
        .casedf(b"001")               {op := Op.BNE;    branchSel := BranchSel.BNE}
        .casedf(b"100")               {op := Op.BLT;    branchSel := BranchSel.BLT}
        .casedf(b"101")               {op := Op.BGE;    branchSel := BranchSel.BGE}
        .casedf(b"110")               {op := Op.BLTU;   branchSel := BranchSel.BLTU}
        .casedf(b"111")               {op := Op.BGEU;   branchSel := BranchSel.BGEU}
    }
    //////////////////////////////////////////////
    // Load from Memory
    //////////////////////////////////////////////
    .casedf(b"0000011"){
      imm := immIType
      branchSel := BranchSel.Next
      rs1OpSel := RS1OpSel.RegSource
      rs2OpSel := RS2OpSel.Immediate
      wbSel := WriteBackSel.Mem
      aluSel := ALUSel.ADD
      matchdf(func3)
        .casedf(b"000")               {op := Op.LB;     dmemSel := DMemSel.LB}
        .casedf(b"001")               {op := Op.LH;     dmemSel := DMemSel.LH}
        .casedf(b"010")               {op := Op.LW;     dmemSel := DMemSel.LW}
        .casedf(b"100")               {op := Op.LBU;    dmemSel := DMemSel.LBU}
        .casedf(b"101")               {op := Op.LHU;    dmemSel := DMemSel.LHU}
    }
    //////////////////////////////////////////////
    // Store to Memory
    //////////////////////////////////////////////
    .casedf(b"0100011"){
      imm := immSType
      branchSel := BranchSel.Next
      rs1OpSel := RS1OpSel.RegSource
      rs2OpSel := RS2OpSel.Immediate
      wbSel := WriteBackSel.DontCare
      aluSel := ALUSel.ADD
      matchdf(func3)
        .casedf(b"000")               {op := Op.SB;     dmemSel := DMemSel.SB}
        .casedf(b"001")               {op := Op.SH;     dmemSel := DMemSel.SH}
        .casedf(b"010")               {op := Op.SW;     dmemSel := DMemSel.SW}
    }
    //////////////////////////////////////////////
    // Immediate Calc
    //////////////////////////////////////////////
    .casedf(b"0010011"){
      imm := immIType
      branchSel := BranchSel.Next
      rs1OpSel := RS1OpSel.RegSource
      rs2OpSel := RS2OpSel.Immediate
      wbSel := WriteBackSel.ALU
      dmemSel := DMemSel.DontCare
      matchdf(func3)
        .casedf(b"000")               {op := Op.ADDI;   aluSel := ALUSel.ADD}
        .casedf(b"010")               {op := Op.SLTI;   aluSel := ALUSel.SLT}
        .casedf(b"011")               {op := Op.SLTIU;  aluSel := ALUSel.SLTU}
        .casedf(b"100")               {op := Op.XORI;   aluSel := ALUSel.XOR}
        .casedf(b"110")               {op := Op.ORI;    aluSel := ALUSel.OR}
        .casedf(b"111")               {op := Op.ANDI;   aluSel := ALUSel.AND}
        .casedf(b"001") {
          matchdf(func7)
            .casedf(b"0000000")       {op := Op.SLLI;   aluSel := ALUSel.SLL}
        }
        .casedf(b"101") {
          matchdf(func7)
            .casedf(b"0000000")       {op := Op.SRLI;   aluSel := ALUSel.SRL}
            .casedf(b"0100000")       {op := Op.SRAI;   aluSel := ALUSel.SRA}
        }
    }
    //////////////////////////////////////////////
    // R-Type
    //////////////////////////////////////////////
    .casedf(b"0110011"){
      branchSel := BranchSel.Next
      rs1OpSel := RS1OpSel.RegSource
      rs2OpSel := RS2OpSel.RegSource
      wbSel := WriteBackSel.ALU
      dmemSel := DMemSel.DontCare
      matchdf(func7 ## func3)
        .casedf(b"0000000" ## b"000") {op := Op.ADD;    aluSel := ALUSel.ADD}
        .casedf(b"0100000" ## b"000") {op := Op.SUB;    aluSel := ALUSel.SUB}
        .casedf(b"0000000" ## b"001") {op := Op.SLL;    aluSel := ALUSel.SLL}
        .casedf(b"0000000" ## b"010") {op := Op.SLT;    aluSel := ALUSel.SLT}
        .casedf(b"0000000" ## b"011") {op := Op.SLTU;   aluSel := ALUSel.SLTU}
        .casedf(b"0000000" ## b"100") {op := Op.XOR;    aluSel := ALUSel.XOR}
        .casedf(b"0000000" ## b"101") {op := Op.SRL;    aluSel := ALUSel.SRL}
        .casedf(b"0100000" ## b"101") {op := Op.SRA;    aluSel := ALUSel.SRA}
        .casedf(b"0000000" ## b"110") {op := Op.OR;     aluSel := ALUSel.OR}
        .casedf(b"0000000" ## b"111") {op := Op.AND;    aluSel := ALUSel.AND}
    }

  val inst = DecodedInst(rs1_addr = rs1_addr, rs2_addr = rs2_addr, rd_addr = rd_addr, rd_wren = rd_wren,
    imm = imm, shamt = shamt, branchSel = branchSel, rs1OpSel = rs1OpSel, rs2OpSel = rs2OpSel,
    aluSel = aluSel, wbSel = wbSel, dmemSel = dmemSel)

  atOwnerDo {
    this.instRaw <> fetchInst.instRaw
  }
}


case class DecodedInst(
  rs1_addr  : DFBits[5],
  rs2_addr  : DFBits[5],
  rd_addr   : DFBits[5],
  rd_wren   : DFBool,

//Immediate values for ALU execution
  imm       : DFBits[32],
  shamt     : DFUInt[5],

//Control Signals
  branchSel : DFEnum[BranchSel],
  rs1OpSel  : DFEnum[RS1OpSel],
  rs2OpSel  : DFEnum[RS2OpSel],
  aluSel    : DFEnum[ALUSel],
  wbSel     : DFEnum[WriteBackSel],
  dmemSel   : DFEnum[DMemSel]
)
