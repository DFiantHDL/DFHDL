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

@df class Decoder(fetchInst : IMemInst) extends DFDesign {
  private val instIn    = new IMemInst <> IN
  final   val instOut   = new DecodedInst <> OUT

  import instIn.raw
  import instOut.{rs1_addr, rs2_addr, rd_addr, debugOp, imm, branchSel, rs1OpSel, rs2OpSel, aluSel, wbSel, dmemSel, rd_wren}

  private val opcode    = raw(6, 0)
  private val func7     = raw(31, 25)
  private val func3     = raw(14, 12)
  private val func73    = func7 ++ func3
  private val immIType  = raw(31, 20).sint.resize(32).bits
  private val immSType  = (raw(31, 25), raw(11, 7)).bits.sint.resize(32).bits
  private val immBType  = (raw(31), raw(7), raw(30, 25), raw(11, 8), b"0").bits.sint.resize(32).bits
  private val immUType  = raw(31, 12).resizeRight(32)
  private val immJType  = (raw(31), raw(19, 12), raw(20), raw(30, 21), b"0").bits.sint.resize(32).bits
  private val notOpCode = raw(31, 7)
  rs1_addr := raw(19, 15)
  rs2_addr := raw(24, 20)
  rd_addr := raw(11, 7)

  instIn <> instOut
  debugOp := DebugOp.Unsupported //Default op is not supported unless selected otherwise
  imm := immIType //Default immediate is IType unless selected otherwise
  branchSel := BranchSel.Next
  rs1OpSel := RS1OpSel.DontCare
  rs2OpSel := RS2OpSel.DontCare
  aluSel := ALUSel.DontCare
  wbSel := WriteBackSel.DontCare
  dmemSel := DMemSel.DontCare
  rd_wren := false

  matchdf(opcode)
    //////////////////////////////////////////////
    // LUI
    //////////////////////////////////////////////
    .casedf(b"0110111") {
      imm := immUType
      rs1OpSel := RS1OpSel.Immediate
      rs2OpSel := RS2OpSel.DontCare
      wbSel := WriteBackSel.ALU
      rd_wren := true
      aluSel := ALUSel.COPY1
      dmemSel := DMemSel.DontCare
      branchSel := BranchSel.Next
      debugOp := DebugOp.LUI
    }
    //////////////////////////////////////////////
    // AUIPC
    //////////////////////////////////////////////
    .casedf(b"0010111") {
      imm := immUType
      rs1OpSel := RS1OpSel.Immediate
      rs2OpSel := RS2OpSel.PC
      wbSel := WriteBackSel.ALU
      rd_wren := true
      aluSel := ALUSel.ADD
      dmemSel := DMemSel.DontCare
      branchSel := BranchSel.Next
      debugOp := DebugOp.AUIPC
    }
    //////////////////////////////////////////////
    // JAL
    //////////////////////////////////////////////
    .casedf(b"1101111") {
      imm := immJType
      rs1OpSel := RS1OpSel.DontCare
      rs2OpSel := RS2OpSel.DontCare
      wbSel := WriteBackSel.PCPlus4
      rd_wren := true
      aluSel := ALUSel.DontCare
      dmemSel := DMemSel.DontCare
      branchSel := BranchSel.JAL
      debugOp := DebugOp.JAL
    }
    //////////////////////////////////////////////
    // JALR
    //////////////////////////////////////////////
    .casedf(b"1100111") {
      matchdf(func3)
        .casedf(b"000") {
          imm := immIType
          rs1OpSel := RS1OpSel.RegSource
          rs2OpSel := RS2OpSel.Immediate
          wbSel := WriteBackSel.PCPlus4
          rd_wren := true
          aluSel := ALUSel.DontCare
          dmemSel := DMemSel.DontCare
          branchSel := BranchSel.JALR
          debugOp := DebugOp.JALR
        }
        .casedf_{}
    }
    //////////////////////////////////////////////
    // Branch
    //////////////////////////////////////////////
    .casedf(b"1100011") {
      imm := immBType
      rs1OpSel := RS1OpSel.DontCare
      rs2OpSel := RS2OpSel.DontCare
      wbSel := WriteBackSel.DontCare
      rd_wren := false
      aluSel := ALUSel.DontCare
      dmemSel := DMemSel.DontCare
      matchdf(func3)
        .casedf(b"000")               {debugOp := DebugOp.BEQ;    branchSel := BranchSel.BEQ}
        .casedf(b"001")               {debugOp := DebugOp.BNE;    branchSel := BranchSel.BNE}
        .casedf(b"100")               {debugOp := DebugOp.BLT;    branchSel := BranchSel.BLT}
        .casedf(b"101")               {debugOp := DebugOp.BGE;    branchSel := BranchSel.BGE}
        .casedf(b"110")               {debugOp := DebugOp.BLTU;   branchSel := BranchSel.BLTU}
        .casedf(b"111")               {debugOp := DebugOp.BGEU;   branchSel := BranchSel.BGEU}
        .casedf_{}
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
      rd_wren := true
      aluSel := ALUSel.ADD
      matchdf(func3)
        .casedf(b"000")               {debugOp := DebugOp.LB;     dmemSel := DMemSel.LB}
        .casedf(b"001")               {debugOp := DebugOp.LH;     dmemSel := DMemSel.LH}
        .casedf(b"010")               {debugOp := DebugOp.LW;     dmemSel := DMemSel.LW}
        .casedf(b"100")               {debugOp := DebugOp.LBU;    dmemSel := DMemSel.LBU}
        .casedf(b"101")               {debugOp := DebugOp.LHU;    dmemSel := DMemSel.LHU}
        .casedf_{}
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
      rd_wren := false
      aluSel := ALUSel.ADD
      matchdf(func3)
        .casedf(b"000")               {debugOp := DebugOp.SB;     dmemSel := DMemSel.SB}
        .casedf(b"001")               {debugOp := DebugOp.SH;     dmemSel := DMemSel.SH}
        .casedf(b"010")               {debugOp := DebugOp.SW;     dmemSel := DMemSel.SW}
        .casedf_{}
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
      rd_wren := true
      dmemSel := DMemSel.DontCare
      matchdf(func3)
        .casedf(b"000")               {debugOp := DebugOp.ADDI;   aluSel := ALUSel.ADD}
        .casedf(b"010")               {debugOp := DebugOp.SLTI;   aluSel := ALUSel.SLT}
        .casedf(b"011")               {debugOp := DebugOp.SLTIU;  aluSel := ALUSel.SLTU}
        .casedf(b"100")               {debugOp := DebugOp.XORI;   aluSel := ALUSel.XOR}
        .casedf(b"110")               {debugOp := DebugOp.ORI;    aluSel := ALUSel.OR}
        .casedf(b"111")               {debugOp := DebugOp.ANDI;   aluSel := ALUSel.AND}
        .casedf(b"001") {
          matchdf(func7)
            .casedf(b"0000000")       {debugOp := DebugOp.SLLI;   aluSel := ALUSel.SLL}
            .casedf_{}
        }
        .casedf(b"101") {
          matchdf(func7)
            .casedf(b"0000000")       {debugOp := DebugOp.SRLI;   aluSel := ALUSel.SRL}
            .casedf(b"0100000")       {debugOp := DebugOp.SRAI;   aluSel := ALUSel.SRA}
            .casedf_{}
        }
        .casedf_{}
    }
    //////////////////////////////////////////////
    // R-Type
    //////////////////////////////////////////////
    .casedf(b"0110011"){
      branchSel := BranchSel.Next
      rs1OpSel := RS1OpSel.RegSource
      rs2OpSel := RS2OpSel.RegSource
      wbSel := WriteBackSel.ALU
      rd_wren := true
      dmemSel := DMemSel.DontCare
      matchdf(func73)
        .casedf(b"0000000" ++ b"000") {debugOp := DebugOp.ADD;    aluSel := ALUSel.ADD}
        .casedf(b"0100000" ++ b"000") {debugOp := DebugOp.SUB;    aluSel := ALUSel.SUB}
        .casedf(b"0000000" ++ b"001") {debugOp := DebugOp.SLL;    aluSel := ALUSel.SLL}
        .casedf(b"0000000" ++ b"010") {debugOp := DebugOp.SLT;    aluSel := ALUSel.SLT}
        .casedf(b"0000000" ++ b"011") {debugOp := DebugOp.SLTU;   aluSel := ALUSel.SLTU}
        .casedf(b"0000000" ++ b"100") {debugOp := DebugOp.XOR;    aluSel := ALUSel.XOR}
        .casedf(b"0000000" ++ b"101") {debugOp := DebugOp.SRL;    aluSel := ALUSel.SRL}
        .casedf(b"0100000" ++ b"101") {debugOp := DebugOp.SRA;    aluSel := ALUSel.SRA}
        .casedf(b"0000000" ++ b"110") {debugOp := DebugOp.OR;     aluSel := ALUSel.OR}
        .casedf(b"0000000" ++ b"111") {debugOp := DebugOp.AND;    aluSel := ALUSel.AND}
        .casedf_{}
    }
    //////////////////////////////////////////////
    // System
    //////////////////////////////////////////////
    .casedf(b"1110011"){
      matchdf(func3)
        .casedf(b"000") { //ECALL/EBREAK
          matchdf(notOpCode)
            .casedf(b"000000000000" ++ b"00000" ++ b"000" ++ b"00000") {debugOp := DebugOp.ECALL;}
            .casedf(b"000000000001" ++ b"00000" ++ b"000" ++ b"00000") {debugOp := DebugOp.EBREAK;}
            .casedf_{}
        }
        .casedf(b"001")               {debugOp := DebugOp.CSRRW;  }
        .casedf(b"010")               {debugOp := DebugOp.CSRRS;  }
        .casedf(b"011")               {debugOp := DebugOp.CSRRC;  }
        .casedf(b"101")               {debugOp := DebugOp.CSRRWI; }
        .casedf(b"110")               {debugOp := DebugOp.CSRRSI; }
        .casedf(b"111")               {debugOp := DebugOp.CSRRCI; }
        .casedf_{}
    }
    .casedf(b"0001111"){debugOp := DebugOp.FENCE;}//FENCE
    .casedf_{}

  atOwnerDo {
    this.instIn <> fetchInst
  }
}

@df class DecodedInst extends IMemInst {
  //Register File Addresses & Control
  final val rs1_addr  = DFBits[5]
  final val rs2_addr  = DFBits[5]
  final val rd_addr   = DFBits[5]
  final val rd_wren   = DFBit()

  //Immediate values for ALU execution
  final val imm       = DFBits[32]

  //Control Signals
  final val branchSel = DFEnum(BranchSel)
  final val rs1OpSel  = DFEnum(RS1OpSel)
  final val rs2OpSel  = DFEnum(RS2OpSel)
  final val aluSel    = DFEnum(ALUSel)
  final val wbSel     = DFEnum(WriteBackSel)
  final val dmemSel   = DFEnum(DMemSel)
  final val debugOp   = DFEnum(DebugOp) //Just for debugging
}

