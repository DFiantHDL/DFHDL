import DFiant._

package object RISCV2 {
  final val XLEN = 32
  type XLEN = XLEN.type
  final val StartAddress = h"00000000"
  final val regsNum = 0 until 32

  implicit object Op extends Enum.Auto {
    val Unsupported, LUI, AUIPC, JAL, JALR, BEQ, BNE, BLT, BGE, BLTU, BGEU,
    LB, LH, LW, LBU, LHU, SB, SH, SW, ADDI, SLTI, SLTIU, XORI, ORI, ANDI,
    SLLI, SRLI, SRAI, ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND = Entry()
  }

  implicit object BranchSel extends Enum.Manual(4) {
    val Next, BNE, BEQ, BGE,
    BGEU, BLT, BLTU, JAL, JALR = EntryDelta()
  }
  type BranchSel = BranchSel.type

  implicit object RS1OpSel extends Enum.Manual(2) {
    val RegSource, Immediate  = EntryDelta()
    val DontCare = RegSource //Giving another name to an entry as a Don't Care value
  }
  type RS1OpSel = RS1OpSel.type

  implicit object RS2OpSel extends Enum.Manual(2) {
    val RegSource, Immediate, PC  = EntryDelta()
    val DontCare = RegSource
  }
  type RS2OpSel = RS2OpSel.type

  implicit object ALUSel extends Enum.Manual(4) {
    val ADD, SUB, SLL, SRL, SRA, AND, OR, XOR, SLT, SLTU, COPY1 = EntryDelta()
    val DontCare = ADD
  }
  type ALUSel = ALUSel.type

  implicit object WriteBackSel extends Enum.Manual(2) {
    val ALU, Mem, PCPlus4, CSR = EntryDelta()
    val DontCare = ALU
  }
  type WriteBackSel = WriteBackSel.type

  implicit object DMemSel extends Enum.Auto {
    val LB, LH, LW, LBU, LHU, SB, SH, SW = Entry()
    val DontCare = LW
  }
  type DMemSel = DMemSel.type

}
