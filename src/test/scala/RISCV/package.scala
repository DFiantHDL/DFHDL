import DFiant._

package object RISCV {
  final val XLEN = 32
  final val GH_len = 10 // added by me
  final val LH_len = 6
  final val Local_table = 8 // added
  final val tag_len = XLEN - Local_table - 2 // added
  final val row_len_local = tag_len + LH_len
  type XLEN = XLEN.type
  type GH_len = GH_len.type // added by me
  type LH_len = LH_len.type // added
  type Local_table = Local_table.type // added
  type tag_len = tag_len.type // added
  type row_len_local = row_len_local.type
  final val regsNum = 0 until 32
  final val GHNum = 0 until 1024 //2<<GH_len added by me
  final val LHNum = 0 until 64//(2 << LH_len)
  final val tournament_entries = 0 until 1024//(2<<GH_len)
  final val LH_table_entries = 0 until 256//( 2 << Local_table)

  implicit object DebugOp extends Enum.Auto {
    val Unsupported, LUI, AUIPC, JAL, JALR, BEQ, BNE, BLT, BGE, BLTU, BGEU,
    LB, LH, LW, LBU, LHU, SB, SH, SW, ADDI, SLTI, SLTIU, XORI, ORI, ANDI,
    SLLI, SRLI, SRAI, ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND,
    FENCE, FENCE_I, ECALL, EBREAK, CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI = Entry()
  }
  type DebugOp = DebugOp.type

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
