import DFiant._

package object RISCV {
  final val XLEN = 32
  type XLEN = XLEN.type
  final val StartAddress = 0

  object PCSel extends Enum.Manual(3) {
    val Plus4, Branch, Jump, JumpReg, Exception = EntryDelta()
  }
  type PCSel = PCSel.type

  object BranchSel extends Enum.Manual(4) {
    val Next, NotEqual, Equal, GreaterEqual,
    GreaterEqualUnsigned, LessThan, LessThanUnsigned, Jump, JumpReg = EntryDelta()
  }
  type BranchSel = BranchSel.type

  object RS1OpSel extends Enum.Manual(2) {
    val RegSource, Immediate  = EntryDelta()
    val DontCare = RegSource //Giving another name to an entry as a Don't Care value
  }
  type RS1OpSel = RS1OpSel.type

  object RS2OpSel extends Enum.Manual(2) {
    val RegSource, Immediate, PC  = EntryDelta()
    val DontCare = RegSource
  }
  type RS2OpSel = RS2OpSel.type

  object ALUSel extends Enum.Manual(4) {
    val DontCare, ADD, SUB, SLL, SRL, SRA, AND, OR, XOR, SLT, SLTU, COPY1 = EntryDelta()
  }
  type ALUSel = ALUSel.type

  object WriteBackSel extends Enum.Manual(2) {
    val ALU, Mem, PCPlus4, CSR = EntryDelta()
    val DontCare = ALU
  }
  type WriteBackSel = WriteBackSel.type

  object MemFuncSel extends Enum.Manual(2) {
    val Read, Write, Fence = EntryDelta()
    val DontCare = Read
  }
  type MemFuncSel = MemFuncSel.type

}
