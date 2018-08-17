package Proc
import DFiant._


object Opcode extends Enum.Manual(7) {
  val LUI     = Entry(b"0110111")
  val AUIPC   = Entry(b"0010111")
  val JAL     = Entry(b"1101111")
  val JALR    = Entry(b"1100111")
  val BEQ     = Entry(b"1100011")
  val BNE     = Entry(b"0110111")
  val BLT     = Entry(b"0110111")
  val BGE     = Entry(b"0110111")
  val BLTU    = Entry(b"0110111")
  val BGEU    = Entry(b"0110111")
}

object PCSel extends Enum.Manual(3) {
  val Plus4, Branch, Jump, JumpReg, Exception = Entry.incLastBy(1)
}

object BranchSel extends Enum.Manual(4) {
  val Next, NotEqual, Equal, GreaterEqual,
      GreaterEqualUnsigned, LessThan, LessThanUnsigned, Jump, JumpReg = Entry.incLastBy(1)
}

object RS1OpSel extends Enum.Manual(2) {
  val RegSource1, ImmediateUType, ZeroExtendedCSRI  = Entry.incLastBy(1)
  val DontCare = RegSource1 //Giving another name to an entry as a Don't Care value
}

object RS2OpSel extends Enum.Manual(2) {
  val RegSource2, ImmediateIType, ImmediateSType, PC  = Entry.incLastBy(1)
  val DontCare = RegSource2
}

object ControlSel extends Enum.Manual(1) {
  val Disable, Enable = Entry.incLastBy(1)
  val DontCare = Disable
}

object WriteBackSel extends Enum.Manual(2) {
  val ALU, Mem, PCPlus4, CSR = Entry.incLastBy(1)
  val DontCare = ALU
}

object MemFuncSel extends Enum.Manual(2) {
  val Read, Write, Fence = Entry.incLastBy(1)
  val DontCare = Read
}

trait Instr extends DFBits[Instr.XLEN] {
  val opcode  = bits( 6,  0)
  def isRType : DFBool = ???
  def asRType : RType = ???
}

trait RType extends Instr {
  val func7   = bits(31, 25)
  val rs2     = bits(24, 20)
  val rs1     = bits(19, 15)
  val func3   = bits(14, 12)
  val rd      = bits(11,  7)
}

trait ADD extends RType {

}

object Instr {
  type XLEN = 32
}
