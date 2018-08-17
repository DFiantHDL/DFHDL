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
  val Plus4   = Entry(0)  // PC + 4
  val Branch  = Entry(1)  // branch_target
  val Jump    = Entry(2)  // jump_target
  val JumpR   = Entry(3)  // jump_reg_target
  val Except  = Entry(4)  // exception
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
