package Proc
import DFiant._


trait Opcode extends Enum.Manual[7] {
  val LUI     = Entry(bin"0110111")
  val AUIPC   = Entry(bin"0010111")
  val JAL     = Entry(bin"1101111")
  val JALR    = Entry(bin"1100111")
  val BEQ     = Entry(bin"1100011")
  val BNE     = Entry(bin"0110111")
  val BLT     = Entry(bin"0110111")
  val BGE     = Entry(bin"0110111")
  val BLTU    = Entry(bin"0110111")
  val BGEU    = Entry(bin"0110111")
}
object Opcode extends Opcode


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
