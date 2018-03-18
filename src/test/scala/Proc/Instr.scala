package Proc
import DFiant._

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
