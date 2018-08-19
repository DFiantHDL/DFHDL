package Proc
import DFiant._
import DFiant.internals._


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
  val Plus4, Branch, Jump, JumpReg, Exception = EntryDelta()
}

object BranchSel extends Enum.Manual(4) {
  val Next, NotEqual, Equal, GreaterEqual,
      GreaterEqualUnsigned, LessThan, LessThanUnsigned, Jump, JumpReg = EntryDelta()
}

object RS1OpSel extends Enum.Manual(2) {
  val RegSource1, ImmediateUType, ZeroExtendedCSRI  = EntryDelta()
  val DontCare = RegSource1 //Giving another name to an entry as a Don't Care value
}

object RS2OpSel extends Enum.Manual(2) {
  val RegSource2, ImmediateIType, ImmediateSType, PC  = EntryDelta()
  val DontCare = RegSource2
}

object ControlSel extends Enum.Manual(1) {
  val Disable, Enable = EntryDelta()
  val DontCare = Disable
}

object WriteBackSel extends Enum.Manual(2) {
  val ALU, Mem, PCPlus4, CSR = EntryDelta()
  val DontCare = ALU
}

object MemFuncSel extends Enum.Manual(2) {
  val Read, Write, Fence = EntryDelta()
  val DontCare = Read
}

object Test {
  type BV[W] = BitVector//[W]
  class RType(funct7 : BV[7], rs2 : BV[5], rs1 : BV[5], funct3 : BV[3], rd : BV[5], opcode : BV[7])
  object RType {
    def unapply(instr : BV[32]) = Some(instr.bits(31, 25), instr.bits(24, 20), instr.bits(19, 15), instr.bits(14, 12), instr.bits(11, 7), instr.bits(6, 0))
    class InstrCO(funct7Check : BV[7], funct3Check : BV[3], opcodeCheck : BV[7]) {
      def unapply(instr : BV[32]) : Boolean = instr match {
        case RType(funct7,rs2,rs1,funct3,rd,opcode) if (funct7 == funct7Check) && (funct3 == funct3Check) && (opcode == opcodeCheck) => true
        case _ => false
      }
    }
  }
  class IType(imm : BV[12], rs1 : BV[5], funct3 : BV[3], rd : BV[5], opcode : BV[7])
  object IType {
    def unapply(instr : BV[32]) = Some(instr.bits(31, 20), instr.bits(19, 15), instr.bits(14, 12), instr.bits(11, 7), instr.bits(6, 0))
    class InstrCO(funct7Check : BV[7], funct3Check : BV[3], opcodeCheck : BV[7]) {
      def unapply(instr : BV[32]) : Boolean = instr match {
        case IType(imm,rs1,funct3,rd,opcode) if (funct3 == funct3Check) && (opcode == opcodeCheck) => true
        case _ => false
      }
    }
  }

  //                                   Funct7   Funct3   Opcode
  object SLLI extends RType.InstrCO(b"0000000", b"001", b"0010011")
  object SRLI extends RType.InstrCO(b"0000000", b"101", b"0010011")
  object SRAI extends RType.InstrCO(b"0100000", b"101", b"0010011")

  //                                   Funct7   Funct3   Opcode
  object ADD  extends RType.InstrCO(b"0000000", b"000", b"0110011")
  object SUB  extends RType.InstrCO(b"0100000", b"000", b"0110011")
  object SLL  extends RType.InstrCO(b"0000000", b"001", b"0110011")
  object SLT  extends RType.InstrCO(b"0000000", b"010", b"0110011")
  object SLTU extends RType.InstrCO(b"0000000", b"011", b"0110011")
  object XOR  extends RType.InstrCO(b"0000000", b"100", b"0110011")
  object SRL  extends RType.InstrCO(b"0000000", b"101", b"0110011")
  object SRA  extends RType.InstrCO(b"0100000", b"101", b"0110011")
  object OR   extends RType.InstrCO(b"0000000", b"110", b"0110011")
  object AND  extends RType.InstrCO(b"0000000", b"111", b"0110011")

  def decode(inst : BV[32]) : Unit = {
    inst match {
      case SLLI() =>
      case SRLI() =>
      case SRAI() =>

      case ADD () =>
      case SUB () =>
      case SLL () =>
      case SLT () =>
      case SLTU() =>
      case XOR () =>
      case SRL () =>
      case SRA () =>
      case OR  () =>
      case AND () =>
      case _      =>
    }
  }
}


trait Instr extends DFBits[Instr.XLEN] {
  val opcode  = bits( 6,  0)
}

trait RType extends Instr {
  val func7   = bits(31, 25)
  val rs2     = bits(24, 20)
  val rs1     = bits(19, 15)
  val func3   = bits(14, 12)
  val rd      = bits(11,  7)
}

trait IType extends Instr {
  def imm     = bits(31, 20)
  val rs1     = bits(19, 15)
  val func3   = bits(14, 12)
  val rd      = bits(11,  7)
}

trait SType extends Instr {
  val imm     = (bits(31, 25), bits(11, 7)).bits()
  val rs2     = bits(24, 20)
  val rs1     = bits(19, 15)
  val func3   = bits(14, 12)
}

trait UType extends Instr {
  val imm     = bits(31, 12)
  val rd      = bits(11,  7)
}

trait UJType extends Instr {
  val imm     = bits(31, 12)
  val rd      = bits(11,  7)
}

object Instr {
  type XLEN = 32
}
