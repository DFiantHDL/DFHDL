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
  val Plus4, Branch, Jump, JumpReg, Exception = EntryIncLastBy(1)
}

object BranchSel extends Enum.Manual(4) {
  val Next, NotEqual, Equal, GreaterEqual,
      GreaterEqualUnsigned, LessThan, LessThanUnsigned, Jump, JumpReg = EntryIncLastBy(1)
}

object RS1OpSel extends Enum.Manual(2) {
  val RegSource1, ImmediateUType, ZeroExtendedCSRI  = EntryIncLastBy(1)
  val DontCare = RegSource1 //Giving another name to an entry as a Don't Care value
}

object RS2OpSel extends Enum.Manual(2) {
  val RegSource2, ImmediateIType, ImmediateSType, PC  = EntryIncLastBy(1)
  val DontCare = RegSource2
}

object ControlSel extends Enum.Manual(1) {
  val Disable, Enable = EntryIncLastBy(1)
  val DontCare = Disable
}

object WriteBackSel extends Enum.Manual(2) {
  val ALU, Mem, PCPlus4, CSR = EntryIncLastBy(1)
  val DontCare = ALU
}

object MemFuncSel extends Enum.Manual(2) {
  val Read, Write, Fence = EntryIncLastBy(1)
  val DontCare = Read
}

object Test {
  type BV[W] = BitVector//[W]
  class RType(funct7 : BV[7], rs2 : BV[5], rs1 : BV[5], funct3 : BV[3], rd : BV[5], opcode : BV[7])
  object RType {
    def unapply(instr : BV[32]) = Some(instr.bits(31, 25), instr.bits(24, 20), instr.bits(19, 15), instr.bits(14, 12), instr.bits(11, 7), instr.bits(6, 0))
    class InstrCO(funct7Check : BV[7], funct3Check : BV[3], opcodeCheck : BV[7]) {
      def unapply(instr : BV[32]) = instr match {
        case RType(funct7,rs2,rs1,funct3,rd,opcode) if (funct7 == funct7Check) && (funct3 == funct3Check) && (opcode == opcodeCheck) => Some(rd, rs1, rs2)
        case _ => None
      }
    }
  }
  class IType(imm : BV[12], rs1 : BV[5], funct3 : BV[3], rd : BV[5], opcode : BV[7])
  object IType {
    def unapply(instr : BV[32]) = Some(instr.bits(31, 20), instr.bits(19, 15), instr.bits(14, 12), instr.bits(11, 7), instr.bits(6, 0))
    class InstrCO(funct7Check : BV[7], funct3Check : BV[3], opcodeCheck : BV[7]) {
      def unapply(instr : BV[32]) = instr match {
        case IType(imm,rs1,funct3,rd,opcode) if (funct3 == funct3Check) && (opcode == opcodeCheck) => Some(rd, rs1, imm)
        case _ => None
      }
    }
  }

  //            RD         RS1          Shamt                                              Funct7   Funct3   Opcode
  class SLLI(rd : BV[5], rs1 : BV[5], shamt : BV[5]); object SLLI  extends RType.InstrCO(b"0000000", b"001", b"0010011")
  class SRLI(rd : BV[5], rs1 : BV[5], shamt : BV[5]); object SRLI  extends RType.InstrCO(b"0000000", b"101", b"0010011")
  class SRAI(rd : BV[5], rs1 : BV[5], shamt : BV[5]); object SRAI  extends RType.InstrCO(b"0100000", b"101", b"0010011")

  //            RD         RS1          RS2                                                Funct7   Funct3   Opcode
  class ADD (rd : BV[5], rs1 : BV[5], rs2   : BV[5]); object ADD  extends RType.InstrCO(b"0000000", b"000", b"0110011")
  class SUB (rd : BV[5], rs1 : BV[5], rs2   : BV[5]); object SUB  extends RType.InstrCO(b"0100000", b"000", b"0110011")
  class SLL (rd : BV[5], rs1 : BV[5], rs2   : BV[5]); object SLL  extends RType.InstrCO(b"0000000", b"001", b"0110011")
  class SLT (rd : BV[5], rs1 : BV[5], rs2   : BV[5]); object SLT  extends RType.InstrCO(b"0000000", b"010", b"0110011")
  class SLTU(rd : BV[5], rs1 : BV[5], rs2   : BV[5]); object SLTU extends RType.InstrCO(b"0000000", b"011", b"0110011")
  class XOR (rd : BV[5], rs1 : BV[5], rs2   : BV[5]); object XOR  extends RType.InstrCO(b"0000000", b"100", b"0110011")
  class SRL (rd : BV[5], rs1 : BV[5], rs2   : BV[5]); object SRL  extends RType.InstrCO(b"0000000", b"101", b"0110011")
  class SRA (rd : BV[5], rs1 : BV[5], rs2   : BV[5]); object SRA  extends RType.InstrCO(b"0100000", b"101", b"0110011")
  class OR  (rd : BV[5], rs1 : BV[5], rs2   : BV[5]); object OR   extends RType.InstrCO(b"0000000", b"110", b"0110011")
  class AND (rd : BV[5], rs1 : BV[5], rs2   : BV[5]); object AND  extends RType.InstrCO(b"0000000", b"111", b"0110011")

//  case class ADD2 (rd : BV[5], rs1 : BV[5], rs2   : BV[5]) extends RType(b"0000000", rs2, rs1, b"000", rd, b"0110011") {
//  }
//  object ADD2 {
//    def unapply(arg: BV[32]): Option[(BV[5], BV[5], BV[5])] = ???
//
//  }

  def decode(instr : BV[32]) : Unit = {
    instr match {
      case SLLI (rd, rs1, shamt) =>
      case SRLI (rd, rs1, shamt) =>
      case SRAI (rd, rs1, shamt) =>

      case ADD (rd, rs1, rs2) =>
      case SUB (rd, rs1, rs2) =>
      case SLL (rd, rs1, rs2) =>
      case SLT (rd, rs1, rs2) =>
      case SLTU(rd, rs1, rs2) =>
      case XOR (rd, rs1, rs2) =>
      case SRL (rd, rs1, rs2) =>
      case SRA (rd, rs1, rs2) =>
      case OR  (rd, rs1, rs2) =>
      case AND (rd, rs1, rs2) =>
      case _ =>
    }
  }
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
