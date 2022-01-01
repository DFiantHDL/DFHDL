package RISCV

import DFiant.*

//https://five-embeddev.com/riscv-isa-manual/latest/opcode-map.html
enum Opcode extends DFEnum.Manual(7):
  val value: DFUInt[7] <> TOKEN = d"5'$ordinal".bits ++ b"11"
  // scalafmt: { align.tokens = [{code = ","}]}
  case LOAD,   LOAD_FP,  custom_0, MISC_MEM, OP_IMM, AUIPC, OP_IMM_32, _48b
  case STORE,  STORE_FP, custom_1, AMO,      OP,     LUI,   OP_32,     _64b
  case MADD,   MSUB,     NMSUB,    NMADD,    OP_FP,  resr1, custom_2,  _48b2
  case BRANCH, JALR,     resr2,    JAL,      SYSTEM, resr3, custom_3,  _80b

case class Instr(fields: DFBits[25] <> VAL, opcode: Opcode <> VAL)

case class IType(rs1: DFUInt[5] <> VAL, rs2: DFUInt[5] <> VAL)
//object IType:
//  def unapply(opcode: Instr <> VAL)(using
//      DFC
//  ): Option[IType] = None

class Tester extends DFSpec:
//  enum Instr(fields: DFBits[25] <> VAL, val opcode: Opcode <> VAL):
//    case Branch(fields: DFBits[25] <> VAL) extends Instr(fields, Opcode.BRANCH)
//
  val instr = Instr <> IN
//  val opcode = Opcode <> IN

//  instr match
//    case IType(rs1, rs2) =>
//  instr match
//    case Instr.BEQ(fields) =>
