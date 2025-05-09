package RISCV

import dfhdl.*

//https://five-embeddev.com/riscv-isa-manual/latest/opcode-map.html
enum Opcode extends Encoded.Manual(7):
  val value: UInt[7] <> CONST = (d"5'$ordinal", b"11").toBits
  // scalafmt: { align.tokens = [{code = ","}]}
  case LOAD,   LOAD_FP,  custom_0, MISC_MEM, OP_IMM, AUIPC, OP_IMM_32, _48b
  case STORE,  STORE_FP, custom_1, AMO,      OP,     LUI,   OP_32,     _64b
  case MADD,   MSUB,     NMSUB,    NMADD,    OP_FP,  resr1, custom_2,  _48b2
  case BRANCH, JALR,     resr2,    JAL,      SYSTEM, resr3, custom_3,  _80b

enum ALUSel extends Encoded:
  case ADD, SUB, SLL, SRL, SRA, AND, OR, XOR, SLT, SLTU, COPY1

case class Instr(fields: Bits[25] <> VAL, opcode: Opcode <> VAL) extends Struct

case class IType(rs1: UInt[5] <> VAL, rs2: UInt[5] <> VAL)
