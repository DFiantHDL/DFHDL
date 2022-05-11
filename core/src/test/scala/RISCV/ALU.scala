package RISCV
import DFiant.*

class ALU extends DFDesign:
  // scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
  val op1    = Bits(32) <> IN
  val op2    = Bits(32) <> IN
  val aluSel = ALUSel   <> IN
  val aluOut = Bits(32) <> OUT

  // helper casted values
  private val op1u  = op1.uint
  private val op2u  = op2.uint
  private val op1s  = op1.sint
  private val op2s  = op2.sint
  private val shamt = op2(4, 0)

  import ALUSel.*
  val outCalc: Bits[32] <> VAL = aluSel match
    case ADD   => (op1u + op2u).bits
    case SUB   => (op1u - op2u).bits
    case AND   => op1 & op2
    case OR    => op1 | op2
    case XOR   => op1 ^ op2
    case SLT   => (op1s < op2s).bits.resize(32)
    case SLTU  => (op1u < op2u).bits.resize(32)
    case SLL   => op1 << shamt
    case SRL   => op1 >> shamt
    case SRA   => (op1s >> shamt).bits
    case COPY1 => op1

  aluOut := outCalc
end ALU
