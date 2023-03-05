package RISCV
import dfhdl.*

class ALU extends DFDesign:
  // scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
  val op1    = Bits(32) <> IN
  val op2    = Bits(32) <> IN
  val aluSel = ALUSel   <> IN
  val aluOut = Bits(32) <> OUT

  private val shamt = op2(4, 0)

  import ALUSel.*
  private val outCalc: Bits[32] <> VAL = aluSel match
    case ADD   => op1 + op2
    case SUB   => op1 - op2
    case AND   => op1 & op2
    case OR    => op1 | op2
    case XOR   => op1 ^ op2
    case SLT   => (op1.sint < op2.sint).bits.resize(32)
    case SLTU  => (op1 < op2).bits.resize(32)
    case SLL   => op1 << shamt
    case SRL   => op1 >> shamt
    case SRA   => (op1.sint >> shamt).bits
    case COPY1 => op1
    case _     => ?
  aluOut := outCalc
end ALU
