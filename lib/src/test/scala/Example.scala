import dfhdl.*

enum ALUSel extends Encode:
  case ADD, SUB, SLL, SRL, SRA, AND, OR, XOR, SLT, SLTU, COPY1

class ALU extends EDDesign:
  // scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
  val op1    = Bits(32) <> IN
  val op2    = Bits(32) <> IN
  val aluSel = ALUSel   <> IN
  val aluOut = Bits(32) <> OUT

  process(all):
    val shamt = op2(4, 0)

    import ALUSel.*
    val outCalc: Bits[32] <> VAL = aluSel match
      case ADD   => op1 + op2
      case SUB   => op1 - op2
      case AND   => op1 & op2
      case OR    => op1 | op2
      case XOR   => op1 ^ op2
      case SLT   => (op1.sint < op2.sint).extend
      case SLTU  => (op1 < op2).extend
      case SLL   => op1 << shamt
      case SRL   => op1 >> shamt
      case SRA   => (op1.sint >> shamt).bits
      case COPY1 => op1
      case _     => ?
    aluOut := outCalc
end ALU

@main def hello: Unit =
  given options.CompilerOptions.CommitFolder = ".\\sandbox"

  ALU()
    .printCodeString
    .compile
    .printGenFiles
    .lint
end hello
