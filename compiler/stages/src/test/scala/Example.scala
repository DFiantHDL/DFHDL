import dfhdl.*
import dfhdl.compiler.stages.verilog.*
import scala.sys.process.*

enum ALUSel extends Encode:
  case ADD, SUB, SLL, SRL, SRA, AND, OR, XOR, SLT, SLTU, COPY1

class ALU extends EDDesign:
  // scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
  val op1    = Bits(32) <> IN
  val op2    = Bits(32) <> IN
  val aluSel = ALUSel   <> IN
  val aluOut = Bits(32) <> OUT

  process(all) {
    // helper casted values
    val op1u  = op1.uint
    val op2u  = op2.uint
    val op1s  = op1.sint
    val op2s  = op2.sint
    val shamt = op2(4, 0)

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
      case _     => ?
    aluOut := outCalc
  }
end ALU

@main def hello: Unit =
  import backends.verilog.sv2005
  System.setProperty("user.dir", "c:\\Users\\oronpo\\IdeaProjects\\dfhdl")
  val top = new ALU
  top.compile
//  top.printVerilogCode
  top.commitVerilogCode()
  val output = Process(
    "verilator_bin --lint-only -Wall -I./../../sandbox ./../../sandbox/ALU.sv"
  ).!
//  println("done")
