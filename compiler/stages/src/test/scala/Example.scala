import dfhdl.*
import dfhdl.compiler.stages.verilog.*

enum ALUSel extends Encode:
  case ADD, SUB, SLL, SRL, SRA, AND, OR, XOR, SLT, SLTU, COPY1

class ALU extends EDDesign:
  // scalafmt: { align.tokens = [{code = "<>"}, {code = "="}, {code = "=>"}, {code = ":="}]}
  val op1 = SInt(32) <> IN
  val x   = op1 > 0
end ALU

@main def hello: Unit =
  import backends.verilog.sv2005
  val top = new ALU
  top.printCodeString
end hello
