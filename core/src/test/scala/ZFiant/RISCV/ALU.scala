package ZFiant
package RISCV

trait ALU extends DFDesign {
  private val op1     = DFBits[32]      <> IN
  private val op2     = DFBits[32]      <> IN
  private val aluSel  = DFEnum(ALUSel)  <> IN
  private val aluOut  = DFBits[32]      <> OUT

  //helper casted values
  private val op1u = op1.uint
  private val op2u = op2.uint
  private val op1s = op1.sint
  private val op2s = op2.sint
  private val shamt = op2(4, 0).uint

  private val outCalc = DFBits[32].matchdf(aluSel)
    .casedf(ALUSel.ADD){(op1u + op2u).bits}
    .casedf(ALUSel.SUB){(op1u - op2u).bits}
    .casedf(ALUSel.AND){op1 & op2}
    .casedf(ALUSel.OR){op1 | op2}
    .casedf(ALUSel.XOR){op1 ^ op2}
    .casedf(ALUSel.SLT){(op1s < op2s).bits.resize(32)}
    .casedf(ALUSel.SLTU){(op1u < op2u).bits.resize(32)}
    .casedf(ALUSel.SLL){op1 << shamt}
    .casedf(ALUSel.SRL){op1 >> shamt}
    .casedf(ALUSel.SRA){(op1s >> shamt).bits}
    .casedf(ALUSel.COPY1){op1}

  aluOut <> outCalc

  //    def calcConn(op1 : DFBits[32], op2 : DFBits[32], aluSel : DFEnum[ALUSel])(
  //      implicit ctx : DFAny.Op.Context
  //    ) : DFBits[32] = {
  //      this.op1 <> op1
  //      this.op2 <> op2
  //      this.aluSel <> aluSel
  //      this.aluOut
  //    }
}

object ALUApp extends App {
  val alu = new ALU {}
  import compiler.backend.vhdl._
  alu.compile.printGenFiles()
}

