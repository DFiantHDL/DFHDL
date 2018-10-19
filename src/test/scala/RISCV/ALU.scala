package RISCV

import DFiant._

trait ALU extends DFDesign {
  final val op1     = DFBits(32)      <> IN
  final val op2     = DFBits(32)      <> IN
  final val shamt   = DFUInt(5)       <> IN
  final val aluSel  = DFEnum(ALUSel)  <> IN
  final val out     = DFBits(32)      <> OUT

  //helper casted values
  private val op1u = op1.uint
  private val op2u = op2.uint
  private val op1s = op1.sint
  private val op2s = op2.sint

  private val outCalc = DFBits(32).matchdf(aluSel)
    .casedf(ALUSel.ADD){(op1u + op2u).bits}
    .casedf(ALUSel.SUB){(op1u - op2u).bits}
    .casedf(ALUSel.AND){op1 & op2}
    .casedf(ALUSel.OR){op1 | op2}
    .casedf(ALUSel.XOR){op1 ^ op2}
    .casedf(ALUSel.SLT){(op1s < op2s).bits.extendLeftTo(32)}
    .casedf(ALUSel.SLTU){(op1u < op2u).bits.extendLeftTo(32)}
    .casedf(ALUSel.SLL){op1 << shamt}
    .casedf(ALUSel.SRL){op1 >> shamt}
    .casedf(ALUSel.SRA){(op1s >> shamt).bits}
    .casedf(ALUSel.COPY1){op1}
    .casedf_{b0s}

  out <> outCalc
}





trait ALUTest extends DFDesign {
  val op1     = DFBits(32)     <> IN init(h"00000000", h"00000001", h"00000002")
  val op2     = DFBits(32)     <> IN init(h"00000010", h"00000011", h"00000012")
  val shamt   = DFUInt(5)      <> IN init(1, 2)
  val aluSel  = DFEnum(ALUSel) <> IN init(ALUSel.ADD, ALUSel.SLL, ALUSel.AND)
  val out     = DFBits(32)     <> OUT

  val alu = new ALU {}
  op1 <> alu.op1
  op2 <> alu.op2
  shamt <> alu.shamt
  aluSel <> alu.aluSel
  out <> alu.out
}




object ALUTestApp extends App {
  import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
  implicit val a = DFAnyConfiguration.foldedInit

  val alu = new ALU {}.printVHDLString
//  val aluTest = new ALUTest {}.printCodeString
}