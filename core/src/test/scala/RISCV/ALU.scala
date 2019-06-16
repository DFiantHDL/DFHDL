/*
 *     This file is part of DFiant.
 *
 *     DFiant is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     DFiant is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public License
 *     along with DFiant.  If not, see <https://www.gnu.org/licenses/>.
 */

package RISCV

import DFiant._

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
    .casedf(ALUSel.SLT){(op1s < op2s).bits.extendLeftTo(32)}
    .casedf(ALUSel.SLTU){(op1u < op2u).bits.extendLeftTo(32)}
    .casedf(ALUSel.SLL){op1 << shamt}
    .casedf(ALUSel.SRL){op1 >> shamt}
    .casedf(ALUSel.SRA){(op1s >> shamt).bits}
    .casedf(ALUSel.COPY1){op1}
    .casedf_{b0s}

  aluOut <> outCalc

  def calcConn(op1 : DFBits[32], op2 : DFBits[32], aluSel : DFEnum[ALUSel])(
    implicit ctx : DFAny.Op.Context
  ) : DFBits[32] = {
    this.op1 <> op1
    this.op2 <> op2
    this.aluSel <> aluSel
    this.aluOut
  }
}


trait ALUTest extends DFDesign {
  val op1     = DFBits[32]     <> IN init(h"00000000", h"00000001", h"00000002")
  val op2     = DFBits[32]     <> IN init(h"00000010", h"00000011", h"00000012")
  val aluSel  = DFEnum(ALUSel) <> IN init(ALUSel.ADD, ALUSel.SLL, ALUSel.AND)
  val aluOut  = DFBits[32]     <> OUT

  val alu = new ALU {}
  aluOut <> alu.calcConn(op1, op2, aluSel)

}




object ALUTestApp extends App {
  import Xilinx.FPGAs.`XC7VX485T-2FFG1761C`._
  implicit val a = DFAnyConfiguration.foldedInit

//  val alu = new ALU {}
//  val vhdALU = alu.compileToVHDL.print().toFile("test.vhd")
  val aluTest = new ALUTest {}.printCodeString
}