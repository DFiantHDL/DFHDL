package Proc

import DFiant._

trait ALU extends DFDesign {
  val op1     = DFBits(32)      <> IN
  val op2     = DFBits(32)      <> IN
  val shamt   = DFUInt(5)       <> IN
  val aluSel  = DFEnum(ALUSel)  <> IN
  val out     = DFBits(32)      <> OUT


  val op1u = op1.uint
  val op2u = op2.uint
  val op1s = op1.sint
  val op2s = op2.sint

//  exe_alu_out := MuxCase(0.U, Array(
//    (io.ctl.alu_fun === ALU_ADD)  -> (exe_alu_op1 + exe_alu_op2).toUInt,
//    (io.ctl.alu_fun === ALU_SUB)  -> (exe_alu_op1 - exe_alu_op2).toUInt,
//    (io.ctl.alu_fun === ALU_AND)  -> (exe_alu_op1 & exe_alu_op2).toUInt,
//    (io.ctl.alu_fun === ALU_OR)   -> (exe_alu_op1 | exe_alu_op2).toUInt,
//    (io.ctl.alu_fun === ALU_XOR)  -> (exe_alu_op1 ^ exe_alu_op2).toUInt,
//    (io.ctl.alu_fun === ALU_SLT)  -> (exe_alu_op1.toSInt < exe_alu_op2.toSInt).toUInt,
//    (io.ctl.alu_fun === ALU_SLTU) -> (exe_alu_op1 < exe_alu_op2).toUInt,
//    (io.ctl.alu_fun === ALU_SLL)  -> ((exe_alu_op1 << alu_shamt)(conf.xprlen-1, 0)).toUInt,
//    (io.ctl.alu_fun === ALU_SRA)  -> (exe_alu_op1.toSInt >> alu_shamt).toUInt,
//    (io.ctl.alu_fun === ALU_SRL)  -> (exe_alu_op1 >> alu_shamt).toUInt,
//    (io.ctl.alu_fun === ALU_COPY1)-> exe_alu_op1
//  ))

  val outCalc = DFBits(32).matchdf(aluSel)
    .casedf(ALUSel.ADD){(op1u + op2u).bits}
    .casedf(ALUSel.SUB){(op1u - op2u).bits}
    .casedf(ALUSel.AND){op1 & op2}
    .casedf(ALUSel.OR){op1 | op2}
    .casedf(ALUSel.XOR){op1 ^ op2}
    .casedf(ALUSel.SLT){(op1s < op2s).bits.extendLeftTo(32)}
    .casedf(ALUSel.SLTU){(op1u < op2u).bits.extendLeftTo(32)}
    .casedf_{h"00000000"}

  out <> outCalc
}
