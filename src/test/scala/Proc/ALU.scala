package Proc

import DFiant._

trait ALU extends DFDesign {
  val op1 = DFBits(32) <> IN
  val op2 = DFBits(32) <> IN
  val aluSel = DFEnum(ALUSel) <> IN
  val out = DFBits(32) <> OUT

  val cond = DFBool()
  val a = DFBits(32).select(cond)(op1, op2)
  val sel = DFUInt(2)
  val b = DFBits(32).select(sel, op1)(op1, op2, op1, op2)
  implicit class Bla(a : DFAny) {
    def matchdf[T](body : => T) : T = ???
//    def casedf[T](sel : DF)
  }
  val op1u = op1.uint
  val op2u = op2.uint
//  val aaa = new DFAny.NewVar(0,"")(???,???) {
//    override type TVal = DFAny
//    override type TVar = DFAny.Var
//  }
//  new aluSel.matchdf(DFBits(32)) {
////    casedf(ALUSel.ADD){b"11111111111111111111111111111111"}
//    casedf(ALUSel.SUB){(op1u - op2u).bits}
//  }

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

}
