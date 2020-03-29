package ZFiant
package compiler.backend.vhdl

import compiler.sync._

private object Sim {
  object Assert {
    def unapply(assert : DFSimMember.Assert)(implicit printer: Printer) : Option[String] = {
      import printer.config._
      import formatter._
      val clkName = ClockParams.get.name
      val msg = assert.msg.seq.map {
        case Left(v) => s"$FN to_hstring(${Value.ref(v)})"
        case Right(s) => s""""$s""""
      }.mkString(" & ")
      val report = s"$KW report $msg $KW severity $TP${assert.severity};"
      val statement = assert.condOptionRef match {
        case Some(condRef) =>
          val cond = Value.ref(condRef.get)
          s"$KW assert ($cond) $report"
        case None =>
          report
      }
      Some(If(s"$OP rising_edge($clkName)", List(statement), If.End()))
    }
  }

  object Finish {
    def unapply(assert : DFSimMember.Finish)(implicit printer: Printer) : Option[String] = {
      import printer.config._
      import formatter._
      val clkName = ClockParams.get.name
      Some(If(s"$OP rising_edge($clkName)", List(s"finish(0);"), If.End()))
    }
  }
}
