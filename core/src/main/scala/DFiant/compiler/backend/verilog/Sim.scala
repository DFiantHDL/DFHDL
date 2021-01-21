package DFiant
package compiler.backend.verilog
import compiler.printer.formatter._
import constraints.timing.sync._
import ClockParams.Edge
import ResetParams.Active
import DFiant.sim._

private object Sim {
  val guardName = "clk_sim_guard"
  private def clkGuarded(
      statement: String
  )(implicit printer: Printer): String = {
    import printer.config._
    s"""$KW if ($guardName) $KW begin $statement $KW end"""
  }
  private def rstGuarded(
      statement: String
  )(implicit printer: Printer): String = {
    import printer.config._
    val rstName = ResetParams.get.name
    val rstGuard = ResetParams.get.active match {
      case Active.Low  => rstName
      case Active.High => s"$OP!$rstName"
    }
    val clkName = ClockParams.get.name
    val clkGuard = ClockParams.get.edge match {
      case Edge.Rising  => s"$OP!$clkName"
      case Edge.Falling => clkName
    }
    s"""$KW if ($rstGuard && $clkGuard) $KW begin $statement $KW end"""
  }
  private def guarded(cond: Option[DFAny.Member], guardedStatement: String)(
      implicit printer: Printer
  ): String = {
    import printer.config._
    cond match {
      case Some(value) =>
        val condStr = Value.ref(value)
        val statement =
          s"$KW if ($OP!${condStr.applyBrackets()}) $guardedStatement"
        Verilator.ifelsedef(rstGuarded(statement), clkGuarded(statement))
      case None =>
        val statement = guardedStatement
        Verilator.ifelsedef(rstGuarded(statement), clkGuarded(statement))
    }
  }
  object Assert {
    def unapply(
        assert: DFSimMember.Assert
    )(implicit printer: Printer): Option[String] = {
      import printer.config._
      val msg = assert.msgRef.seq.map {
        case Left(v) =>
          v.get match {
            case DFBits(w) if w % 4 == 0 => s"0x%.${w / 4}H"
            case DFBits(_) => s"%0b"
            case DFUInt(_) => s"%0d"
            case DFSInt(_) => s"%0d"
            case DFBool() => s"%0d"
            case DFBit() => s"%0d"
            case DFEnum(_) => s"%0s"
          }
        case Right(s) => s
      }.mkString("\"","","\"")
      val args = assert.msgRef.seq.collect {
        case Left(v) =>
          val vStr = Value.ref(v)
          v.get match {
            case DFEnum(entries) => s"${EnumEntriesDcl.tostrFuncName(entries)}($vStr)"
            case _ => vStr
          }
      }.mkString(", ")
      val display = if (args.isEmpty) s"$$$KW display($msg);" else s"$$$KW display($msg, $args);"
      Some(guarded(assert.condOptionRef.map(c => c.get), display))
    }
  }

  object Finish {
    def unapply(
        assert: DFSimMember.Finish
    )(implicit printer: Printer): Option[String] = {
      import printer.config._
      Some(guarded(None, s"$$$KW finish;"))
    }
  }
}
