package DFiant
package compiler.backend.verilog
import printer.formatter._
import DFiant.compiler.sync._
import ClockParams.Edge
import ResetParams.Active
import DFiant.sim._

private object Sim {
  private def clkRstGuard(implicit printer: Printer) : String = {
    import printer.config._
    val clkName = ClockParams.get.name
    val rstName = ResetParams.get.name
    val clkGuard = ClockParams.get.edge match {
      case Edge.Rising => clkName
      case Edge.Falling => s"$OP!$clkName"
    }
    val rstGuard = ResetParams.get.active match {
      case Active.Low => rstName
      case Active.High => s"$OP!$rstName"
    }
    s"$clkGuard && $rstGuard"
  }
  object Assert {
    def unapply(assert : DFSimMember.Assert)(implicit printer: Printer) : Option[String] = {
      import printer.config._
      val msg = assert.msgRef.seq.map {
        case Left(v) =>
          v.get match {
            case DFBits(w) if w % 4 == 0 => s"0x%0h"
            case DFUInt(w) if w % 4 == 0 => s"0x%0h"
            case DFBits(_) => s"%0b"
            case DFUInt(_) => s"%0d"
            case DFSInt(_) => s"%0d"
            case DFBool() => s"%0d"
            case DFBit() => s"%0d"
            case DFEnum(enumType) => s"%0d"
          }
        case Right(s) => s
      }.mkString("\"","","\"")
      val args = assert.msgRef.seq.collect {
        case Left(v) => Value.ref(v)
      }.mkString(", ")
      val display = if (args.isEmpty) s"$$$KW display($msg);" else s"$$$KW display($msg, $args);"
      val statement = assert.condOptionRef match {
        case Some(condRef) =>
          val cond = Value.ref(condRef.get)
          s"$KW if ($clkRstGuard && $OP!${cond.applyBrackets()}) $display"
        case None =>
          s"$KW if ($clkRstGuard) $display"
      }
      Some(statement)
    }
  }

  object Finish {
    def unapply(assert : DFSimMember.Finish)(implicit printer: Printer) : Option[String] = {
      import printer.config._
      Some(s"if (${clkRstGuard}) $$$KW finish;")
    }
  }
}
