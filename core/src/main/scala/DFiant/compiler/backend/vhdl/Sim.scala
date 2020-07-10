package DFiant
package compiler.backend.vhdl

import DFiant.compiler.sync.ResetParams.Active
import compiler.sync._
import DFiant.sim._
import printer.formatter._

private object Sim {
  private def clkRstGuard(implicit printer: Printer) : String = {
    import printer.config._
    val clkName = ClockParams.get.name
    val rstName = ResetParams.get.name
    val rstActive = ResetParams.get.active match {
      case Active.Low => "'1'"
      case Active.High => "'0'"
    }
    s"$OP rising_edge($clkName) $OP and $rstName $OP= $rstActive"
  }
  object Assert {
    def unapply(assert : DFSimMember.Assert)(implicit printer: Printer) : Option[String] = {
      import printer.config._
      val msg = assert.msgRef.seq.map {
        case Left(v) =>
          v.get match {
            case DFBits(w) if w % 4 == 0 => s"$FN to_hstring(${Value.ref(v)})"
            case DFUInt(w) if w % 4 == 0 => s"$FN to_hstring($FN to_slv(${Value.ref(v)}))"
            case value => revision match {
              case Revision.V93 => value match {
                case DFBits(_) => s"$TP std_logic_vector'image(${Value.ref(value)})"
                case DFUInt(_) => s"$TP unsigned'image(${Value.ref(value)})"
                case DFSInt(_) => s"$TP signed'image(${Value.ref(value)})"
                case DFBool() => s"$TP boolean'image(${Value.ref(value)})"
                case DFBit() => s"$TP std_logic'image(${Value.ref(value)})"
                case DFEnum(enumType) => s"${enumType.name}_type'image(${Value.ref(value)})"
              }
              case Revision.V2008 => s"$FN to_string(${Value.ref(v)})"
            }
          }
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
      Some(If(clkRstGuard, List(statement), If.End()))
    }
  }

  object Finish {
    def unapply(assert : DFSimMember.Finish)(implicit printer: Printer) : Option[String] = {
      import printer.config._
      val finish = revision match {
        case Revision.V93 => List(s"""$KW report "Simulation Finished" $KW severity $TP FAILURE;""")
        case Revision.V2008 => List(s"finish(0);")
      }
      Some(If(clkRstGuard, finish, If.End()))
    }
  }
}
