package DFiant
package compiler.backend.vhdl

import compiler.sync._

private object Sim {
  object Assert {
    def unapply(assert : DFSimMember.Assert)(implicit printer: Printer, revision: VHDLRevision) : Option[String] = {
      import printer.config._
      import formatter._
      val clkName = ClockParams.get.name
      val msg = assert.msgRef.seq.map {
        case Left(v) =>
          v.get match {
            case DFBits(w) if w % 4 == 0 => s"$FN to_hstring(${Value.ref(v)})"
            case DFUInt(w) if w % 4 == 0 => s"$FN to_hstring($FN to_slv(${Value.ref(v)}))"
            case value => revision match {
              case VHDLRevision.VHDL1993 => value match {
                case DFBits(_) => s"$TP std_logic_vector'image(${Value.ref(value)})"
                case DFUInt(_) => s"$TP unsigned'image(${Value.ref(value)})"
                case DFSInt(_) => s"$TP signed'image(${Value.ref(value)})"
                case DFBool() => s"$TP boolean'image(${Value.ref(value)})"
                case DFBit() => s"$TP std_logic'image(${Value.ref(value)})"
                case DFEnum(enumType) => s"${enumType.name}_type'image(${Value.ref(value)})"
              }
              case VHDLRevision.VHDL2008 => s"$FN to_string(${Value.ref(v)})"
            }
          }
        case Right(s) => s""""$s""""
      }.mkString(" & ")
      val report = s"$KW report $msg $KW severity $TP${assert.severity};"
      val statement = assert.condOptionRef match {
        case Some(condRef) =>
          val cond = Value.boolRef(condRef.get)
          s"$KW assert ($cond) $report"
        case None =>
          report
      }
      Some(If(s"$OP rising_edge($clkName)", List(statement), If.End()))
    }
  }

  object Finish {
    def unapply(assert : DFSimMember.Finish)(implicit printer: Printer, revision: VHDLRevision) : Option[String] = {
      import printer.config._
      import formatter._
      val clkName = ClockParams.get.name
      val finish = revision match {
        case VHDLRevision.VHDL1993 => List(s"""$KW report "Simulation Finished" $KW severity $TP FAILURE;""")
        case VHDLRevision.VHDL2008 => List(s"finish(0);")
      }
      Some(If(s"$OP rising_edge($clkName)", finish, If.End()))
    }
  }
}
