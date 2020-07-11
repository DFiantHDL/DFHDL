package DFiant
package compiler.backend.verilog

import printer.formatter._

private object File {
  def apply(globalDefsFileName : String, localDefs : String, module: String)(implicit printer: Printer) : String = {
    s"""
       |${Header(globalDefsFileName)}
       |${SimHeader()}
       |$EMPTY
       |$localDefs
       |$EMPTY
       |$module""".stripMargin.formatted
  }
}

//////////////////////////////////////////////////////////////////////////////////
// Library
//////////////////////////////////////////////////////////////////////////////////
private object Header {
  def apply(globalDefsFileName : String)(implicit printer: Printer) : String = {
    import printer.config._
    s"""$FN`default_nettype	${ALGN(0)}$TP none
       |$FN`timescale $LIT 1ns/$LIT 1ps
       |$FN`include "$globalDefsFileName.v"
       |""".stripMargin
  }
}
private object SimHeader {
  def apply()(implicit printer: Printer) : String = {
//    import printer.config._
//    if (printer.inSimulation) revision match {
//      case Revision.V93 => ""
//      case Revision.V2008 =>
//        s"""$EMPTY
//           |$KW library $TP std;
//           |$KW use $TP std.$TP env.$KW all;
//           |""".stripMargin
//    } else ""
    ""
  }
}
//////////////////////////////////////////////////////////////////////////////////
