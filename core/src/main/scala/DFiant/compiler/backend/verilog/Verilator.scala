package DFiant
package compiler.backend.verilog
import compiler.printer.formatter._

object Verilator {
  private def macroDef(macroKeyword: String, statementBlock: String)(implicit
      printer: Printer
  ): String = {
    import printer.config._
    s"""$FN`$macroKeyword VERILATOR
       |${statementBlock}
       |$FN`endif
       |""".stripMargin
  }
  def ifdef(statementBlock: String)(implicit printer: Printer): String =
    macroDef("ifdef", statementBlock)
  def ifndef(statementBlock: String)(implicit printer: Printer): String =
    macroDef("ifndef", statementBlock)
  def ifelsedef(ifBlock: String, elseBlock: String)(implicit
      printer: Printer
  ): String = {
    import printer.config._
    s"""$FN`ifdef VERILATOR
       |${ifBlock}
       |$FN`else
       |${elseBlock}
       |$FN`endif
       |""".stripMargin
  }
}
