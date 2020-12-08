package DFiant
package compiler.backend.verilog
import compiler.printer.formatter._

private object AlwaysBlock {
  def apply(sensitivity: String, statements: List[String])(implicit
      printer: Printer
  ): String = {
    import printer.config._
    if (statements.isEmpty) ""
    else
      s"""$KW always @($sensitivity)
       |$KW begin
       |${statements.mkString("\n").delim()}
       |$KW end
       |""".stripMargin
  }
  object Sensitivity {
    def apply(names: scala.List[String])(implicit printer: Printer): String =
      if (names.isEmpty) s"${printer.config.KW}*"
      else names.mkString(s" ${printer.config.KW}or ")
  }
}
