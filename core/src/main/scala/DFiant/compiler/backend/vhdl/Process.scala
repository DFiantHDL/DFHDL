package DFiant
package compiler.backend.vhdl
import compiler.printer.formatter._

private object Process {
  def apply(
      name: String,
      sensitivity: String,
      variables: List[String],
      statements: List[String]
  )(implicit
      printer: Printer
  ): String = {
    import printer.config._
    val variablesStr =
      if (variables.isEmpty) "" else variables.mkString("\n", "\n", "").delim()
    if (statements.isEmpty) ""
    else
      s"""$name : $KW process $sensitivity$variablesStr
       |$KW begin
       |${statements.mkString("\n").delim()}
       |$KW end $KW process;""".stripMargin
  }
  object Sensitivity {
    def all(implicit printer: Printer): String = s"(${printer.config.KW}all)"
    def apply(names: scala.List[String])(implicit printer: Printer): String =
      if (names.isEmpty) ""
      else names.mkString("(", ", ", ")")
  }
}
