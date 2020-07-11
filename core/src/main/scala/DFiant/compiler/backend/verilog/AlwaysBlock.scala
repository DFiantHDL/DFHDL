package DFiant
package compiler.backend.verilog
import printer.formatter._

private object AlwaysBlock {
  def apply(sensitivity : String, statements : List[String])(
    implicit printer : Printer
  ) : String = {
    import printer.config._
    if (statements.isEmpty) "" else
    s"""$KW always @($sensitivity)
       |$KW begin
       |${statements.mkString("\n").delim()}
       |$KW end
       |""".stripMargin
  }
  object Sensitivity {
    object List {
      def apply(names : scala.List[String])(implicit printer : Printer) : String =
        names.mkString(s" ${printer.config.KW}or ")
    }
    object All {
      def apply()(implicit printer : Printer) : String = s"${printer.config.KW}*"
    }
  }
}
