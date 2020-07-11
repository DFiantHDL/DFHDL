package DFiant
package compiler.backend.verilog
import printer.formatter._

private object Module {
  def apply(moduleName : String, ports : List[String], statements : List[String])(
    implicit printer : Printer
  )  : String = {
    import printer.config._
    s"""$KW module $moduleName(
       |${ports.mkString(",\n").delim()}
       |);
       |${statements.mkString("\n").delim()}
       |$KW endmodule""".stripMargin
  }
}
