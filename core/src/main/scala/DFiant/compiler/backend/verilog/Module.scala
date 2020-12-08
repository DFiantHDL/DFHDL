package DFiant
package compiler.backend.verilog
import compiler.printer.formatter._

private object Module {
  def apply(moduleName: String, ports: List[String], statements: List[String])(
      implicit printer: Printer
  ): String = {
    import printer.config._
    val portsStr =
      if (ports.isEmpty) "();" else s"(\n${ports.mkString(",\n").delim()}\n);"
    s"""$KW module $moduleName$portsStr
       |${statements.mkString("\n").delim()}
       |$KW endmodule""".stripMargin
  }
}
