package DFiant
package compiler.backend.verilog

import compiler.printer.formatter._

private object ModuleInstance {
  def apply(name: String, moduleName: String, connections: List[String])(
      implicit printer: Printer
  ): String = {
    import printer.config._
    s"""$moduleName $name(
       |${connections.mkString(",\n").delim()}
       |);""".stripMargin
  }
}
