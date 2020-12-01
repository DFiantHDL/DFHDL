package DFiant
package compiler.backend.vhdl
import compiler.printer.formatter._

private object Component {
  def apply(entityName : String, ports : List[String])(implicit printer : Printer) : String = {
    import printer.config._
    s"""$KW component $entityName $KW is
       |$KW port (
       |${ports.mkString(";\n").delim()}
       |);
       |$KW end $entityName;""".stripMargin
  }
}

private object ComponentInstance {
  def apply(name : String, entityName : String, connections : List[String])(
    implicit printer : Printer
  ) : String = {
    import printer.config._
    s"""$name : $KW entity $TP work.$entityName(${entityName}_arch) $KW port $KW map (
       |${connections.mkString(",\n").delim()}
       |);""".stripMargin
  }
}

