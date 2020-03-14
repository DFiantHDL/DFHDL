package ZFiant
package compiler.backend.vhdl

object Component {
  def apply(entityName : String, ports : List[String])(implicit printer : Printer) : String = {
    import printer.config._
    import formatter._
    s"""$KW component $entityName $KW is
       |$KW port (
       |${ports.mkString(";\n").delim}
       |);
       |$KW end $entityName;""".stripMargin
  }
}

object ComponentInstance {
  def apply(name : String, entityName : String, connections : List[(String, String)])(
    implicit printer : Printer
  ) : String = {
    import printer.config._
    import formatter._
    s"""$name : $KW entity work.$entityName(${entityName}_arch) $KW port $KW map (
       |${connections.map(c => s"${c._1} => ${c._2}").mkString(",\n").delim}
       |);""".stripMargin
  }
}

