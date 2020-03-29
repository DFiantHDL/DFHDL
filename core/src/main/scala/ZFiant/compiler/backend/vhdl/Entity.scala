package ZFiant
package compiler.backend.vhdl

private object Entity {
  def apply(name : String, ports : List[String])(implicit printer : Printer) : String = {
    import printer.config._
    import formatter._
    val portCluster =
      if (ports.isEmpty) ""
      else
        s"""$KW port (
           |${ports.mkString(";\n").delim()}
           |);""".stripMargin

    s"""$KW entity $name $KW is
       |$portCluster
       |$KW end $name;
       |""".stripMargin
  }
}
