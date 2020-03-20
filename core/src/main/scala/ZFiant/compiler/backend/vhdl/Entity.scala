package ZFiant
package compiler.backend.vhdl

private object Entity {
  def apply(name : String, ports : List[String])(implicit printer : Printer) : String = {
    import printer.config._
    import formatter._
    s"""$KW entity $name $KW is
       |$KW port (
       |${ports.mkString(";\n").delim()}
       |);
       |$KW end $name;
       |""".stripMargin
  }
}
