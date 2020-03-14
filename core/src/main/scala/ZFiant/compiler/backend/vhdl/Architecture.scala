package ZFiant
package compiler.backend.vhdl

object Architecture {
  def apply(name : String, entityName : String, declarations : List[String], statements : List[String])(
    implicit printer : Printer
  )  : String = {
    import printer.config._
    import formatter._
    s"""$KW architecture $name $KW of $entityName $KW is
       |${declarations.mkString("\n").delim}
       |$KW begin
       |${statements.mkString("\n").delim}
       |$KW end $name;""".stripMargin
  }
}
