package DFiant
package compiler.backend.verilog
import compiler.printer.formatter._

private object If {
  def apply(cond : String, statements : List[String], closing : String)(
    implicit printer : Printer
  ) : String = {
    import printer.config._
    s"""$KW if ($cond) ${block(statements)}${if(closing.isEmpty) "" else closing}""".stripMargin
  }
  private def block(statements : List[String])(implicit printer: Printer) : String = {
    import printer.config._
    if (statements.length == 1) s"${statements.head.removeAlignment}\n" //single line body ==> we remove alignment for better view
    else s"\n$KW begin\n${{statements.mkString("\n")}.delim()}\n$KW end\n"
  }
  object ElsIf {
    def apply(cond : String, statements : List[String], closing : String)(implicit printer : Printer) : String = {
      import printer.config._
      s"""$KW else if ($cond) ${block(statements)}${if(closing.isEmpty) "" else closing}""".stripMargin
    }
  }
  object Else {
    def apply(statements : List[String])(implicit printer : Printer) : String = {
      import printer.config._
      s"""$KW else ${block(statements)}""".stripMargin
    }
  }
}
