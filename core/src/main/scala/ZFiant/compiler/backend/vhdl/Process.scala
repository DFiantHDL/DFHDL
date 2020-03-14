package ZFiant
package compiler.backend.vhdl

private object Process {
  def apply(name : String, sensitivity : String, variables : List[String], statements : List[String])(
    implicit printer : Printer
  ) : String = {
    import printer.config._
    import formatter._
    s"""$name : $KW process ($sensitivity)
       |${variables.mkString("\n").delim}
       |$KW begin
       |${statements.mkString("\n").delim}
       |$KW end $KW process;""".stripMargin
  }
  object Sensitivity {
    object List {
      def apply(names : scala.List[String])(implicit printer : Printer) : String = names.mkString(",")
    }
    object All {
      def apply()(implicit printer : Printer) : String = s"${printer.config.KW}all"
    }
  }
}
