package ZFiant
package compiler.backend.vhdl

private object Process {
  def apply(sensitivity : String, variables : List[String], statements : List[String])(
    implicit printer : Printer
  ) : String = {
    import printer.config._
    import formatter._
    s"""process ($sensitivity)
       |${variables.mkString("\n").delim}
       |begin
       |${statements.mkString("\n").delim}
       |end process;""".stripMargin
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
