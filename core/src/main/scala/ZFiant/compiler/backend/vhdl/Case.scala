package ZFiant
package compiler.backend.vhdl

private object Case {
  def apply(expression : String, whens : List[String])(implicit printer : Printer) : String = {
    import printer.config._
    import formatter._
    s"""$KW case $expression $KW is
       |${whens.mkString("\n").delim()}
       |$KW end $KW case;""".stripMargin
  }

  object When {
    def apply(choice : String, statements : List[String])(implicit printer : Printer) : String = {
      import printer.config._
      import formatter._
      s"""$KW when $choice =>
         |${statements.mkString("\n").delim()}""".stripMargin
    }
  }
  object Choice {
    object Others {
      def apply()(implicit printer : Printer) : String = s"${printer.config.KW}others"
    }
    object List {
      def apply(values : scala.List[String])(implicit printer : Printer) : String = values.mkString("|")
    }
  }
}
