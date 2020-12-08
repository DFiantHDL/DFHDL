package DFiant
package compiler.backend.verilog
import compiler.printer.formatter._

private object Case {
  def apply(expression: String, items: String, allowDontCare: Boolean)(implicit
      printer: Printer
  ): String = {
    import printer.config._
    s"""$KW case${if (allowDontCare) "z" else ""} ($expression)
       |${items.delim()}
       |$KW endcase""".stripMargin
  }

  object Item {
    def apply(pattern: String, statements: List[String])(implicit
        printer: Printer
    ): String = {
      import printer.config._
      val statementsStr = statements.mkString("\n")
      if (statements.length == 1)
        s"""$pattern : ${if (statementsStr.contains("\n")) statementsStr
        else statementsStr.removeAlignment}""".stripMargin
      else
        s"""$pattern : $KW begin
           |${statementsStr.delim()}
           |$KW end""".stripMargin
    }
  }
  object Choice {
    object Default {
      def apply()(implicit printer: Printer): String =
        s"${printer.config.KW}default"
    }
    object Pattern {
      def apply(pattern: DFAny.Pattern, width: Int)(implicit
          printer: Printer
      ): String = {
        import printer.config._
        pattern match {
          case x: DFBits.Pattern =>
            x.patternSet.map { p => Value.const(p) }.mkString(",")
          case x: DFUInt.Pattern =>
            x.patternSet
              .map(p =>
                p.toRange.map(v => s"$LIT${width}'$LIT d${v}").mkString(",")
              )
              .mkString(",")
          case x: DFSInt.Pattern =>
            x.patternSet
              .map(p =>
                p.toRange
                  .map {
                    case v if v >= 0 => s"$LIT${width}'$LIT d${v}"
                    case v if v < 0  => s"$LIT-${width}s'$LIT d${-v}"
                  }
                  .mkString(",")
              )
              .mkString(",")
          case x: DFBool.Pattern =>
            x.patternSet.map(p => if (p) s"$LIT 1" else s"$LIT 0").mkString(",")
          case x: DFEnum.Pattern =>
            x.patternSet.map(p => EnumTypeDcl.enumEntryRefName(p)).mkString(",")
          case _ =>
            throw new IllegalArgumentException(
              s"\nUnsupported pattern type for Verilog compilation: $pattern"
            )
        }
      }
    }
  }
}
