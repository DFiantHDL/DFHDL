package DFiant
package compiler.backend.verilog
import printer.formatter._

private object Case {
  def apply(expression : String, items : String)(implicit printer : Printer) : String = {
    import printer.config._
    s"""$KW case ($expression)
       |${items.delim()}
       |$KW endcase""".stripMargin
  }

  object Item {
    def apply(pattern : String, statements : List[String])(implicit printer : Printer) : String = {
      import printer.config._
      if (statements.length == 1)
        s"""$pattern : ${statements.mkString("\n").removeAlignment}""".stripMargin
      else
        s"""$pattern : $KW begin
           |${statements.mkString("\n").delim()}
           |$KW end""".stripMargin
    }
  }
  object Choice {
    object Default {
      def apply()(implicit printer : Printer) : String = s"${printer.config.KW}default"
    }
    object Pattern {
      def apply(pattern : DFAny.Pattern[_], width : Int)(implicit printer : Printer) : String = {
        import printer.config._
        pattern match {
          case x : DFBits.Pattern =>
            x.patternSet.map{p =>
              if (width % 4 == 0) s"""$LIT${width}'h${p.toHex}"""
              else s"""$LIT${width}'b${p.toBin}"""
            }.mkString(",")
          case x : DFUInt.Pattern =>
            x.patternSet.map(p => p.toRange.map(v => s"$LIT${width}'$LIT d${v}").mkString(",")).mkString(",")
          case x : DFSInt.Pattern =>
            x.patternSet.map(p => p.toRange.map{
              case v if v >= 0 => s"$LIT${width}'$LIT d${v}"
              case v if v < 0 => s"$LIT-${width}s'$LIT d${-v}"
            }.mkString(",")).mkString(",")
          case x : DFBool.Pattern =>
            x.patternSet.map(p => if (p) s"$LIT 1" else s"$LIT 0").mkString(",")
          case x : DFEnum.Pattern =>
            x.patternSet.map(p => s"`E_${p.enumType.name}_${p.enumType.entries(p.value).name}".toUpperCase).mkString(",")
          case _ => throw new IllegalArgumentException(s"\nUnsupported pattern type for Verilog compilation: $pattern")
        }
      }
    }
  }
}