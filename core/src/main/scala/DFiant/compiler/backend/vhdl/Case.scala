package DFiant
package compiler.backend.vhdl
import printer.formatter._

private object Case {
  def apply(expression : String, whens : String, allowDontCare : Boolean)(implicit printer : Printer) : String = {
    import printer.config._
    s"""$KW case${if (allowDontCare) "?" else ""} $expression $KW is
       |${whens.delim()}
       |$KW end $KW case;""".stripMargin
  }

  object When {
    def apply(choice : String, statements : List[String])(implicit printer : Printer) : String = {
      import printer.config._
      s"""$KW when $choice =>
         |${statements.mkString("\n").delim()}""".stripMargin
    }
  }
  object Choice {
    object Others {
      def apply()(implicit printer : Printer) : String = s"${printer.config.KW}others"
    }
    object Pattern {
      private def intervalBigIntToString(t : Interval[BigInt])(implicit printer : Printer) : String = {
        import continuum.bound._
        import printer.config._
        val lower = t.lower.bound match {
          case Closed(v) => v
          case Open(v) => v-1
          case Unbounded() => throw new IllegalArgumentException("\nUnexpected unbounded interval")
        }
        val upper = t.upper.bound match {
          case Closed(v) => v
          case Open(v) => v+1
          case Unbounded() => throw new IllegalArgumentException("\nUnexpected unbounded interval")
        }
        if (lower == upper) lower.toString()
        else s"$LIT$lower ${printer.config.KW}to $LIT$upper"
      }
      def apply(pattern : DFAny.Pattern[_])(implicit printer : Printer) : String = pattern match {
        case x : DFBits.Pattern => x.patternSet.map(p => Value.const(p)).mkString("|")
        case x : DFUInt.Pattern => x.patternSet.map(p => intervalBigIntToString(p)).mkString("|")
        case x : DFSInt.Pattern => x.patternSet.map(p => intervalBigIntToString(p)).mkString("|")
        case x : DFBool.Pattern => x.patternSet.map(p => if (p) "'1'" else "'0'").mkString("|")
        case x : DFEnum.Pattern => x.patternSet.map(p => EnumTypeDcl.enumEntryFullName(p)).mkString("|")
        case _ => throw new IllegalArgumentException(s"\nUnsupported pattern type for VHDL compilation: $pattern")
      }
    }
  }
}
