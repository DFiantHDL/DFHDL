package ZFiant.vhdl.ast

final case class Process(sensitivity : Process.Sensitivity, variables : List[Value.Dcl.Modifier.Port], statements : List[Statement]) extends Statement {
  override def toString: String =
    s"""process ($sensitivity)
       |${variables.mkString("\n").delim}
       |begin
       |${statements.mkString("\n").delim}
       |end process;""".stripMargin
}

object Process {
  sealed trait Sensitivity extends Product with Serializable
  object Sensitivity {
    case class List(names : scala.List[Name]) extends Sensitivity {
      override def toString: String = names.mkString(",")
    }
    case object All extends Sensitivity {
      override def toString: String = "all"
    }
  }
  def sync_rst(clkName : Name, rstName : Name) : Process = ???
}
