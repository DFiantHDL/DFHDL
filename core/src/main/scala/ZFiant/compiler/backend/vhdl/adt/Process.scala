package ZFiant.compiler.backend.vhdl.adt

import ZFiant.compiler.backend.utils._

final case class Process(sensitivity : Process.Sensitivity, variables : List[ValueDcl[ValueDcl.Modifier.Port]], statements : List[Statement]) extends Statement {
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
    case class List(names : scala.List[String]) extends Sensitivity {
      override def toString: String = names.mkString(",")
    }
    case object All extends Sensitivity {
      override def toString: String = "all"
    }
  }
}
