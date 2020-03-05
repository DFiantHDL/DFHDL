package ZFiant.compiler.backend.vhdl.adt

import ZFiant.compiler.backend.utils._

final case class Process(sensitivity : Process.Sensitivity, variables : List[Value.Dcl[Value.Dcl.Modifier.Port]], statements : List[Statement]) extends Statement {
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
//  def ClkAsyncRst(clk : Clock, rst : Reset, variables : List[Value.Dcl[Value.Dcl.Modifier.Port]], resetStatements : List[Statement], clockedStatements : List[Statement]) : Process = {
//    val sensitivity = Sensitivity.List(List(clk.name, rst.name))
//    val statements : List[Statement] = List(If(rst.active, resetStatements, If.ElsIf(clk.active, clockedStatements, If.End)))
//    Process(sensitivity, variables, statements)
//  }
//  def Async(variables : List[Value.Dcl[Value.Dcl.Modifier.Port]], statements : List[Statement]) : Process =
//    Process(Sensitivity.All, variables, statements)
}
