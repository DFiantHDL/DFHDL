package ZFiant.compiler.backend.vhdl.adt

import ZFiant.compiler.backend.utils._

final case class If(cond : String, statements : List[Statement], closing : If.Closing) extends Statement {
  override def toString: String =
    s"""if $cond then
       |${statements.mkString("\n").delim}
       |$closing""".stripMargin
}

object If {
  sealed trait Closing extends Statement
  case object End extends Closing {
    override def toString: String = "end if;"
  }
  final case class ElsIf(cond : String, statements : List[Statement], closing : Closing) extends Closing {
    override def toString: String =
      s"""elsif $cond then
         |${statements.mkString("\n").delim}
         |$closing""".stripMargin
  }
  final case class Else(statements : List[Statement]) extends Closing {
    override def toString: String =
      s"""else
         |${statements.mkString("\n").delim}
         |$End""".stripMargin
  }
}
