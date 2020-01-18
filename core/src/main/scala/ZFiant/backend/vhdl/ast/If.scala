package ZFiant.backend.vhdl.ast

import ZFiant.backend.utils._

final case class If(cond : Value, statements : List[Statement], closing : If.Closing) extends Statement {
  cond.rtType match {
    case Value.Type.boolean => //OK
    case Value.Type.std_logic => //OK
    case _ => ??? //Bad
  }
  override def toString: String =
    s"""if ${cond.refString} then
       |${statements.mkString("\n").delim}
       |$closing""".stripMargin
}

object If {
  sealed trait Closing extends Statement
  case object End extends Closing {
    override def toString: String = "end if;"
  }
  final case class ElsIf(cond : Value, statements : List[Statement], closing : Closing) extends Closing {
    override def toString: String =
      s"""elsif ${cond.refString} then
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
