package ZFiant.vhdl.ast

final case class Case(expression : Value, whens : List[Case.When]) extends Statement {
  override def toString: String =
    s"""case ${expression.refString} is
       |${whens.mkString("\n").delim}
       |end case;""".stripMargin

}
object Case {
  final case class When(choice : Choice, statements : List[Statement]) {
    override def toString: String =
      s"""when $choice =>
         |${statements.mkString("\n").delim}""".stripMargin
  }
  sealed trait Choice extends Product with Serializable
  object Choice {
    case object Others extends Choice {
      override def toString: String = "others"
    }
    case class List(values : scala.List[String]) extends Choice {
      override def toString: String = values.mkString("|")
    }
  }
}
