package ZFiant.backend.vhdl.ast

import ZFiant.backend.utils._
final case class Architecture(name : Name, entityName : Name, declarations : List[Declaration], statements : List[Statement]) {
  override def toString: String =
    s"""architecture $name of $entityName is
       |${declarations.mkString("\n").delim}
       |begin
       |${statements.mkString("\n").delim}
       |end $name;""".stripMargin
}
