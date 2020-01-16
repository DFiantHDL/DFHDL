package ZFiant.vhdl.ast

import DFiant.internals.StringExtras

final case class Architecture(name : Name, entityName : Name, declarations : List[Declaration], statements : List[Statement]) {
  override def toString: String =
    s"""architecture $name of $entityName is
       |${declarations.mkString("\n").delimRowsBy(delim)}
       |begin
       |${statements.mkString("\n").delimRowsBy(delim)}
       |end $name;""".stripMargin
}
