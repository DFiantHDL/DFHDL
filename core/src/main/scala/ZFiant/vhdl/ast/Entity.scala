package ZFiant.vhdl.ast

import DFiant.internals.StringExtras

final case class Entity(name : Name, ports : List[Value.Dcl[Value.Dcl.Modifier.Port]]) {
  override def toString: String =
    s"""entity $name is
       |port (
       |${ports.mkString(";\n").delimRowsBy(delim)}
       |);
       |end $name;""".stripMargin
}
