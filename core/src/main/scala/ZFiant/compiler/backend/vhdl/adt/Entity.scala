package ZFiant.compiler.backend.vhdl.adt

import ZFiant.compiler.backend.utils._

final case class Entity(name : Name, ports : List[Value.Dcl[Value.Dcl.Modifier.Port]]) {
  override def toString: String =
    s"""entity $name is
       |port (
       |${ports.mkString(";\n").delim}
       |);
       |end $name;""".stripMargin
}
