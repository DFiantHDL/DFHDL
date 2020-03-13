package ZFiant.compiler.backend.vhdl.adt

import ZFiant.compiler.backend.utils._

final case class Entity(name : String, ports : List[ValueDcl[ValueDcl.Modifier.Port]]) {
  override def toString: String =
    s"""entity $name is
       |port (
       |${ports.mkString(";\n").delim}
       |);
       |end $name;""".stripMargin
}
