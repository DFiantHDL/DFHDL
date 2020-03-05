package ZFiant.compiler.backend.vhdl.adt

import ZFiant.compiler.backend.utils._

final case class Component(entityName : Name, ports : List[Value.Dcl[Value.Dcl.Modifier.Port]]) extends Declaration {
  override def toString: String =
    s"""component $entityName is
       |port (
       |${ports.mkString(";\n").delim}
       |);
       |end $entityName;""".stripMargin

}

final case class ComponentInstance(name : Name, entityName : Name, connections : List[(Name, Value)]) extends HasName with Statement {
  override def toString: String =
    s"""$name : $entityName
       |port map (
       |${connections.map(c => s"${c._1} => ${c._2.refString}").mkString(",\n").delim}
       |);""".stripMargin
}

