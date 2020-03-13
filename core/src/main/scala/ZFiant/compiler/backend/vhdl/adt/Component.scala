package ZFiant.compiler.backend.vhdl.adt

import ZFiant.compiler.backend.utils._

final case class Component(entityName : String, ports : List[ValueDcl[ValueDcl.Modifier.Port]]) extends Declaration {
  override def toString: String =
    s"""component $entityName is
       |port (
       |${ports.mkString(";\n").delim}
       |);
       |end $entityName;""".stripMargin

}

final case class ComponentInstance(name : String, entityName : String, connections : List[(String, String)]) extends Statement {
  override def toString: String =
    s"""$name : entity work.$entityName(${entityName}_arch) port map (
       |${connections.map(c => s"${c._1} => ${c._2}").mkString(",\n").delim}
       |);""".stripMargin
}

