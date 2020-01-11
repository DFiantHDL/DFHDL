package ZFiant.vhdl

case class component(name : Name, ports : List[Value.Def]) extends declaration

case class component_instance(name : Name, componentName : Name, connections : List[Net.Connection]) extends statement

