package ZFiant.vhdl

case class Component(name : Name, ports : List[Value.Def[Value.Def.Modifier.Port]]) extends Declaration

case class ComponentInstance(name : Name, componentName : Name, connections : List[Net.Connection]) extends Statement

