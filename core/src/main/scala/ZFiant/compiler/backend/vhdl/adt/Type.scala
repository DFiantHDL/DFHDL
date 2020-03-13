package ZFiant.compiler.backend.vhdl.adt

sealed trait Type extends Product with Serializable
object Type {
  final case class std_logic_vector(width : Int) extends Type {
    override def toString: String = s"std_logic_vector(${width-1} downto 0)"
  }
  final case class unsigned(width : Int) extends Type {
    override def toString: String = s"unsigned(${width-1} downto 0)"
  }
  final case class signed(width : Int) extends Type {
    override def toString: String = s"signed(${width-1} downto 0)"
  }
  case object std_logic extends Type {
    override def toString: String = s"std_logic"
  }
  case object boolean extends Type {
    override def toString: String = s"boolean"
  }
  case class enumeration(name: String) extends Type {
    override def toString: String = name
  }
}
