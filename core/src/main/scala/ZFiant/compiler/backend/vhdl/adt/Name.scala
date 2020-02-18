package ZFiant.compiler.backend.vhdl.adt

final case class Name(value : String) {
  val isAnonymous : Boolean = value.isEmpty
  override def toString: String = value
}
object Name {
  def anonymous : Name = Name("")
}
