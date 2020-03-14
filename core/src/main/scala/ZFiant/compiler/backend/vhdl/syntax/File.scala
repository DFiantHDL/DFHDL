package ZFiant.compiler.backend.vhdl.syntax

final case class File(entity: String, architecture: String) {
  override def toString: String = s"$entity\n$architecture"
}
