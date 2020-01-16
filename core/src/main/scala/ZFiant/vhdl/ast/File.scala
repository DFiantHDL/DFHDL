package ZFiant.vhdl.ast

final case class File(entity: Entity, architecture: Architecture) {
  override def toString: String = s"$entity\n$architecture"
}
