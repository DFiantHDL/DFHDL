package ZFiant.compiler.backend.vhdl.adt

final case class File(entity: Entity, architecture: Architecture) {
  override def toString: String = s"$entity\n$architecture"
}
