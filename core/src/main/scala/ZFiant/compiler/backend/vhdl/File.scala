package ZFiant
package compiler.backend.vhdl

final case class File(entity: String, architecture: String) {
  override def toString: String = s"$entity\n\n$architecture"
}
