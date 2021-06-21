package DFiant.compiler.ir
import DFiant.internals.*

final case class Meta(
    nameOpt: Option[String],
    position: Position,
    lateConstruction: Boolean
):
  final val isAnonymous: Boolean = nameOpt.isEmpty
  final val name: String =
    nameOpt.getOrElse(s"anon${this.hashCode.toHexString}")
  def =~(that: Meta): Boolean = this.nameOpt == that.nameOpt
