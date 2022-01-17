package DFiant.compiler.ir
import DFiant.internals.*

final case class Meta(
    nameOpt: Option[String],
    position: Position,
    lateConstruction: Boolean
):
  val isAnonymous: Boolean = nameOpt.isEmpty
  val name: String =
    nameOpt.getOrElse(s"anon${this.hashCode.toHexString}")
  def anonymize: Meta = copy(nameOpt = None)
  def setName(name: String): Meta = copy(nameOpt = Some(name))
  def =~(that: Meta): Boolean = this.nameOpt == that.nameOpt
