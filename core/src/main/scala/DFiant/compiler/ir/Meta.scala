package DFiant.compiler.ir
import DFiant.internals.*
sealed trait Meta extends Product, Serializable:
  val nameOpt: Option[String]
  val position: Position
  val lateConstruction: Boolean
  final val isAnonymous: Boolean = nameOpt.isEmpty
  final val name: String =
    nameOpt.getOrElse(s"anon${this.hashCode.toHexString}")
  

final case class MemberMeta(
    nameOpt: Option[String],
    position: Position,
    lateConstruction: Boolean
) extends Meta

final case class OwnerMeta(
    nameOpt: Option[String],
    position: Position,
    lateConstruction: Boolean,
    clsName: String,
    clsPosition: Position
) extends Meta
