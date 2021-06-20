package DFiant.compiler.ir
import DFiant.internals.*
sealed trait Meta extends Product, Serializable:
  val nameOpt: Option[String]
  val position: Position
  val lateConstruction: Boolean
  def =~(that: Meta): Boolean
  final val isAnonymous: Boolean = nameOpt.isEmpty
  final val name: String =
    nameOpt.getOrElse(s"anon${this.hashCode.toHexString}")

final case class MemberMeta(
    nameOpt: Option[String],
    position: Position,
    lateConstruction: Boolean
) extends Meta:
  def =~(that: Meta): Boolean = that match {
    case _: MemberMeta => this.nameOpt == that.nameOpt
    case _             => false
  }

final case class OwnerMeta(
    nameOpt: Option[String],
    position: Position,
    lateConstruction: Boolean,
    clsName: String,
    clsPosition: Position
) extends Meta:
  def =~(that: Meta): Boolean = that match {
    case that: OwnerMeta =>
      this.nameOpt == that.nameOpt && this.clsName == that.clsName
    case _ => false
  }
