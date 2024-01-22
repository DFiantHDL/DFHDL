package dfhdl.compiler.ir
import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.{ClassTag, classTag}
import dfhdl.internals.hashString

type DFRefAny = DFRef[DFMember]
sealed trait DFRef[+M <: DFMember] derives CanEqual:
  val refType: ClassTag[M @uncheckedVariance]
  final def =~(that: DFRefAny)(using MemberGetSet): Boolean = this.get =~ that.get
  def get(using getSet: MemberGetSet): M = getSet(this)
  override def toString: String = s"<${this.hashString}>"

object DFRef:
  sealed trait Empty extends DFRef[DFMember.Empty]:
    val refType = classTag[DFMember.Empty]
    override def get(using getSet: MemberGetSet): DFMember.Empty = DFMember.Empty
  trait OneWay[+M <: DFMember] extends DFRef[M]
  object OneWay:
    object Empty extends OneWay[DFMember.Empty] with DFRef.Empty

  trait TwoWay[+M <: DFMember, +O <: DFMember] extends DFRef[M]:
    val originRefType: ClassTag[O @uncheckedVariance]
  type TwoWayAny = TwoWay[DFMember, DFMember]
  object TwoWay:
    object Empty extends TwoWay[DFMember.Empty, DFMember.Empty] with DFRef.Empty:
      val originRefType = classTag[DFMember.Empty]

  def unapply[M <: DFMember](ref: DFRef[M])(using MemberGetSet): Option[M] = Some(ref.get)
end DFRef

type IntParamRef = DFRef.TwoWay[DFVal, DFVal] | Int
extension (intParamRef: IntParamRef)
  def int = intParamRef.asInstanceOf[Int]
  def intOpt: Option[Int] = intParamRef match
    case int: Int => Some(int)
    case _        => None
  def ref: DFRef.TwoWay[DFVal, DFVal] = intParamRef.asInstanceOf[DFRef.TwoWay[DFVal, DFVal]]
  def refOpt: Option[DFRef.TwoWay[DFVal, DFVal]] = intParamRef match
    case ref: DFRef.TwoWay[DFVal, DFVal] => Some(ref)
    case _                               => None
