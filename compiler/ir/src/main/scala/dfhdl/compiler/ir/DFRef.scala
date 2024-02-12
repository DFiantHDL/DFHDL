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
  def getInt(using MemberGetSet): Int = (intParamRef: @unchecked) match
    case Int(int) => int
  def ref: DFRef.TwoWay[DFVal, DFVal] = intParamRef.asInstanceOf[DFRef.TwoWay[DFVal, DFVal]]
  def refOpt: Option[DFRef.TwoWay[DFVal, DFVal]] = intParamRef match
    case ref: DFRef.TwoWay[DFVal, DFVal] => Some(ref)
    case _                               => None
  def =~(that: IntParamRef)(using MemberGetSet): Boolean =
    (intParamRef, that) match
      case (thisRef: DFRef.TwoWay[DFVal, DFVal], thatRef: DFRef.TwoWay[DFVal, DFVal]) =>
        thisRef =~ thatRef
      case (thisInt: Int, thatInt: Int) => thisInt == thatInt
      case _                            => false
end extension
extension (intCompanion: Int.type)
  def unapply(intParamRef: IntParamRef)(using MemberGetSet): Option[Int] =
    (intParamRef: @unchecked) match
      case int: Int => Some(int)
      case DFRef(dfVal: DFVal) =>
        dfVal.getParamData.asInstanceOf[Option[Option[BigInt]]].flatten.map(_.toInt)
