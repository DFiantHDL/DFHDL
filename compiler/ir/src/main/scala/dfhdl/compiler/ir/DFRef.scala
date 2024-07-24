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

  trait TypeRef extends TwoWay[DFVal, DFVal]:
    val refType = classTag[DFVal]
    val originRefType = classTag[DFVal]

  def unapply[M <: DFMember](ref: DFRef[M])(using MemberGetSet): Option[M] = Some(ref.get)
end DFRef

opaque type IntParamRef = DFRef.TypeRef | Int
object IntParamRef:
  def apply(int: Int): IntParamRef = int
  def apply(ref: DFRef.TypeRef): IntParamRef = ref
  extension (intParamRef: IntParamRef)
    def isInt: Boolean = intParamRef match
      case int: Int => true
      case _        => false
    def getInt(using MemberGetSet): Int = (intParamRef: @unchecked) match
      case Int(int) => int
    def isRef: Boolean = intParamRef match
      case ref: DFRef.TypeRef => true
      case _                  => false
    def getRef: Option[DFRef.TypeRef] = intParamRef match
      case ref: DFRef.TypeRef => Some(ref)
      case _                  => None
    def =~(that: IntParamRef)(using MemberGetSet): Boolean =
      (intParamRef, that) match
        case (thisRef: DFRef.TypeRef, thatRef: DFRef.TypeRef) =>
          thisRef =~ thatRef
        case (thisInt: Int, thatInt: Int) => thisInt == thatInt
        case _                            => false
  end extension
end IntParamRef
extension (intCompanion: Int.type)
  def unapply(intParamRef: IntParamRef)(using MemberGetSet): Option[Int] =
    (intParamRef: @unchecked) match
      case int: Int => Some(int)
      case DFRef(dfVal: DFVal) =>
        dfVal.getConstData.asInstanceOf[Option[Option[BigInt]]].flatten.map(_.toInt)
