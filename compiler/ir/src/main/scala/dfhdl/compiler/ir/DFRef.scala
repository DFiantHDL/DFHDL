package dfhdl.compiler.ir
import scala.annotation.unchecked.uncheckedVariance
import dfhdl.internals.hashString

type DFRefAny = DFRef[DFMember]
sealed trait DFRef[+M <: DFMember] derives CanEqual:
  final def =~(that: DFRefAny)(using MemberGetSet): Boolean = this.get =~ that.get
  def get(using getSet: MemberGetSet): M = getSet(this)
  def getOption(using getSet: MemberGetSet): Option[M] = getSet.getOption(this)
  def copyAsNewRef: this.type
  override def toString: String = s"<${this.hashString}>"

object DFRef:
  sealed trait Empty extends DFRef[DFMember.Empty]:
    override def get(using getSet: MemberGetSet): DFMember.Empty = DFMember.Empty
  trait OneWay[+M <: DFMember] extends DFRef[M]:
    self =>
    final def copyAsNewRef: this.type = new OneWay[M] {}.asInstanceOf[this.type]
  object OneWay:
    object Empty extends OneWay[DFMember.Empty] with DFRef.Empty

  trait TwoWay[+M <: DFMember, +O <: DFMember] extends DFRef[M]:
    def copyAsNewRef: this.type = new TwoWay[M, O] {}.asInstanceOf[this.type]
  type TwoWayAny = TwoWay[DFMember, DFMember]
  object TwoWay:
    object Empty extends TwoWay[DFMember.Empty, DFMember.Empty] with DFRef.Empty

  trait TypeRef extends TwoWay[DFVal.CanBeExpr, DFVal.CanBeExpr]:
    override def copyAsNewRef: this.type = new TypeRef {}.asInstanceOf[this.type]

  extension (ref: DFRefAny)
    def isTypeRef: Boolean = ref match
      case ref: TypeRef => true
      case _            => false
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
        case (thisRef: DFRef.TypeRef, thatRef: DFRef.TypeRef) => thisRef =~ thatRef
        case (thisInt: Int, thatInt: Int)                     => thisInt == thatInt
        case _                                                => false
    def isSimilarTo(that: IntParamRef)(using MemberGetSet): Boolean =
      def fakeConst(value: Int): DFVal.Const =
        DFVal.Const(DFInt32, Some(BigInt(value)), DFRef.OneWay.Empty, Meta.empty, DFTags.empty)
      (intParamRef, that) match
        case (thisRef: DFRef.TypeRef, thatRef: DFRef.TypeRef) =>
          thisRef.get.isSimilarTo(thatRef.get)
        case (thisInt: Int, thatInt: Int) => thisInt == thatInt
        case (thisRef: DFRef.TypeRef, thatInt: Int) =>
          thisRef.get.isSimilarTo(fakeConst(thatInt))
        case (thisInt: Int, thatRef: DFRef.TypeRef) =>
          thatRef.get.isSimilarTo(fakeConst(thisInt))
    def copyAsNewRef: IntParamRef = intParamRef match
      case ref: DFRef.TypeRef => ref.copyAsNewRef
      case _                  => intParamRef
  end extension
end IntParamRef
extension (intCompanion: Int.type)
  def unapply(intParamRef: IntParamRef)(using MemberGetSet): Option[Int] =
    (intParamRef: @unchecked) match
      case int: Int => Some(int)
      case DFRef(dfVal: DFVal) =>
        dfVal.getConstData.asInstanceOf[Option[Option[BigInt]]].flatten.map(_.toInt)
