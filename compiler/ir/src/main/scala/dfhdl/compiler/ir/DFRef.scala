package dfhdl.compiler.ir
import scala.annotation.unchecked.uncheckedVariance
import dfhdl.internals.hashString
import upickle.default.*

type DFRefAny = DFRef[DFMember]
sealed trait DFRef[+M <: DFMember] extends Product, Serializable derives CanEqual:
  val grpId: Int
  val id: Int
  final def =~(that: DFRefAny)(using MemberGetSet): Boolean = this.get =~ that.get
  def get(using getSet: MemberGetSet): M = getSet(this)
  def getOption(using getSet: MemberGetSet): Option[M] = getSet.getOption(this)
  def copyAsNewRef(using RefGen): this.type
  override def toString: String = s"<${this.hashString}>"

object DFRef:
  sealed trait Empty extends DFRef[DFMember.Empty]:
    val grpId: Int = 0
    val id: Int = 0
    override def get(using getSet: MemberGetSet): DFMember.Empty = DFMember.Empty
  sealed trait OneWay[+M <: DFMember] extends DFRef[M]:
    final def copyAsNewRef(using refGen: RefGen): this.type =
      refGen.genOneWay[M].asInstanceOf[this.type]
  object OneWay:
    final case class Gen[M <: DFMember](grpId: Int, id: Int) extends OneWay[M]
    case object Empty extends OneWay[DFMember.Empty] with DFRef.Empty

  sealed trait TwoWay[+M <: DFMember, +O <: DFMember] extends DFRef[M]:
    def copyAsNewRef(using refGen: RefGen): this.type =
      refGen.genTwoWay[M, O].asInstanceOf[this.type]
  type TwoWayAny = TwoWay[DFMember, DFMember]
  object TwoWay:
    final case class Gen[M <: DFMember, O <: DFMember](grpId: Int, id: Int) extends TwoWay[M, O]
    case object Empty extends TwoWay[DFMember.Empty, DFMember.Empty] with DFRef.Empty

  final case class TypeRef(grpId: Int, id: Int) extends TwoWay[DFVal.CanBeExpr, DFVal.CanBeExpr]:
    override def copyAsNewRef(using refGen: RefGen): this.type =
      refGen.genTypeRef.asInstanceOf[this.type]

  extension (ref: DFRefAny)
    def isTypeRef: Boolean = ref match
      case ref: TypeRef => true
      case _            => false
  def unapply[M <: DFMember](ref: DFRef[M])(using MemberGetSet): Option[M] = Some(ref.get)

  given [T <: DFRefAny]: ReadWriter[T] =
    readwriter[String].bimap(
      ref =>
        ref match
          case TwoWay.Empty          => "TWE"
          case OneWay.Empty          => "OWE"
          case TypeRef(grpId, id)    => s"TR_${grpId}_${id}"
          case TwoWay.Gen(grpId, id) => s"TW_${grpId}_${id}"
          case OneWay.Gen(grpId, id) => s"OW_${grpId}_${id}"
      ,
      str =>
        if str == "TWE" then TwoWay.Empty.asInstanceOf[T]
        else if str == "OWE" then OneWay.Empty.asInstanceOf[T]
        else
          val parts = str.split("_")
          parts(0) match
            case "TR" => TypeRef(parts(1).toInt, parts(2).toInt).asInstanceOf[T]
            case "TW" => TwoWay.Gen(parts(1).toInt, parts(2).toInt).asInstanceOf[T]
            case "OW" => OneWay.Gen(parts(1).toInt, parts(2).toInt).asInstanceOf[T]
            case _    => throw new IllegalArgumentException(s"Unknown reference format: $str")
    )
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
    def copyAsNewRef(using RefGen): IntParamRef = intParamRef match
      case ref: DFRef.TypeRef => ref.copyAsNewRef
      case _                  => intParamRef
  end extension

  given ReadWriter[IntParamRef] = readwriter[ujson.Value].bimap(
    param =>
      param match
        case int: Int           => int
        case ref: DFRef.TypeRef => write(ref.asInstanceOf[DFRefAny])
    ,
    json =>
      json match
        case ujson.Str(s) => read[DFRefAny](s).asInstanceOf[IntParamRef]
        case ujson.Num(n) => n.toInt
        case _ => throw new IllegalArgumentException(s"Expected String or Int, got $json")
  )
end IntParamRef

extension (intCompanion: Int.type)
  def unapply(intParamRef: IntParamRef)(using MemberGetSet): Option[Int] =
    (intParamRef: @unchecked) match
      case int: Int => Some(int)
      case DFRef(dfVal: DFVal) =>
        dfVal.getConstData.asInstanceOf[Option[Option[BigInt]]].flatten.map(_.toInt)

class RefGen(private var grpId: Int, private var lastId: Int):
  private def nextId: Int =
    val newId = lastId + 1
    lastId = newId
    newId
  def getGrpId: Int = grpId
  def setGrpId(newGrpId: Int): Unit =
    grpId = newGrpId
  def genOneWay[M <: DFMember]: DFRef.OneWay[M] = DFRef.OneWay.Gen(grpId, nextId)
  def genTwoWay[M <: DFMember, O <: DFMember]: DFRef.TwoWay[M, O] = DFRef.TwoWay.Gen(grpId, nextId)
  def genTypeRef: DFRef.TypeRef = DFRef.TypeRef(grpId, nextId)

object RefGen:
  def fromGetSet(using getSet: MemberGetSet): RefGen =
    val rt = getSet.designDB.refTable
    val grpId = rt.last._1.grpId
    val lastId = rt.keys.map(_.id).max
    RefGen(grpId, lastId)
