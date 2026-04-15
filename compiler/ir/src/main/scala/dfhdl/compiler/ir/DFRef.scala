package dfhdl.compiler.ir
import scala.annotation.unchecked.uncheckedVariance
import dfhdl.internals.hashString
import upickle.default.*
import scala.collection.mutable
import scala.collection.immutable.ListMap
import DFVal.Func.Op as FuncOp

type DFRefAny = DFRef[DFMember]
sealed trait DFRef[+M <: DFMember] extends Product, Serializable derives CanEqual:
  val grpId: (Int, Int)
  val id: Int
  final def =~(that: DFRefAny)(using MemberGetSet): Boolean = this.get =~ that.get
  def get(using getSet: MemberGetSet): M = getSet(this)
  def getOption(using getSet: MemberGetSet): Option[M] = getSet.getOption(this)
  def copyAsNewRef(using RefGen): this.type
  override def toString: String = write(this)

object DFRef:
  sealed trait Empty extends DFRef[DFMember.Empty]:
    val grpId: (Int, Int) = (0, 0)
    val id: Int = 0
    override def get(using getSet: MemberGetSet): DFMember.Empty = DFMember.Empty
  sealed trait OneWay[+M <: DFMember] extends DFRef[M]:
    def copyAsNewRef(using refGen: RefGen): this.type =
      refGen.genOneWay[M].asInstanceOf[this.type]
  object OneWay:
    final case class Gen[M <: DFMember](grpId: (Int, Int), id: Int) extends OneWay[M]
    case object Empty extends OneWay[DFMember.Empty] with DFRef.Empty

  final case class DuplicationRef(owner: DFOwnerNamed) extends OneWay[DFOwnerNamed]:
    val grpId: (Int, Int) = (-1, -1)
    val id: Int = -1
    override def get(using getSet: MemberGetSet): DFOwnerNamed = owner
    override def getOption(using getSet: MemberGetSet): Option[DFOwnerNamed] = Some(owner)
    override def copyAsNewRef(using refGen: RefGen): this.type = this

  sealed trait TwoWay[+M <: DFMember, +O <: DFMember] extends DFRef[M]:
    def copyAsNewRef(using refGen: RefGen): this.type =
      refGen.genTwoWay[M, O].asInstanceOf[this.type]
  type TwoWayAny = TwoWay[DFMember, DFMember]
  object TwoWay:
    final case class Gen[M <: DFMember, O <: DFMember](grpId: (Int, Int), id: Int)
        extends TwoWay[M, O]
    case object Empty extends TwoWay[DFMember.Empty, DFMember.Empty] with DFRef.Empty

  final case class TypeRef(grpId: (Int, Int), id: Int)
      extends TwoWay[DFVal.CanBeExpr, DFVal.CanBeExpr]:
    override def copyAsNewRef(using refGen: RefGen): this.type =
      refGen.genTypeRef.asInstanceOf[this.type]

  extension (list: List[DFRefAny])
    def =~(that: List[DFRefAny])(using MemberGetSet): Boolean =
      list.length == that.length && list.lazyZip(that).forall(_ =~ _)

  extension (list: ListMap[String, DFRefAny])
    def =~(that: ListMap[String, DFRefAny])(using MemberGetSet): Boolean =
      list.size == that.size && list.lazyZip(that).forall {
        case ((k1, v1), (k2, v2)) => k1 == k2 && v1 =~ v2
      }

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
          case TypeRef(grpId, id)    => s"TR_${grpId._1.toHexString}_${grpId._2.toHexString}_${id}"
          case TwoWay.Gen(grpId, id) => s"TW_${grpId._1.toHexString}_${grpId._2.toHexString}_${id}"
          case OneWay.Gen(grpId, id) => s"OW_${grpId._1.toHexString}_${grpId._2.toHexString}_${id}"
          case _: DuplicationRef     =>
            throw new IllegalArgumentException("DuplicationRef must never be serialized")
      ,
      str =>
        if str == "TWE" then TwoWay.Empty.asInstanceOf[T]
        else if str == "OWE" then OneWay.Empty.asInstanceOf[T]
        else
          val parts = str.split("_")
          parts(0) match
            case "TR" =>
              TypeRef(
                (Integer.parseUnsignedInt(parts(1), 16), Integer.parseUnsignedInt(parts(2), 16)),
                parts(3).toInt
              ).asInstanceOf[T]
            case "TW" =>
              TwoWay.Gen(
                (Integer.parseUnsignedInt(parts(1), 16), Integer.parseUnsignedInt(parts(2), 16)),
                parts(3).toInt
              ).asInstanceOf[T]
            case "OW" =>
              OneWay.Gen(
                (Integer.parseUnsignedInt(parts(1), 16), Integer.parseUnsignedInt(parts(2), 16)),
                parts(3).toInt
              ).asInstanceOf[T]
            case _ => throw new IllegalArgumentException(s"Unknown reference format: $str")
          end match
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
    def getIntUNSAFE(using MemberGetSet): Int = getIntOpt.get
    def getIntConstData(using MemberGetSet): ConstData[Int] = intParamRef.runtimeChecked match
      case int: Int            => ConstData.KnownConst(int)
      case DFRef(dfVal: DFVal) =>
        dfVal.getConstData[Option[BigInt]] match
          case ConstData.KnownConst(Some(i: BigInt)) => ConstData.KnownConst(i.toInt)
          case ConstData.UnknownConst(dfVal)         => ConstData.UnknownConst(dfVal)
          case _                                     => ConstData.NotConst
    def getIntOpt(using MemberGetSet): Option[Int] = getIntConstData match
      case ConstData.KnownConst(i: Int) => Some(i)
      case _                            => None
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
        case (thisInt: Int, thatInt: Int)           => thisInt == thatInt
        case (thisRef: DFRef.TypeRef, thatInt: Int) =>
          thisRef.get.isSimilarTo(fakeConst(thatInt))
        case (thisInt: Int, thatRef: DFRef.TypeRef) =>
          thatRef.get.isSimilarTo(fakeConst(thisInt))
    def copyAsNewRef(using RefGen): IntParamRef = intParamRef match
      case ref: DFRef.TypeRef => ref.copyAsNewRef
      case _                  => intParamRef
    // Compares two parametric integer references via the given comparator.
    // If both reduce to a concrete Int, or their symbolic parts cancel under
    // subtraction, returns `Some(func(diff, 0))` where `diff = this - that`.
    // Returns `None` when the unknown parts don't cancel.
    def compare(that: IntParamRef)(func: (Int, Int) => Boolean)(using
        MemberGetSet
    ): Option[Boolean] =
      (intParamRef, that) match
        // Fast path: both refs are already concrete Ints.
        case (l: Int, r: Int) => Some(func(l, r))
        case _                =>
          object ConstInt:
            def unapply(v: DFVal): Option[Int] = v match
              case c: DFVal.Const =>
                c.data match
                  case Some(i: BigInt) => Some(i.toInt)
                  case _               => None
              case _ => None
          // Decompose into (list of non-constant base terms, integer offset).
          def decompose(v: DFVal): (List[DFVal], Int) = v match
            case ConstInt(i)                            => (Nil, i)
            case DFVal.Func(op = FuncOp.+, args = args) =>
              args.map(_.get).foldLeft((List.empty[DFVal], 0)) { case ((bases, off), arg) =>
                val (argBases, argOff) = decompose(arg)
                (bases ++ argBases, off + argOff)
              }
            case DFVal.Func(op = FuncOp.-, args = List(aRef, bRef)) =>
              (aRef.get, bRef.get) match
                case (other, ConstInt(k)) =>
                  val (bases, off) = decompose(other)
                  (bases, off - k)
                case _ => (List(v), 0)
            case _ => (List(v), 0)
          def reduce(ref: IntParamRef): Option[(List[DFVal], Int)] =
            ref.getIntConstData match
              case ConstData.KnownConst(i)    => Some((Nil, i))
              case ConstData.UnknownConst(dv) => Some(decompose(dv))
              case ConstData.NotConst         => None
          // Check whether two base lists are equivalent as multi-sets under
          // structural similarity.
          def basesMatch(lhs: List[DFVal], rhs: List[DFVal]): Boolean =
            lhs.length == rhs.length && {
              val remaining = scala.collection.mutable.ListBuffer.from(rhs)
              lhs.forall { l =>
                remaining.indexWhere(_.isSimilarTo(l)) match
                  case -1 => false
                  case i  => remaining.remove(i); true
              }
            }
          for
            (lBases, lOff) <- reduce(intParamRef)
            (rBases, rOff) <- reduce(that)
            if basesMatch(lBases, rBases)
          yield func(lOff - rOff, 0)
    end compare
  end extension

  given ReadWriter[IntParamRef] = readwriter[ujson.Value].bimap(
    param =>
      param match
        case int: Int           => writeJs(int)
        case ref: DFRef.TypeRef => writeJs(ref)
    ,
    json =>
      json match
        case ujson.Num(n) => n.toInt
        case ujson.Str(_) => read[DFRef.TypeRef](json)
        case _ => throw new IllegalArgumentException(s"Expected String or Int, got $json")
  )
end IntParamRef

object IntUNSAFE:
  def unapply(intParamRef: IntParamRef)(using MemberGetSet): Option[Int] =
    intParamRef.runtimeChecked match
      case int: Int            => Some(int)
      case DFRef(dfVal: DFVal) =>
        dfVal.getConstData[Option[BigInt]].toOption.flatten.map(_.toInt)

class RefGen private (
    private var magnetID: Int,
    private var grpId: (Int, Int),
    private var lastId: Int
) extends Serializable:
  private def nextMagnetID: Int =
    val newId = magnetID + 1
    magnetID = newId
    newId
  private def nextId: Int =
    val newId = lastId + 1
    lastId = newId
    newId
  private val magnetIDMap = mutable.Map.empty[Product, Int]
  def getMagnetID(t: Product): Int = magnetIDMap.getOrElseUpdate(t, nextMagnetID)
  def getGrpId: (Int, Int) = grpId
  def setGrpId(newGrpId: (Int, Int)): Unit =
    grpId = newGrpId
  def genOneWay[M <: DFMember]: DFRef.OneWay[M] = DFRef.OneWay.Gen(grpId, nextId)
  def genTwoWay[M <: DFMember, O <: DFMember]: DFRef.TwoWay[M, O] = DFRef.TwoWay.Gen(grpId, nextId)
  def genTypeRef: DFRef.TypeRef = DFRef.TypeRef(grpId, nextId)
end RefGen

object RefGen:
  def initial: RefGen = RefGen(0, (0, 0), 0)
  def fromGetSet(using getSet: MemberGetSet): RefGen =
    val rt = getSet.designDB.refTable
    val grpId = rt.last._1.grpId
    val lastId = rt.keys.map(_.id).max
    val magnetID = getSet.designDB.members.view.collect {
      case DFOpaque.Val(dfType) if dfType.isMagnet => dfType.id
    }.maxOption.getOrElse(0)
    RefGen(magnetID, grpId, lastId)
