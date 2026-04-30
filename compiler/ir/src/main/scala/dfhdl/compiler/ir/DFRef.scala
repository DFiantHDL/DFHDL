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
    def getIntConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Int] =
      intParamRef.runtimeChecked match
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
      compare(that)(_ == _).getOrElse(false)
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
          // Strip type-preserving AsIs wrappers and DesignParams whose owner
          // design has a parent (i.e., is not the top design). For non-top
          // designs, the parameter was provided by the instantiating parent —
          // resolve it via `appliedOrDefaultVal`. Params on a top design have
          // no parent and stay opaque: they are the symbolic free variables
          // exposed to the user at elaboration time.
          def strip(v: DFVal): DFVal = v match
            case DFVal.Alias.AsIs(dfType = dt, relValRef = DFRef(relVal))
                if dt == relVal.dfType =>
              strip(relVal)
            case dp: DFVal.DesignParam if !dp.getOwnerDesign.isTop =>
              strip(dp.appliedOrDefaultVal)
            case _ => v
          object ConstInt:
            def unapply(v: DFVal): Option[Int] = v match
              case c: DFVal.Const =>
                c.data match
                  case Some(i: BigInt) => Some(i.toInt)
                  case _               => None
              case _ => None
          // Decompose into (list of non-constant base terms, integer offset).
          def decompose(v: DFVal): (List[DFVal], Int) = strip(v) match
            case ConstInt(i)                            => (Nil, i)
            case DFVal.Func(op = FuncOp.+, args = args) =>
              args.map(_.get).foldLeft((List.empty[DFVal], 0)) { case ((bases, off), arg) =>
                val (argBases, argOff) = decompose(arg)
                (bases ++ argBases, off + argOff)
              }
            case DFVal.Func(op = FuncOp.-, args = List(aRef, bRef)) =>
              decompose(bRef.get) match
                case (Nil, k) =>
                  val (aBases, aOff) = decompose(aRef.get)
                  (aBases, aOff - k)
                case _ => (List(v), 0)
            case other => (List(other), 0)
          // Deep structural equality that keeps stripping at every level,
          // including through Func args — so `clog2(childParam)` matches
          // `clog2(parentParam)` once `childParam` has been resolved to
          // `parentParam` via `appliedOrDefaultVal`.
          def structEq(a: DFVal, b: DFVal): Boolean =
            val sa = strip(a)
            val sb = strip(b)
            (sa, sb) match
              case (af: DFVal.Func, bf: DFVal.Func) =>
                af.op == bf.op && af.dfType =~ bf.dfType &&
                af.args.length == bf.args.length &&
                af.args.lazyZip(bf.args).forall((ar, br) => structEq(ar.get, br.get))
              case _ => sa =~ sb
          // Check whether two base lists are equivalent as multi-sets under
          // deep structural equality after stripping.
          def basesMatch(lhs: List[DFVal], rhs: List[DFVal]): Boolean =
            lhs.length == rhs.length && {
              val remaining = mutable.ListBuffer.from(rhs)
              lhs.forall { l =>
                remaining.indexWhere(structEq(_, l)) match
                  case -1 => false
                  case i  => remaining.remove(i); true
              }
            }
          def asDFVal(ref: IntParamRef): Option[DFVal] = ref match
            case i: Int =>
              Some(DFVal.Const(
                DFInt32, Some(BigInt(i)), DFRef.OneWay.Empty, Meta.empty, DFTags.empty
              ))
            case r: DFRef.TypeRef => r.getOption
          for
            lVal <- asDFVal(intParamRef)
            rVal <- asDFVal(that)
            (lBases, lOff) = decompose(lVal)
            (rBases, rOff) = decompose(rVal)
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
