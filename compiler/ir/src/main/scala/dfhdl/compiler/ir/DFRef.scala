package dfhdl.compiler.ir
import scala.annotation.unchecked.uncheckedVariance
import dfhdl.internals.hashString
import upickle.default.*
import scala.collection.mutable
import scala.collection.immutable.ListMap

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
    override def copyAsNewRef(using RefGen): this.type = this
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

// A reference to a design block used as a stable structural key — the `subDBs`
// key, `DFDesignInst.designRef`, and `DFInterface.interfaceRef`. Fully opaque (no
// `<: OneWay[...]` bound) so it is NOT a regular reference: it cannot be resolved,
// freshened, or placed in a refTable by accident. Use `.asRef` for the deliberate,
// explicit unwrap when the underlying design-block reference is genuinely needed.
into opaque type StaticRef = DFRef.OneWay[DFDesignBlock]
object StaticRef:
  // The source is `DFOwner.Ref` (the broad owner-ref type) rather than just
  // `OneWay[DFDesignBlock]`: a design block's `ownerRef` is unified with its
  // instantiating `designRef` (a design-block key) but is typed as `DFOwner.Ref`,
  // so this also re-tags those `subDBs` keys/lookups. `designRef`/`interfaceRef`
  // (`OneWay[DFDesignBlock] <: DFOwner.Ref`) convert through the same path.
  def apply(ref: DFOwner.Ref): StaticRef = ref.asInstanceOf[StaticRef]
  given Conversion[DFOwner.Ref, StaticRef] = apply(_)
  given CanEqual[StaticRef, StaticRef] = CanEqual.derived
  // Opaque, so it does not inherit the `[T <: DFRefAny]` ReadWriter; provide its own
  // (the underlying ref serializes with the standard DFRef string format).
  given ReadWriter[StaticRef] =
    summon[ReadWriter[DFRefAny]].asInstanceOf[ReadWriter[StaticRef]]
  // The deliberate, explicit unwrap to the underlying design-block reference.
  extension (ref: StaticRef)
    def asRef: DFRef.OneWay[DFDesignBlock] = ref
    // Resolve the referenced design block. A unified hierarchy key is a design's
    // `ownerRef` token, so it resolves STRUCTURALLY through the context's design registry
    // (`getDesignBlockByKey`: the mutable run's design map, the root's `subDBs`, or a
    // flat DB's `designBlockByKey`) and never through the refTable, where the same token
    // maps to the design's OWNER. Shared by every member kind that carries a design-block
    // key: `DFDesignInst.designRef` and a method call's `DFVal.Func.Op.Def`.
    def getDesignBlock(using getSet: MemberGetSet): DFDesignBlock =
      getSet.getDesignBlockByKey(ref)
end StaticRef

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
    // Symbolic equivalence is decided by `IntExprCalc`, so e.g. `2 * W`
    // matches `W + W` and `max(W, W + 1)` matches `W + 1`.
    // With `elimSymbolicMaxMin`, a mixed max/min additionally reduces to its
    // constant operands (`max(W, 16)` reads as `16`), so a width-fit decision
    // such as `16 >= max(W, 16)` answers definitively; this deliberately
    // discards the symbolic case, so it is only for check sites that accept
    // by that rule, never for equality/similarity (see `IntExprCalc.constDiff`).
    def compare(that: IntParamRef, elimSymbolicMaxMin: Boolean = false)(
        func: (Int, Int) => Boolean
    )(using
        MemberGetSet
    ): Option[Boolean] =
      (intParamRef, that) match
        // Fast path: both refs are already concrete Ints.
        case (l: Int, r: Int) => Some(func(l, r))
        case _                =>
          def asDFVal(ref: IntParamRef): Option[DFVal] = ref match
            case i: Int =>
              Some(DFVal.Const(
                DFInt32, Some(BigInt(i)), DFRef.OneWay.Empty, Meta.empty, DFTags.empty
              ))
            case r: DFRef.TypeRef => r.getOption
          for
            lVal <- asDFVal(intParamRef)
            rVal <- asDFVal(that)
            diff <- IntExprCalc.constDiff(
              lVal,
              rVal,
              resolveDesignParams = true,
              elimSymbolicMaxMin
            )
          yield func(diff, 0)
    end compare
    // Width-fit decision `this >= that` (see `IntExprCalc.widthFitCompare`): the constant
    // difference rule of `compare` (with the max/min symbolic elimination) plus a
    // non-negativity proof over the validity domain, where both sides are widths and hence
    // `>= 1` for every valid elaboration. Relations that hold for every valid parameter
    // assignment (e.g. `2 * W >= W`) are accepted, and provably violated ones
    // (e.g. `W >= 2 * W`) decide `Some(false)`. Width-fit check sites only, never for
    // equality/similarity.
    def widthFitGE(that: IntParamRef)(using MemberGetSet): Option[Boolean] =
      (intParamRef, that) match
        // Fast path: both refs are already concrete Ints.
        case (l: Int, r: Int) => Some(l >= r)
        case _                =>
          def asDFVal(ref: IntParamRef): Option[DFVal] = ref match
            case i: Int =>
              Some(DFVal.Const(
                DFInt32, Some(BigInt(i)), DFRef.OneWay.Empty, Meta.empty, DFTags.empty
              ))
            case r: DFRef.TypeRef => r.getOption
          for
            lVal <- asDFVal(intParamRef)
            rVal <- asDFVal(that)
            decision <- IntExprCalc.widthFitCompare(lVal, rVal)
          yield decision
    end widthFitGE
    // The constant difference `this - that` when all symbolic terms cancel (see `compare`);
    // `None` otherwise. Lets printers render a widening as a relative extension (`.eby(k)`,
    // `EBY_U`/`EBY_S`, VHDL `eby`) exactly when the width delta folds to a literal. Design
    // parameters stay OPAQUE here: the relative form is only used when the delta holds for
    // every parameter assignment, or a printed `.eby(k)` would pin an overridable width to
    // its currently applied value.
    def constDiffFrom(that: IntParamRef)(using MemberGetSet): Option[Int] =
      (intParamRef, that) match
        case (l: Int, r: Int) => Some(l - r)
        case _                =>
          def asDFVal(ref: IntParamRef): Option[DFVal] = ref match
            case i: Int =>
              Some(DFVal.Const(
                DFInt32, Some(BigInt(i)), DFRef.OneWay.Empty, Meta.empty, DFTags.empty
              ))
            case r: DFRef.TypeRef => r.getOption
          for
            lVal <- asDFVal(intParamRef)
            rVal <- asDFVal(that)
            diff <- IntExprCalc.constDiff(lVal, rVal, resolveDesignParams = false)
          yield diff
    end constDiffFrom
    // The literal widening delta `this - that` for printers preferring the RELATIVE
    // extension spelling (`.eby(k)`, `EBY_U`/`EBY_S`, VHDL `eby`): defined when `this` is
    // a LITERAL width sitting `k` above a literal source width (a widening between
    // literal widths carries no spelling in the IR, and the relative form is the
    // canonical one), or when `this` is an ANONYMOUS `base + k` width increment whose
    // base is the source width itself (e.g. a `W + 1` target over a `W`-wide source). A
    // NAMED width (a parameter or named constant) or any other expression shape prints
    // absolutely, by name, so the width symbols the user can see are preserved.
    def widenDeltaOpt(that: IntParamRef)(using MemberGetSet): Option[Int] =
      intParamRef match
        case _: Int             => constDiffFrom(that).filter(_ > 0)
        case ref: DFRef.TypeRef =>
          ref.getOption match
            case Some(func: DFVal.Func) if func.isAnonymous && func.op == DFVal.Func.Op.+ =>
              func.args match
                case baseRef :: DFRef(konst: DFVal.Const) :: Nil =>
                  konst.data match
                    case Some(k: BigInt) if k > 0 =>
                      val thatValOpt: Option[DFVal] = that match
                        case thatRef: DFRef.TypeRef => thatRef.getOption
                        case i: Int                 =>
                          Some(DFVal.Const(
                            DFInt32, Some(BigInt(i)), DFRef.OneWay.Empty, Meta.empty, DFTags.empty
                          ))
                      val baseEquiv = thatValOpt.exists { thatVal =>
                        (thatVal == baseRef.get) ||
                        (IntExprCalc.constDiff(
                          baseRef.get,
                          thatVal,
                          resolveDesignParams = false
                        ) == Some(0))
                      }
                      if (baseEquiv) Some(k.toInt) else None
                    case _ => None
                case _ => None
            case _ => None
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
    val db = getSet.designDB
    // The hierarchical root holds no members/refTable of its own (all content
    // lives in the sub-DBs), so aggregate across every sub-DB. A non-root DB
    // (flat or a single sub-DB) is handled exactly as before.
    val rt =
      if (db.isRoot) db.subDBs.values.view.flatMap(_.refTable).toMap
      else db.refTable
    val members =
      if (db.isRoot) db.subDBs.values.view.flatMap(_.members).toList
      else db.members
    val grpId = rt.last._1.grpId
    val lastId = rt.keys.map(_.id).max
    val magnetID = members.view.collect {
      case DFOpaque.Val(dfType) if dfType.isMagnet => dfType.id
    }.maxOption.getOrElse(0)
    RefGen(magnetID, grpId, lastId)
  end fromGetSet
end RefGen
