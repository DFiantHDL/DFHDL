package dfhdl.compiler
package ir
import dfhdl.internals.*

import annotation.tailrec
import scala.collection.immutable.ListMap
import scala.reflect.{ClassTag, classTag}

sealed trait DFMember extends Product, Serializable, HasRefCompare[DFMember] derives CanEqual:
  val ownerRef: DFOwner.Ref
  val meta: Meta
  val tags: DFTags
  final def setMeta(metaFunc: Meta => Meta)(using getSet: MemberGetSet): this.type =
    getSet.set(this)(m => setMeta(metaFunc(m.meta)))
  final def setTags(tagsFunc: DFTags => DFTags)(using getSet: MemberGetSet): this.type =
    getSet.set(this)(m => setTags(tagsFunc(m.tags)))
  protected def setMeta(meta: Meta): this.type
  protected def setTags(tags: DFTags): this.type
  final def getOwner(using MemberGetSet): DFOwner = ownerRef.get match
    case o: DFOwner     => o
    case DFMember.Empty => throw new IllegalArgumentException(s"No owner found for member $this.")
  final def getOwnerNamed(using MemberGetSet): DFOwnerNamed = getOwner match
    case b: DFOwnerNamed => b
    case o               => o.getOwnerNamed
  final def getOwnerBlock(using MemberGetSet): DFBlock = getOwner match
    case b: DFBlock => b
    case o          => o.getOwnerBlock
  final def getOwnerDesign(using MemberGetSet): DFDesignBlock =
    getOwnerBlock match
      case d: DFDesignBlock => d
      case b: DFBlock       => b.getOwnerDesign
  final def getOwnerDomain(using MemberGetSet): DFDomainOwner = getOwner match
    case b: DFDomainOwner => b
    case o                => o.getOwnerDomain
  final def getThisOrOwnerDesign(using MemberGetSet): DFDesignBlock = this match
    case d: DFDesignBlock => d
    case x                => x.getOwnerDesign
  final def getThisOrOwnerNamed(using MemberGetSet): DFOwnerNamed = this match
    case d: DFOwnerNamed => d
    case x               => x.getOwnerNamed
  final infix def isMemberOf(that: DFOwnerNamed)(using MemberGetSet): Boolean =
    this match
      case DFDesignBlock.Top() => false
      case _                   => getOwnerNamed == that
  infix def isSameOwnerDesignAs(that: DFMember)(using MemberGetSet): Boolean =
    (this, that) match
      case (DFDesignBlock.Top(), DFDesignBlock.Top()) => this == that
      case (DFDesignBlock.Top(), _)                   => false
      case (_, DFDesignBlock.Top())                   => false
      case _                                          => getOwnerDesign == that.getOwnerDesign
  final infix def isOneLevelBelow(that: DFMember)(using MemberGetSet): Boolean =
    this match
      case DFDesignBlock.Top() => false
      case _                   => getOwnerDesign isSameOwnerDesignAs that
  // true if and only if the member is outside the design at any level
  final infix def isOutsideOwner(that: DFOwner)(using MemberGetSet): Boolean =
    !isInsideOwner(that)
  @tailrec private def isInsideOwner(thisMember: DFMember, thatOwner: DFOwner)(using
      MemberGetSet
  ): Boolean =
    thisMember match
      case DFDesignBlock.Top() => false
      case _ =>
        (thisMember.getOwner, thatOwner) match
          case (a, b) if a == b => true
          case (od, _)          => isInsideOwner(od, thatOwner)
  // true if and only if the member is inside the design at any level
  final infix def isInsideOwner(that: DFOwner)(using MemberGetSet): Boolean =
    isInsideOwner(this, that)
  final def getOwnerChain(using MemberGetSet): List[DFBlock] =
    this match
      case d @ DFDesignBlock.Top() => Nil
      case _ =>
        if (getOwnerBlock.isTop) List(getOwnerBlock)
        else getOwnerBlock.getOwnerChain :+ getOwnerBlock
end DFMember

object DFMember:
  extension (member: DFMember)
    def getTagOf[CT <: DFTag: ClassTag]: Option[CT] =
      member.tags.getTagOf[CT]
    def hasTagOf[CT <: DFTag: ClassTag]: Boolean =
      member.tags.hasTagOf[CT]

  type Empty = Empty.type
  case object Empty extends DFMember:
    val ownerRef: DFOwner.Ref = DFRef.OneWay.Empty
    val meta: Meta = Meta(None, Position.unknown, None, Nil)
    val tags: DFTags = DFTags.empty
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case Empty => true
      case _     => false
    protected def setMeta(meta: Meta): this.type = this
    protected def setTags(tags: DFTags): this.type = this
    def getRefs: List[DFRef.TwoWayAny] = Nil

  sealed trait Named extends DFMember:
    final def getName(using MemberGetSet): String = this match
      case o: DFDesignBlock if o.isTop => o.dclName
      case _                           => meta.name
    final lazy val isAnonymous: Boolean = meta.isAnonymous
    final def getFullName(using MemberGetSet): String = this match
      case o: DFDesignBlock if o.isTop => getName
      case _                           => s"${getOwnerNamed.getFullName}.${getName}"
    def getRelativeName(callOwner: DFOwner)(using MemberGetSet): String =
      val namedOwner = callOwner.getThisOrOwnerNamed
      if (this isMemberOf namedOwner) getName
      else if (getOwnerNamed isOneLevelBelow namedOwner) s"${getOwnerNamed.getName}.$getName"
      else if (callOwner isInsideOwner this.getOwnerNamed) getName
      else
        // more complex referencing just summons the two owner chains and compares them.
        // it is possible to do this more efficiently but the simple cases cover the most common usage anyway
        val memberChain = this.getOwnerChain.collect { case o: DFOwnerNamed => o }
        val ctxChain = namedOwner.getOwnerChain.collect { case o: DFOwnerNamed => o }
        val samePath = memberChain.lazyZip(ctxChain).count(_ == _)
        s"${memberChain.drop(samePath).map(_.getName).mkString(".")}.$getName"
    end getRelativeName
  end Named
  extension (member: Named)
    def stripPortSel(using MemberGetSet): Named = member match
      case portSel: DFVal.PortByNameSelect => portSel.getPortDcl
      case _                               => member
end DFMember

sealed trait DFVal extends DFMember.Named:
  val dfType: DFType
  def width(using MemberGetSet): Int = dfType.width
  def isGlobal: Boolean = false
  protected def protIsConst(using MemberGetSet): Boolean
  // using just an integer to escape redundant boxing Option[Boolean] would have achieved
  private var cachedIsConst: Int = -1
  final def isConst(using MemberGetSet): Boolean =
    if (cachedIsConst == -1)
      val localIsConst = protIsConst
      cachedIsConst = if (localIsConst) 1 else 0
      localIsConst
    else if (cachedIsConst > 0) true
    else false
end DFVal

object DFVal:
  type Ref = DFRef.TwoWay[DFVal, DFMember]
  enum Modifier derives CanEqual:
    case VAR, IN, OUT, INOUT

  extension (dfVal: DFVal)
    def isPort: Boolean = dfVal match
      case dcl: DFVal.Dcl =>
        dcl.modifier match
          case Modifier.IN | Modifier.OUT | Modifier.INOUT => true
          case _                                           => false
      case _ => false
    def isVar: Boolean = dfVal match
      case dcl: DFVal.Dcl =>
        dcl.modifier match
          case Modifier.VAR => true
          case _            => false
      case _ => false
    def isOpen: Boolean = dfVal match
      case _: Open => true
      case _       => false
    @tailrec def dealias(using MemberGetSet): Option[DFVal.Dcl | DFVal.Open] = dfVal match
      case dcl: DFVal.Dcl                           => Some(dcl)
      case portByNameSelect: DFVal.PortByNameSelect => Some(portByNameSelect.getPortDcl)
      case open: DFVal.Open                         => Some(open)
      case alias: DFVal.Alias                       => alias.relValRef.get.dealias
      case _                                        => None
    @tailrec private def departial(range: Range)(using MemberGetSet): (DFVal, Range) =
      extension (range: Range)
        def offset(delta: Int) =
          Range(range.start + delta, range.end + delta)
      dfVal match
        case portByNameSelect: DFVal.PortByNameSelect =>
          portByNameSelect.getPortDcl.departial(range)
        case partial: DFVal.Alias.Partial =>
          val relVal = partial.relValRef.get
          partial match
            case partial: DFVal.Alias.ApplyRange =>
              relVal.departial(range.offset(partial.relBitLow))
            case partial: DFVal.Alias.ApplyIdx =>
              partial match
                case DFVal.Alias.ApplyIdx.Const(idx) =>
                  relVal.departial(range.offset(idx * partial.width))
                // if not a constant index selection, then the entire value range is affected
                case _ =>
                  (relVal, range)
            case partial: DFVal.Alias.SelectField =>
              relVal.departial(
                range.offset(
                  relVal.dfType.asInstanceOf[DFStruct].fieldRelBitLow(partial.fieldName)
                )
              )
            case _ => relVal.departial(range)
          end match
        case _ => (dfVal, range)
      end match
    end departial
    // for a given value remove partial selections as possible
    def departial(using MemberGetSet): (DFVal, Range) = departial(0 until dfVal.width)
    def departialDcl(using MemberGetSet): Option[(DFVal.Dcl, Range)] =
      departial match
        case (dcl: DFVal.Dcl, range) => Some(dcl, range)
        case _                       => None
    def stripPortSel(using MemberGetSet): DFVal = dfVal match
      case portSel: DFVal.PortByNameSelect => portSel.getPortDcl
      case _                               => dfVal
    def getParamData(using MemberGetSet): Option[Any] =
      dfVal match
        case const: DFVal.Const => Some(const.data)
        case func: DFVal.Func =>
          val args = func.args.map(_.get)
          val argData = args.flatMap(_.getParamData)
          val argTypes = args.map(_.dfType)
          if (argData.length != func.args.length) None
          else Some(calcFuncData(func.dfType, func.op, argTypes, argData))
        case alias: DFVal.Alias =>
          val relVal = alias.relValRef.get
          relVal.getParamData match
            case Some(relValData) =>
              alias match
                case alias: DFVal.Alias.AsIs =>
                  Some(
                    dataConversion(alias.dfType, relVal.dfType)(
                      relValData.asInstanceOf[relVal.dfType.Data]
                    )
                  )
                case alias: DFVal.Alias.ApplyRange =>
                  Some(
                    selBitRangeData(
                      relValData.asInstanceOf[(BitVector, BitVector)],
                      alias.relBitHigh,
                      alias.relBitLow
                    )
                  )
                case alias: DFVal.Alias.ApplyIdx =>
                  val relIdx = alias.relIdx.get
                  relIdx.getParamData match
                    case Some(Some(idx: BigInt)) =>
                      val idxInt = idx.toInt
                      val outData = relVal.dfType match
                        case DFBits(_) =>
                          val data = relValData.asInstanceOf[(BitVector, BitVector)]
                          if (data._2.bit(idxInt)) None
                          else Some(data._1.bit(idxInt))
                        case DFVector(_, _) =>
                          Some(relValData.asInstanceOf[Vector[?]](idxInt))
                        case _ => ???
                      Some(outData)
                    case Some(_: None.type) => Some(None)
                    case _                  => None
                case alias: DFVal.Alias.History => None
                case alias: DFVal.Alias.SelectField =>
                  val idx = relVal.dfType.asInstanceOf[DFStruct].fieldRelBitLow(alias.fieldName)
                  Some(relValData.asInstanceOf[List[?]](idx))
            case None => None
          end match
        case _ => None
      end match
    end getParamData
  end extension
  // can be an expression
  sealed trait CanBeExpr extends DFVal

  // can be a global value
  sealed trait CanBeGlobal extends CanBeExpr:
    private[dfhdl] var globalCtx: Any = compiletime.uninitialized
    final override def isGlobal: Boolean = ownerRef.refType equals classTag[DFMember.Empty]
    final override def getRelativeName(callOwner: DFOwner)(using MemberGetSet): String =
      if (isGlobal) this.getName
      else super.getRelativeName(callOwner)
    final override infix def isSameOwnerDesignAs(that: DFMember)(using MemberGetSet): Boolean =
      if (this.isGlobal) false
      else
        that match
          case thatDFVal: DFVal if thatDFVal.isGlobal => false
          case _                                      => super.isSameOwnerDesignAs(that)

  final case class Const(
      dfType: DFType,
      data: Any,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends CanBeExpr,
        CanBeGlobal:
    protected def protIsConst(using MemberGetSet): Boolean = true
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Const =>
        given CanEqual[Any, Any] = CanEqual.derived
        this.dfType =~ that.dfType && this.data == that.data &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = dfType.getRefs
  end Const

  final case class Open(
      dfType: DFType,
      ownerRef: DFOwner.Ref
  ) extends DFVal:
    val meta: Meta = Meta(None, Position.unknown, None, Nil)
    val tags: DFTags = DFTags.empty
    protected def protIsConst(using MemberGetSet): Boolean = false
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case _: Open => true
      case _       => false
    protected def setMeta(meta: Meta): this.type = this
    protected def setTags(tags: DFTags): this.type = this
    def getRefs: List[DFRef.TwoWayAny] = dfType.getRefs
  end Open

  final case class Dcl(
      dfType: DFType,
      modifier: Modifier,
      initRefList: List[Dcl.InitRef],
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFVal:
    protected def protIsConst(using MemberGetSet): Boolean = false
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Dcl =>
        val sameInit =
          if (this.initRefList.length == that.initRefList.length)
            this.initRefList.lazyZip(that.initRefList).forall(_ =~ _)
          else false
        this.dfType =~ that.dfType && this.modifier == that.modifier && sameInit &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    def initList(using MemberGetSet): List[DFVal] = initRefList.map(_.get)
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = dfType.getRefs ++ initRefList
  end Dcl
  object Dcl:
    type InitRef = DFRef.TwoWay[DFVal, Dcl]

  final case class Func(
      dfType: DFType,
      op: Func.Op,
      args: List[DFVal.Ref],
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends CanBeExpr,
        CanBeGlobal:
    protected def protIsConst(using MemberGetSet): Boolean =
      args.forall(_.get.isConst)
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Func =>
        this.dfType =~ that.dfType && this.op == that.op && (this.args
          .lazyZip(that.args)
          .forall((l, r) => l =~ r)) &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = dfType.getRefs ++ args
  end Func

  object Func:
    enum Op derives CanEqual:
      case +, -, *, /, ===, =!=, <, >, <=, >=, &, |, ^, %, ++
      case >>, <<, ror, rol, reverse, repeat
      case unary_-, unary_~, unary_!
      case rising, falling
      case clog2, max, min

  final case class PortByNameSelect(
      dfType: DFType,
      designInstRef: PortByNameSelect.Ref,
      portNamePath: String,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFVal:
    protected def protIsConst(using MemberGetSet): Boolean = false
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: PortByNameSelect =>
        this.dfType =~ that.dfType && this.designInstRef =~ that.designInstRef &&
        this.portNamePath == that.portNamePath &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = designInstRef :: dfType.getRefs
  end PortByNameSelect
  object PortByNameSelect:
    type Ref = DFRef.TwoWay[DFDesignInst, PortByNameSelect]
    object Of:
      def unapply(portByNameSelect: PortByNameSelect)(using MemberGetSet): Option[DFVal.Dcl] =
        Some(portByNameSelect.getPortDcl)
    extension (portByNameSelect: PortByNameSelect)
      def getPortDcl(using MemberGetSet): DFVal.Dcl =
        val designInst = portByNameSelect.designInstRef.get
        getSet.designDB.portsByName(designInst)(portByNameSelect.portNamePath)

  sealed trait Alias extends CanBeExpr:
    val relValRef: Alias.Ref
    def getRefs: List[DFRef.TwoWayAny] = relValRef :: dfType.getRefs

  object Alias:
    case object IdentTag extends DFTagOf[DFVal]
    case object DesignParamTag extends DFTagOf[DFVal]
    type Ref = DFRef.TwoWay[DFVal, Alias]
    // This is complete alias that consumes its relative val
    sealed trait Consumer extends Alias:
      val relValRef: ConsumerRef
    type ConsumerRef = DFRef.TwoWay[DFVal, Consumer]

    // This is a partial alias that can propagate its modifier.
    // E.g., a mutable variable `x` that we select its bit `x(1)` is also mutable.
    sealed trait Partial extends Alias, CanBeGlobal:
      val relValRef: PartialRef
    type PartialRef = DFRef.TwoWay[DFVal, Partial]

    final case class AsIs(
        dfType: DFType,
        relValRef: PartialRef,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Partial:
      protected def protIsConst(using MemberGetSet): Boolean = relValRef.get.isConst
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: AsIs =>
          // design parameters are considered to be the same even if they are referencing
          // a different member (this should be quite common), because that member is
          // external to the design.
          val sameRelVal =
            if (this.hasTagOf[DesignParamTag.type]) true
            else this.relValRef =~ that.relValRef
          this.dfType =~ that.dfType && sameRelVal &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    end AsIs

    final case class History(
        dfType: DFType,
        relValRef: ConsumerRef,
        step: Int,
        op: History.Op,
        initRefOption: Option[History.InitRef],
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Consumer:
      protected def protIsConst(using MemberGetSet): Boolean = false
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: History =>
          val sameInit = (this.initRefOption, that.initRefOption) match
            case (Some(l), Some(r)) => l =~ r
            case (None, None)       => true
            case _                  => false
          this.dfType =~ that.dfType && this.relValRef =~ that.relValRef &&
          this.step == that.step && this.op == that.op && sameInit &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      def initOption(using MemberGetSet): Option[DFVal] = initRefOption.map(_.get)
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
      override def getRefs: List[DFRef.TwoWayAny] = relValRef :: dfType.getRefs ++ initRefOption
    end History

    object History:
      type InitRef = DFRef.TwoWay[DFVal, History]
      enum Op derives CanEqual:
        case Prev, Pipe, Reg

    final case class ApplyRange(
        relValRef: PartialRef,
        relBitHigh: Int,
        relBitLow: Int,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Partial:
      protected def protIsConst(using MemberGetSet): Boolean = relValRef.get.isConst
      val dfType: DFType = DFBits(relBitHigh - relBitLow + 1)
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: ApplyRange =>
          this.relValRef =~ that.relValRef &&
          this.relBitHigh == that.relBitHigh && this.relBitLow == that.relBitLow &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    end ApplyRange
    final case class ApplyIdx(
        dfType: DFType,
        relValRef: PartialRef,
        relIdx: DFVal.Ref,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Partial:
      protected def protIsConst(using MemberGetSet): Boolean =
        relValRef.get.isConst && relIdx.get.isConst
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: ApplyIdx =>
          this.dfType =~ that.dfType && this.relValRef =~ that.relValRef &&
          this.relIdx =~ that.relIdx &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
      override def getRefs: List[DFRef.TwoWayAny] = relIdx :: relValRef :: dfType.getRefs
    end ApplyIdx
    object ApplyIdx:
      object Const:
        def unapply(applyIdx: ApplyIdx)(using MemberGetSet): Option[Int] =
          applyIdx.relIdx.get match
            case DFVal.Const(DFUInt(_), data: Option[BigInt] @unchecked, _, _, _) =>
              data.map(_.toInt)
            case _ => None

    final case class SelectField(
        dfType: DFType,
        relValRef: PartialRef,
        fieldName: String,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Partial:
      protected def protIsConst(using MemberGetSet): Boolean = relValRef.get.isConst
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: SelectField =>
          this.dfType =~ that.dfType && this.relValRef =~ that.relValRef &&
          this.fieldName == that.fieldName &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    end SelectField
  end Alias
end DFVal

final case class DFNet(
    lhsRef: DFNet.Ref,
    op: DFNet.Op,
    rhsRef: DFNet.Ref,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFMember:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFNet =>
      this.lhsRef =~ that.lhsRef && this.op == that.op && this.rhsRef =~ that.rhsRef &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  def getRefs: List[DFRef.TwoWayAny] = List(lhsRef, rhsRef)
end DFNet

object DFNet:
  type Ref = DFRef.TwoWay[DFVal | DFInterfaceOwner, DFNet]
  enum Op derives CanEqual:
    case Assignment, NBAssignment, Connection, ViaConnection, LazyConnection
  extension (net: DFNet)
    def isAssignment = net.op match
      case Op.Assignment | Op.NBAssignment => true
      case _                               => false
    def isConnection = net.op match
      case Op.Connection | Op.ViaConnection | Op.LazyConnection => true
      case _                                                    => false
    def isViaConnection = net.op match
      case Op.ViaConnection => true
      case _                => false
    def isLazyConnection = net.op match
      case Op.LazyConnection => true
      case _                 => false

  object Assignment:
    def unapply(arg: DFNet)(using MemberGetSet): Option[(DFVal, DFVal)] = arg match
      case DFNet(
            DFRef(toVal: DFVal),
            Op.Assignment | Op.NBAssignment,
            DFRef(fromVal: DFVal),
            _,
            _,
            _
          ) =>
        Some(toVal, fromVal)
      case _ => None
  object BAssignment:
    def unapply(arg: DFNet)(using MemberGetSet): Option[(DFVal, DFVal)] = arg match
      case Assignment(lhs, rhs) if arg.op == Op.Assignment => Some(lhs, rhs)
      case _                                               => None
  object NBAssignment:
    def unapply(arg: DFNet)(using MemberGetSet): Option[(DFVal, DFVal)] = arg match
      case Assignment(lhs, rhs) if arg.op == Op.NBAssignment => Some(lhs, rhs)
      case _                                                 => None
  object Connection:
    def unapply(net: DFNet)(using
        MemberGetSet
        //             toVal                      fromVal              Swapped
    ): Option[(DFVal.Dcl | DFVal.Open | DFInterfaceOwner, DFVal | DFInterfaceOwner, Boolean)] =
      if (net.isConnection) (net.lhsRef.get, net.rhsRef.get) match
        case (lhsVal: DFVal, rhsVal: DFVal) =>
          val toLeft = getSet.designDB.connectionTable.getNets(lhsVal).contains(net)
          if (toLeft) Some(lhsVal.dealias.get, rhsVal.stripPortSel, false)
          else Some(rhsVal.dealias.get, lhsVal.stripPortSel, true)
        case (lhsIfc: DFInterfaceOwner, rhsIfc: DFInterfaceOwner) =>
          Some(lhsIfc, rhsIfc, false)
        case _ => ??? // not possible
      else None
  end Connection
end DFNet

sealed trait DFOwner extends DFMember:
  val meta: Meta
  def isTop(using MemberGetSet): Boolean = ownerRef.get match
    case DFMember.Empty => true
    case _              => false

sealed trait DFOwnerNamed extends DFOwner, DFMember.Named
sealed trait DFDomainOwner extends DFOwnerNamed:
  val domainType: DomainType

object DFOwner:
  type Ref = DFRef.OneWay[DFOwner | DFMember.Empty]

final case class DFInterfaceOwner(
    domainType: DomainType,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFDomainOwner:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFInterfaceOwner =>
      this.domainType == that.domainType &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  def getRefs: List[DFRef.TwoWayAny] = Nil
end DFInterfaceOwner

sealed trait DFBlock extends DFOwner

final case class ProcessBlock(
    sensitivity: ProcessBlock.Sensitivity,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFBlock,
      DFOwnerNamed:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: ProcessBlock =>
      this.sensitivity =~ that.sensitivity &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  def getRefs: List[DFRef.TwoWayAny] = sensitivity.getRefs
end ProcessBlock
object ProcessBlock:
  sealed trait Sensitivity extends HasRefCompare[Sensitivity], Product, Serializable
      derives CanEqual
  object Sensitivity:
    case object All extends Sensitivity:
      protected def `prot_=~`(that: Sensitivity)(using MemberGetSet): Boolean = that match
        case All => true
        case _   => false
      def getRefs: scala.List[DFRef.TwoWayAny] = Nil
    final case class List(refs: scala.List[DFVal.Ref]) extends Sensitivity:
      protected def `prot_=~`(that: Sensitivity)(using MemberGetSet): Boolean = that match
        case that: List => this.refs.lazyZip(that.refs).forall(_ =~ _)
        case _          => false
      def getRefs: scala.List[DFRef.TwoWayAny] = refs

object DFConditional:
  sealed trait Block extends DFBlock:
    type THeader <: Header
    val guardRef: Block.GuardRef
    val prevBlockOrHeaderRef: Block.Ref
  object Block:
    type Ref = DFRef.TwoWay[Block | Header, DFMember]
    type GuardRef = DFRef.TwoWay[DFVal | DFMember.Empty, DFMember]

  sealed trait Header extends DFVal.CanBeExpr:
    type TBlock <: Block

  final case class DFMatchHeader(
      dfType: DFType,
      selectorRef: DFVal.Ref,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Header:
    type TBlock = DFCaseBlock
    // TODO: if all returned expressions in all blocks and the selector is constant, then
    // the returned result is a constant
    protected def protIsConst(using MemberGetSet): Boolean = false
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFMatchHeader =>
        this.dfType =~ that.dfType && this.selectorRef =~ that.selectorRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = selectorRef :: dfType.getRefs
  end DFMatchHeader

  final case class DFCaseBlock(
      pattern: DFCaseBlock.Pattern,
      guardRef: Block.GuardRef,
      prevBlockOrHeaderRef: DFCaseBlock.Ref,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Block:
    type THeader = DFMatchHeader
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFCaseBlock =>
        this.pattern =~ that.pattern && this.guardRef =~ that.guardRef &&
        this.prevBlockOrHeaderRef =~ that.prevBlockOrHeaderRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = List(guardRef, prevBlockOrHeaderRef) ++ pattern.getRefs
  end DFCaseBlock
  object DFCaseBlock:
    type Ref = DFRef.TwoWay[DFCaseBlock | DFMatchHeader, Block]
    sealed trait Pattern extends HasRefCompare[Pattern] derives CanEqual
    object Pattern:
      case object CatchAll extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean = this == that
        def getRefs: List[DFRef.TwoWayAny] = Nil
      final case class Singleton(valueRef: DFVal.Ref) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: Singleton =>
              this.valueRef =~ that.valueRef
            case _ => false
        def getRefs: List[DFRef.TwoWayAny] = List(valueRef)
      final case class Alternative(list: List[Pattern]) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: Alternative =>
              this.list.lazyZip(that.list).forall(_ =~ _)
            case _ => false
        def getRefs: List[DFRef.TwoWayAny] = list.flatMap(_.getRefs)
      final case class Struct(name: String, fieldPatterns: List[Pattern]) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: Struct =>
              this.name == that.name && this.fieldPatterns
                .lazyZip(that.fieldPatterns)
                .forall(_ =~ _)
            case _ => false
        def getRefs: List[DFRef.TwoWayAny] = fieldPatterns.flatMap(_.getRefs)
      final case class Bind(ref: Bind.Ref, pattern: Pattern) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: Bind =>
              this.ref =~ that.ref && this.pattern =~ that.pattern
            case _ => false
        def getRefs: List[DFRef.TwoWayAny] = ref :: pattern.getRefs
      object Bind:
        type Ref = DFRef.TwoWay[DFVal, DFCaseBlock]
        case object Tag extends DFTagOf[DFVal]
      final case class BindSI(
          op: String,
          parts: List[String],
          refs: List[Bind.Ref]
      ) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: BindSI =>
              this.op == that.op && this.parts == that.parts && this.refs
                .lazyZip(that.refs)
                .forall(_ =~ _)
            case _ => false
        def getRefs: List[DFRef.TwoWayAny] = refs
    end Pattern
  end DFCaseBlock

  final case class DFIfHeader(
      dfType: DFType,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Header:
    type TBlock = DFIfElseBlock
    // TODO: if all returned expressions in all blocks and the selector is constant, then
    // the returned result is a constant
    protected def protIsConst(using MemberGetSet): Boolean = false
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFIfHeader =>
        this.dfType =~ that.dfType &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = dfType.getRefs
  end DFIfHeader

  final case class DFIfElseBlock(
      guardRef: Block.GuardRef,
      prevBlockOrHeaderRef: DFIfElseBlock.Ref,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Block:
    type THeader = DFIfHeader
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFIfElseBlock =>
        this.guardRef =~ that.guardRef && this.prevBlockOrHeaderRef =~ that.prevBlockOrHeaderRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = List(guardRef, prevBlockOrHeaderRef)
  end DFIfElseBlock
  object DFIfElseBlock:
    type Ref = DFRef.TwoWay[DFIfElseBlock | DFIfHeader, Block]
end DFConditional

final case class DFDesignBlock(
    domainType: DomainType,
    dclMeta: Meta,
    instMode: DFDesignBlock.InstMode,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFBlock,
      DFDomainOwner:
  val dclName: String = dclMeta.name
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFDesignBlock =>
      this.domainType == that.domainType &&
      this.dclMeta == that.dclMeta &&
      this.instMode == that.instMode &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  def getRefs: List[DFRef.TwoWayAny] = Nil
end DFDesignBlock

object DFDesignBlock:
  import InstMode.BlackBox.Source
  enum InstMode derives CanEqual:
    case Normal, Def, Simulation
    case BlackBox(args: ListMap[String, Any], verilogSrc: Source, vhdlSrc: Source)
  object InstMode:
    object BlackBox:
      enum Source derives CanEqual:
        case NA
        case File(path: String)
        case Library(libName: String, nameSpace: String)

  extension (dsn: DFDesignBlock)
    def isDuplicate: Boolean = dsn.hasTagOf[DuplicateTag]
    def inSimulation: Boolean = dsn.instMode == InstMode.Simulation

  object Top:
    def unapply(block: DFDesignBlock)(using MemberGetSet): Boolean = block.isTop
  object Internal:
    def unapply(block: DFDesignBlock)(using MemberGetSet): Boolean = !block.isTop
end DFDesignBlock

type DFDesignInst = DFDesignBlock
val DFDesignInst = DFDesignBlock

final case class DomainBlock(
    domainType: DomainType,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFBlock,
      DFDomainOwner:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DomainBlock =>
      this.domainType == that.domainType &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  def getRefs: List[DFRef.TwoWayAny] = Nil
end DomainBlock

sealed trait DFSimMember extends DFMember
object DFSimMember:
  final case class Assert(
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFSimMember:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Assert =>
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = Nil

sealed trait Timer extends DFMember.Named
object Timer:
  type Ref = DFRef.TwoWay[Timer, DFMember]
  type TriggerRef = DFRef.TwoWay[DFVal | DFMember.Empty, DFMember]
  final case class Periodic(
      triggerRef: TriggerRef,
      periodOpt: Option[Time],
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Timer:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Periodic =>
        this.triggerRef =~ that.triggerRef && this.periodOpt == that.periodOpt &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = List(triggerRef)
  end Periodic

  final case class Func(
      sourceRef: Timer.Ref,
      op: Func.Op,
      arg: Time | Ratio,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Timer:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Func =>
        this.sourceRef =~ that.sourceRef && this.op == that.op && this.arg == that.arg &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = List(sourceRef)
  end Func
  object Func:
    enum Op derives CanEqual:
      case Delay, `*`, /

  final case class IsActive(
      timerRef: Ref,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFVal.CanBeExpr:
    val dfType: DFType = DFBool
    protected def protIsConst(using MemberGetSet): Boolean = false
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: IsActive =>
        this.timerRef =~ that.timerRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = List(timerRef)
  end IsActive
end Timer

sealed trait Wait extends DFMember
object Wait:
  final case class Duration(
      timeOpt: Option[Time],
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Wait:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Duration =>
        this.timeOpt == that.timeOpt &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = Nil

  type TriggerRef = DFRef.TwoWay[DFVal, DFMember]
  final case class Until(
      triggerRef: TriggerRef,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Wait:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Until =>
        this.triggerRef =~ that.triggerRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    def getRefs: List[DFRef.TwoWayAny] = List(triggerRef)
end Wait
