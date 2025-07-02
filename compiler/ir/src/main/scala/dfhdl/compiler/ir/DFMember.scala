package dfhdl.compiler
package ir
import dfhdl.internals.*
import upickle.default.*
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
    case o: DFOwner        => o
    case _: DFMember.Empty =>
      throw new IllegalArgumentException(s"No owner found for member $this.")
  final def getOwnerNamed(using MemberGetSet): DFOwnerNamed = getOwner match
    case b: DFOwnerNamed => b
    case o               => o.getOwnerNamed
  final def getOwnerBlock(using MemberGetSet): DFBlock = getOwner match
    case b: DFBlock => b
    case o          => o.getOwnerBlock
  final def getOwnerStepBlock(using MemberGetSet): StepBlock = getOwner match
    case b: StepBlock => b
    case o            => o.getOwnerStepBlock
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
  final def getThisOrOwnerDomain(using MemberGetSet): DFDomainOwner = this match
    case d: DFDomainOwner => d
    case x                => x.getOwnerDomain
  final def getThisOrOwnerNamed(using MemberGetSet): DFOwnerNamed = this match
    case d: DFOwnerNamed => d
    case x               => x.getOwnerNamed
  final def getThisOrOwnerStepBlock(using MemberGetSet): StepBlock = this match
    case d: StepBlock => d
    case x            => x.getOwnerStepBlock
  final infix def isMemberOf(that: DFOwnerNamed)(using MemberGetSet): Boolean =
    this match
      case DFDesignBlock.Top() => false
      case _                   => getOwnerNamed == that
  final def isOwnedCond(cond: DFOwner => Option[Boolean])(using MemberGetSet): Boolean =
    var owner = this.getOwner
    var ret = cond(owner)
    while (ret.isEmpty && !owner.isTop)
      owner = owner.getOwner
      ret = cond(owner)
    ret.getOrElse(false)
  end isOwnedCond
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
      case _                   =>
        (thisMember.getOwner, thatOwner) match
          case (a, b) if a == b => true
          case (od, _)          => isInsideOwner(od, thatOwner)
  // true if and only if the member is inside the design at any level
  final infix def isInsideOwner(that: DFOwner)(using MemberGetSet): Boolean =
    isInsideOwner(this, that)
  final def getOwnerChain(using MemberGetSet): List[DFBlock] =
    this match
      case d @ DFDesignBlock.Top() => Nil
      case _                       =>
        if (getOwnerBlock.isTop) List(getOwnerBlock)
        else getOwnerBlock.getOwnerChain :+ getOwnerBlock
  // count the hierarchy distance from inside to outside
  final def getDistanceFromOwnerDesign(outside: DFDesignBlock)(using MemberGetSet): Int =
    val inside = this.getThisOrOwnerDesign
    var distance = 0
    var dsn = inside
    while (dsn != outside)
      distance = distance + 1
      dsn = dsn.getOwnerDesign
    return distance
end DFMember

object DFMember:
  given ReadWriter[DFMember] = ReadWriter.merge(
    summon[ReadWriter[DFMember.Empty.type]],
    summon[ReadWriter[DFVal]],
    summon[ReadWriter[Statement]],
    summon[ReadWriter[DFInterfaceOwner]],
    summon[ReadWriter[DFBlock]],
    summon[ReadWriter[DFConditional.Header]],
    summon[ReadWriter[DFRange]]
  )
  extension (member: DFMember)
    def getTagOf[CT <: DFTag: ClassTag]: Option[CT] =
      member.tags.getTagOf[CT]
    def hasTagOf[CT <: DFTag: ClassTag]: Boolean =
      member.tags.hasTagOf[CT]
    def getDomainType(using MemberGetSet): DomainType = member.getOwnerDomain.domainType
    def isInDFDomain(using MemberGetSet): Boolean = member.getDomainType match
      case DomainType.DF => true
      case _             => false
    def isInRTDomain(using MemberGetSet): Boolean = member.getDomainType match
      case DomainType.RT(_) => true
      case _                => false
    def isInEDDomain(using MemberGetSet): Boolean = member.getDomainType match
      case DomainType.ED => true
      case _             => false
    def isInProcess(using MemberGetSet): Boolean = member.isOwnedCond(cond = {
      case _: ProcessBlock  => Some(true)
      case _: DFDomainOwner => Some(false)
      case _                => None
    })
    def toJson(using Writer[DFMember]): String = write(member)
  end extension

  sealed trait Empty extends DFMember:
    val ownerRef: DFOwner.Ref = DFRef.OneWay.Empty
    val meta: Meta = Meta(None, Position.unknown, None, Nil)
    val tags: DFTags = DFTags.empty
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case _: Empty => true
      case _        => false
    protected def setMeta(meta: Meta): this.type = this
    protected def setTags(tags: DFTags): this.type = this
    lazy val getRefs: List[DFRef.TwoWayAny] = Nil
    def copyWithNewRefs(using RefGen): this.type = this
  case object Empty extends Empty:
    given ReadWriter[Empty.type] = macroRW

  sealed trait Named extends DFMember:
    final def getName(using MemberGetSet): String = this match
      case o: DFDesignBlock if o.isTop => o.dclName
      case _                           => meta.name
    final lazy val isAnonymous: Boolean = meta.isAnonymous
    final def getFullName(using MemberGetSet): String = this match
      case o: DFDesignBlock if o.isTop => getName
      case _                           => s"${getOwnerNamed.getFullName}.${getName}"
    def getRelativeName(callOwner: DFOwner | DFMember.Empty)(using MemberGetSet): String =
      if (callOwner == DFMember.Empty) getName
      else
        val namedOwner = callOwner.getThisOrOwnerNamed
        if (this.isMemberOf(namedOwner)) getName
        else if (getOwnerNamed isOneLevelBelow namedOwner) s"${getOwnerNamed.getName}.$getName"
        else if (callOwner isInsideOwner this.getOwnerNamed) getName
        else
          // more complex referencing just summons the two owner chains and compares them.
          // it is possible to do this more efficiently but the simple cases cover the most common usage anyway
          val memberChain = this.getOwnerChain.collect { case o: DFOwnerNamed => o }
          val ctxChain =
            namedOwner.getOwnerChain.collect { case o: DFOwnerNamed => o } :+ namedOwner
          val samePath = memberChain.lazyZip(ctxChain).count(_ == _)
          s"${memberChain.drop(samePath).map(_.getName).mkString(".")}.$getName"
    end getRelativeName
  end Named
  extension (member: Named)
    def stripPortSel(using MemberGetSet): Named = member match
      case portSel: DFVal.PortByNameSelect => portSel.getPortDcl
      case _                               => member
end DFMember

sealed trait Statement extends DFMember derives ReadWriter

sealed trait DFVal extends DFMember.Named:
  val dfType: DFType
  def width(using MemberGetSet): Int = dfType.width
  def isGlobal(using MemberGetSet): Boolean = false
  protected def protIsFullyAnonymous(using MemberGetSet): Boolean
  // using just an integer to escape redundant boxing Option[Boolean] would have achieved
  private var cachedIsFullyAnonymous: Int = -1
  final def isFullyAnonymous(using MemberGetSet): Boolean =
    if (cachedIsFullyAnonymous == -1)
      val localIsFullyAnonymous = protIsFullyAnonymous
      cachedIsFullyAnonymous = if (localIsFullyAnonymous) 1 else 0
      localIsFullyAnonymous
    else if (cachedIsFullyAnonymous > 0) true
    else false
  final def isConst(using MemberGetSet): Boolean = getConstData.nonEmpty
  // two expressions are considered to be similar if
  final def isSimilarTo(that: DFVal)(using MemberGetSet): Boolean =
    def stripAsIsAndDesignParam(dfVal: DFVal): DFVal = dfVal match
      case DFVal.Alias.AsIs(dfType = dfType, relValRef = DFRef(relVal))
          if dfType == relVal.dfType =>
        stripAsIsAndDesignParam(relVal)
      case DFVal.DesignParam(dfValRef = DFRef(dfVal)) =>
        stripAsIsAndDesignParam(dfVal)
      case _ => dfVal
    (stripAsIsAndDesignParam(this), stripAsIsAndDesignParam(that)) match
      case (lhs: DFVal.CanBeExpr, rhs: DFVal.CanBeExpr) => lhs.protIsSimilarTo(rhs)
      case (lhs, rhs)                                   => lhs == rhs
  protected def protGetConstData(using MemberGetSet): Option[Any]
  private var cachedConstDataReady: Boolean = false
  private var cachedConstData: Option[Any] = None
  final def wasConstDataAccessed: Boolean = cachedConstDataReady
  final def getConstData(using MemberGetSet): Option[Any] =
    if (cachedConstDataReady) cachedConstData
    else
      cachedConstData = protGetConstData
      cachedConstDataReady = true
      cachedConstData
  def updateDFType(dfType: DFType): this.type
end DFVal

object DFVal:
  type Ref = DFRef.TwoWay[DFVal, DFMember]
  given ReadWriter[DFVal] = ReadWriter.merge(
    summon[ReadWriter[DFVal.Dcl]],
    summon[ReadWriter[DFVal.OPEN]],
    summon[ReadWriter[DFVal.Alias]],
    summon[ReadWriter[DFVal.Const]],
    summon[ReadWriter[DFVal.DesignParam]],
    summon[ReadWriter[DFVal.Func]],
    summon[ReadWriter[DFVal.PortByNameSelect]]
  )
  final case class Modifier(dir: Modifier.Dir, special: Modifier.Special)
      derives CanEqual,
        ReadWriter:
    override def toString(): String =
      special match
        case Modifier.Special.Ordinary => dir.toString()
        case _                         => s"$dir.$special"
  object Modifier:
    extension (mod: Modifier)
      def isReg: Boolean = mod.special == REG
      def isShared: Boolean = mod.special == SHARED
      def isPort: Boolean = mod.dir match
        case Modifier.IN | Modifier.OUT | Modifier.INOUT => true
        case _                                           => false
    enum Dir extends StableEnum derives CanEqual, ReadWriter:
      case VAR, IN, OUT, INOUT
    export Dir.{VAR, IN, OUT, INOUT}
    enum Special extends StableEnum derives CanEqual, ReadWriter:
      case Ordinary, REG, SHARED
    export Special.{Ordinary, REG, SHARED}

  extension (dfVal: DFVal)
    def isPort: Boolean = dfVal match
      case dcl: DFVal.Dcl => dcl.modifier.isPort
      case _              => false
    def isPortOut: Boolean = dfVal match
      case dcl: DFVal.Dcl =>
        dcl.modifier.dir match
          case Modifier.OUT => true
          case _            => false
      case _ => false
    def isPortIn: Boolean = dfVal match
      case dcl: DFVal.Dcl =>
        dcl.modifier.dir match
          case Modifier.IN => true
          case _           => false
      case _ => false
    def isVar: Boolean = dfVal match
      case dcl: DFVal.Dcl =>
        dcl.modifier.dir match
          case Modifier.VAR => true
          case _            => false
      case _ => false
    def isOpen: Boolean = dfVal match
      case _: DFVal.OPEN => true
      case _             => false
    def isDesignParam: Boolean = dfVal match
      case _: DFVal.DesignParam => true
      case _                    => false
    def isReg: Boolean = dfVal match
      case dcl: DFVal.Dcl => dcl.modifier.isReg
      case _              => false
    @tailrec def dealias(using MemberGetSet): Option[DFVal.Dcl | DFVal.OPEN] = dfVal match
      case dcl: DFVal.Dcl                           => Some(dcl)
      case portByNameSelect: DFVal.PortByNameSelect => Some(portByNameSelect.getPortDcl)
      case open: DFVal.OPEN                         => Some(open)
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
              relVal.departial(range.offset(partial.idxLowRef.getInt))
            case partial: DFVal.Alias.ApplyIdx =>
              partial.relIdx.get match
                case DFVal.Alias.ApplyIdx.ConstIdx(idx) =>
                  relVal.departial(range.offset(idx * partial.width))
                // if not a constant index selection, then the entire value range is affected
                case _ =>
                  relVal.dealias match
                    case Some(dcl: DFVal.Dcl) => (dcl, range)
                    case _                    => (relVal, range)
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
    def isBubble(using MemberGetSet): Boolean =
      dfVal match
        case c: DFVal.Const          => c.dfType.isDataBubble(c.data.asInstanceOf[c.dfType.Data])
        case f: DFVal.Func           => f.args.exists(_.get.isBubble)
        case a: DFVal.Alias.ApplyIdx => a.relValRef.get.isBubble || a.relIdx.get.isBubble
        case a: DFVal.Alias.Partial  => a.relValRef.get.isBubble
        case _                       => false
  end extension
  // can be an expression
  sealed trait CanBeExpr extends DFVal:
    protected[ir] def protIsSimilarTo(that: CanBeExpr)(using MemberGetSet): Boolean

  // can be a global value
  sealed trait CanBeGlobal extends CanBeExpr:
    private[dfhdl] var globalCtx: Any = compiletime.uninitialized
    final override def isGlobal(using MemberGetSet): Boolean =
      // during elaboration with a DFC context we can use `Empty` as an indicator
      // that `dfVal` is global. we use it because `dfVal`'s ownerRef could be only available
      // within dfVal's internal cached context. however, when in immutable db, `dfVal.isGlobal` can
      // be invoked with no issue, since the DB `getSet` will have access to all references.
      // note that it could be that the ref can late change from `Empty` to an actual member,
      // so that is why we cannot rely on it after elaboration is done.
      ownerRef.getOption match
        case Some(DFMember.Empty) => true
        case None                 => true
        case _                    => false
    def copyWithoutGlobalCtx: this.type

    final override def getRelativeName(
        callOwner: DFOwner | DFMember.Empty
    )(using MemberGetSet): String =
      if (isGlobal) this.getName
      else super.getRelativeName(callOwner)
    final override infix def isSameOwnerDesignAs(that: DFMember)(using MemberGetSet): Boolean =
      if (this.isGlobal) false
      else
        that match
          case thatDFVal: DFVal if thatDFVal.isGlobal => false
          case _                                      => super.isSameOwnerDesignAs(that)
  end CanBeGlobal

  final case class Const(
      dfType: DFType,
      data: Data,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends CanBeExpr,
        CanBeGlobal derives ReadWriter:
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean = this.isAnonymous
    protected def protGetConstData(using MemberGetSet): Option[Any] = Some(data)
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Const =>
        this.dfType =~ that.dfType && this.data.equals(that.data) &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected[ir] def protIsSimilarTo(that: CanBeExpr)(using MemberGetSet): Boolean =
      that match
        case that: Const =>
          // if constants are named, then they must be the exact same constant instance to be considered similar expressions
          if (!this.isAnonymous && !that.isAnonymous) this == that
          // otherwise, they must both be anonymous and have the similar type expression and same data to be considered similar expressions
          else if (this.isAnonymous && that.isAnonymous)
            this.dfType.isSimilarTo(that.dfType) && this.data.equals(that.data)
          else false
        case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = dfType.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      dfType = dfType.copyWithNewRefs,
      ownerRef = ownerRef.copyAsNewRef
    ).asInstanceOf[this.type]
  end Const

  final case class DesignParam(
      dfType: DFType,
      dfValRef: DesignParam.Ref,
      defaultRef: DesignParam.DefaultRef,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends CanBeExpr derives ReadWriter:
    assert(!this.isAnonymous, "Design parameters cannot be anonymous.")
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean = false
    protected def protGetConstData(using MemberGetSet): Option[Any] =
      dfValRef.get.getConstData
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DesignParam =>
        // design parameters are considered to be the same even if they are referencing
        // a different member (this should be quite common), because that member is
        // external to the design. however, different default value is considered to be a
        // different design parameter.
        this.dfType =~ that.dfType && this.defaultRef =~ that.defaultRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected[ir] def protIsSimilarTo(that: CanBeExpr)(using MemberGetSet): Boolean =
      that match
        case that: DesignParam =>
          this.dfType.isSimilarTo(that.dfType) &&
          this.dfValRef.get.isSimilarTo(that.dfValRef.get)
        case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = dfValRef :: defaultRef :: dfType.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      dfType = dfType.copyWithNewRefs,
      ownerRef = ownerRef.copyAsNewRef,
      dfValRef = dfValRef.copyAsNewRef,
      defaultRef = defaultRef.copyAsNewRef
    ).asInstanceOf[this.type]
  end DesignParam
  object DesignParam:
    type Ref = DFRef.TwoWay[DFVal, DesignParam]
    type DefaultRef = DFRef.TwoWay[DFVal | DFMember.Empty, DesignParam]

  final case class OPEN(
      dfType: DFType,
      ownerRef: DFOwner.Ref
  ) extends DFVal derives ReadWriter:
    val meta: Meta = Meta(None, Position.unknown, None, Nil)
    val tags: DFTags = DFTags.empty
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean = true
    protected def protGetConstData(using MemberGetSet): Option[Any] = None
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case _: OPEN => true
      case _       => false
    protected def setMeta(meta: Meta): this.type = this
    protected def setTags(tags: DFTags): this.type = this
    lazy val getRefs: List[DFRef.TwoWayAny] = dfType.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      dfType = dfType.copyWithNewRefs,
      ownerRef = ownerRef.copyAsNewRef
    ).asInstanceOf[this.type]
  end OPEN

  final case class NOTHING(
      dfType: DFType,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends CanBeExpr derives ReadWriter:
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean = true
    protected def protGetConstData(using MemberGetSet): Option[Any] = None
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case _: NOTHING => true
      case _          => false
    protected[ir] def protIsSimilarTo(that: CanBeExpr)(using MemberGetSet): Boolean =
      this == that
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = dfType.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      dfType = dfType.copyWithNewRefs,
      ownerRef = ownerRef.copyAsNewRef
    ).asInstanceOf[this.type]
  end NOTHING

  final case class Dcl(
      dfType: DFType,
      modifier: Modifier,
      initRefList: List[Dcl.InitRef],
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFVal derives ReadWriter:
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean = false
    protected def protGetConstData(using MemberGetSet): Option[Any] = None
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
    lazy val getRefs: List[DFRef.TwoWayAny] = dfType.getRefs ++ initRefList
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      dfType = dfType.copyWithNewRefs,
      ownerRef = ownerRef.copyAsNewRef,
      initRefList = initRefList.map(_.copyAsNewRef)
    ).asInstanceOf[this.type]
  end Dcl
  object Dcl:
    type InitRef = DFRef.TwoWay[DFVal, Dcl]
    extension (dcl: Dcl)
      def isClkDcl(using MemberGetSet): Boolean = dcl.dfType match
        case DFOpaque(kind = DFOpaque.Kind.Clk) => true
        case _                                  => false
      def isRstDcl(using MemberGetSet): Boolean = dcl.dfType match
        case DFOpaque(kind = DFOpaque.Kind.Rst) => true
        case _                                  => false
      def hasNonBubbleInit(using MemberGetSet): Boolean = dcl.initRefList match
        case DFRef(dfVal) :: _ => !dfVal.isBubble
        case _                 => false
  final case class Func(
      dfType: DFType,
      op: Func.Op,
      args: List[DFVal.Ref],
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends CanBeExpr,
        CanBeGlobal derives ReadWriter:
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean =
      args.forall(_.get.isFullyAnonymous)
    protected def protGetConstData(using MemberGetSet): Option[Any] =
      val args = this.args.map(_.get)
      val argData = args.flatMap(_.getConstData)
      val argTypes = args.map(_.dfType)
      if (argData.length != this.args.length) None
      else Some(calcFuncData(dfType, op, argTypes, argData))
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Func =>
        this.dfType =~ that.dfType && this.op == that.op && (this.args
          .lazyZip(that.args)
          .forall((l, r) => l =~ r)) &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    // TODO: consider algebraic equivalence be added here
    protected[ir] def protIsSimilarTo(that: CanBeExpr)(using MemberGetSet): Boolean =
      that match
        case that: Func =>
          this.dfType.isSimilarTo(that.dfType) && this.op == that.op &&
          (this.args.lazyZip(that.args).forall((l, r) => l.get.isSimilarTo(r.get)))
        case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = dfType.getRefs ++ args
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      dfType = dfType.copyWithNewRefs,
      ownerRef = ownerRef.copyAsNewRef,
      args = args.map(_.copyAsNewRef)
    ).asInstanceOf[this.type]
  end Func

  object Func:
    enum Op extends StableEnum derives CanEqual, ReadWriter:
      case +, -, *, /, ===, =!=, <, >, <=, >=, &, |, ^, %, ++
      case >>, <<, **, ror, rol, reverse, repeat
      case unary_-, unary_~, unary_!
      case rising, falling
      case clog2, max, min, sel
      // special-case of initFile construct for vectors of bits
      case InitFile(format: InitFileFormat, path: String)
    object Op:
      val associativeSet = Set(Op.+, Op.-, Op.`*`, Op.&, Op.|, Op.^, Op.++, Op.max, Op.min)

  final case class PortByNameSelect(
      dfType: DFType,
      designInstRef: PortByNameSelect.Ref,
      portNamePath: String,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFVal derives ReadWriter:
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean = false
    protected def protGetConstData(using MemberGetSet): Option[Any] = None
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: PortByNameSelect =>
        this.dfType =~ that.dfType && this.designInstRef =~ that.designInstRef &&
        this.portNamePath == that.portNamePath &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = designInstRef :: dfType.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      dfType = dfType.copyWithNewRefs,
      ownerRef = ownerRef.copyAsNewRef,
      designInstRef = designInstRef.copyAsNewRef
    ).asInstanceOf[this.type]
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
    lazy val getRefs: List[DFRef.TwoWayAny] = relValRef :: dfType.getRefs

  object Alias:
    type Ref = DFRef.TwoWay[DFVal, Alias]
    given ReadWriter[Alias] = ReadWriter.merge(
      summon[ReadWriter[DFVal.Alias.AsIs]],
      summon[ReadWriter[DFVal.Alias.History]],
      summon[ReadWriter[DFVal.Alias.ApplyRange]],
      summon[ReadWriter[DFVal.Alias.ApplyIdx]],
      summon[ReadWriter[DFVal.Alias.SelectField]]
    )
    // This is complete alias that consumes its relative val
    sealed trait Consumer extends Alias:
      val relValRef: ConsumerRef
    type ConsumerRef = DFRef.TwoWay[DFVal, Consumer]

    // This is a partial alias that can propagate its modifier.
    // E.g., a mutable variable `x` that we select its bit `x(1)` is also mutable.
    sealed trait Partial extends Alias, CanBeGlobal derives ReadWriter:
      val relValRef: PartialRef
    type PartialRef = DFRef.TwoWay[DFVal, Partial]

    final case class AsIs(
        dfType: DFType,
        relValRef: PartialRef,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Partial derives ReadWriter:
      protected def protIsFullyAnonymous(using MemberGetSet): Boolean =
        relValRef.get.isFullyAnonymous
      protected def protGetConstData(using MemberGetSet): Option[Any] =
        val relVal = relValRef.get
        relVal.getConstData.map(relValData =>
          dataConversion(dfType, relVal.dfType)(relValData.asInstanceOf[relVal.dfType.Data])
        )
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: AsIs =>
          this.dfType =~ that.dfType && this.relValRef =~ that.relValRef &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected[ir] def protIsSimilarTo(that: CanBeExpr)(using MemberGetSet): Boolean =
        that match
          case that: AsIs =>
            this.dfType.isSimilarTo(that.dfType) &&
            this.relValRef.get.isSimilarTo(that.relValRef.get)
          case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
      def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
      def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
      def copyWithNewRefs(using RefGen): this.type = copy(
        dfType = dfType.copyWithNewRefs,
        ownerRef = ownerRef.copyAsNewRef,
        relValRef = relValRef.copyAsNewRef
      ).asInstanceOf[this.type]
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
    ) extends Consumer derives ReadWriter:
      protected def protIsFullyAnonymous(using MemberGetSet): Boolean =
        relValRef.get.isFullyAnonymous && initRefOption.map(_.get.isFullyAnonymous).getOrElse(true)
      protected def protGetConstData(using MemberGetSet): Option[Any] = None
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
      protected[ir] def protIsSimilarTo(that: CanBeExpr)(using MemberGetSet): Boolean =
        false
      def initOption(using MemberGetSet): Option[DFVal] = initRefOption.map(_.get)
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
      override lazy val getRefs: List[DFRef.TwoWayAny] =
        relValRef :: dfType.getRefs ++ initRefOption
      def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
      def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
      def copyWithNewRefs(using RefGen): this.type = copy(
        dfType = dfType.copyWithNewRefs,
        ownerRef = ownerRef.copyAsNewRef,
        relValRef = relValRef.copyAsNewRef,
        initRefOption = initRefOption.map(_.copyAsNewRef)
      ).asInstanceOf[this.type]
    end History

    object History:
      type InitRef = DFRef.TwoWay[DFVal, History]
      enum Op extends StableEnum derives CanEqual, ReadWriter:
        case State // represents either `prev` in DF domain or `reg` in RT domain
        case Pipe // pipe only represents a pipe constraint under DF domain
      extension (history: DFVal.Alias.History)
        def hasNonBubbleInit(using MemberGetSet): Boolean = history.initRefOption match
          case Some(DFRef(dfVal)) => !dfVal.isBubble
          case _                  => false

    final case class ApplyRange(
        dfType: DFType,
        relValRef: PartialRef,
        idxHighRef: IntParamRef,
        idxLowRef: IntParamRef,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Partial derives ReadWriter:
      def elementWidth(using MemberGetSet): Int = (dfType: @unchecked) match
        case DFBits(_)                     => 1
        case DFVector(cellType = cellType) => cellType.width
      protected def protIsFullyAnonymous(using MemberGetSet): Boolean =
        relValRef.get.isFullyAnonymous
      protected def protGetConstData(using MemberGetSet): Option[Any] =
        val relVal = relValRef.get
        relVal.getConstData.map(relValData =>
          selRangeData(
            relVal.dfType,
            relValData,
            idxHighRef.getInt,
            idxLowRef.getInt
          )
        )
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: ApplyRange =>
          this.dfType =~ that.dfType && this.relValRef =~ that.relValRef &&
          this.idxHighRef =~ that.idxHighRef && this.idxLowRef =~ that.idxLowRef &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected[ir] def protIsSimilarTo(that: CanBeExpr)(using MemberGetSet): Boolean =
        that match
          case that: ApplyRange =>
            this.dfType.isSimilarTo(that.dfType) &&
            this.relValRef.get.isSimilarTo(that.relValRef.get) &&
            this.idxHighRef.isSimilarTo(that.idxHighRef) && this.idxLowRef.isSimilarTo(
              that.idxLowRef
            )
          case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
      override lazy val getRefs: List[DFRef.TwoWayAny] =
        dfType.getRefs ++ List(relValRef) ++ (idxHighRef match
          case ref: DFRef.TypeRef => List(ref);
          case _                  => Nil) ++ (idxLowRef match
          case ref: DFRef.TypeRef => List(ref);
          case _                  => Nil)
      def updateDFType(dfType: DFType): this.type = this
      def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
      def copyWithNewRefs(using RefGen): this.type = copy(
        dfType = dfType.copyWithNewRefs,
        ownerRef = ownerRef.copyAsNewRef,
        relValRef = relValRef.copyAsNewRef,
        idxHighRef = idxHighRef.copyAsNewRef,
        idxLowRef = idxLowRef.copyAsNewRef
      ).asInstanceOf[this.type]
    end ApplyRange
    final case class ApplyIdx(
        dfType: DFType,
        relValRef: PartialRef,
        relIdx: DFVal.Ref,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Partial derives ReadWriter:
      protected def protIsFullyAnonymous(using MemberGetSet): Boolean =
        relValRef.get.isFullyAnonymous && relIdx.get.isFullyAnonymous
      protected def protGetConstData(using MemberGetSet): Option[Any] =
        val relVal = relValRef.get
        relVal.getConstData.flatMap(relValData =>
          val relIdx = this.relIdx.get
          relIdx.getConstData match
            case Some(Some(idx: BigInt)) =>
              val idxInt = idx.toInt
              val outData = relVal.dfType match
                case DFBits(_) =>
                  val data = relValData.asInstanceOf[(BitVector, BitVector)]
                  if (data._2.bit(idxInt)) None
                  else Some(data._1.bit(idxInt))
                case DFVector(_, _) =>
                  relValData.asInstanceOf[Vector[?]](idxInt)
                case _ => ???
              Some(outData)
            case Some(_: None.type) => Some(None)
            case _                  => None
        )
      end protGetConstData
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: ApplyIdx =>
          this.dfType =~ that.dfType && this.relValRef =~ that.relValRef &&
          this.relIdx =~ that.relIdx &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected[ir] def protIsSimilarTo(that: CanBeExpr)(using MemberGetSet): Boolean =
        that match
          case that: ApplyIdx =>
            this.dfType.isSimilarTo(that.dfType) &&
            this.relValRef.get.isSimilarTo(that.relValRef.get) &&
            this.relIdx.get.isSimilarTo(that.relIdx.get)
          case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
      override lazy val getRefs: List[DFRef.TwoWayAny] = relIdx :: relValRef :: dfType.getRefs
      def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
      def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
      def copyWithNewRefs(using RefGen): this.type = copy(
        dfType = dfType.copyWithNewRefs,
        ownerRef = ownerRef.copyAsNewRef,
        relValRef = relValRef.copyAsNewRef,
        relIdx = relIdx.copyAsNewRef
      ).asInstanceOf[this.type]
    end ApplyIdx
    object ApplyIdx:
      object ConstIdx:
        def unapply(idx: DFVal.Const)(using MemberGetSet): Option[Int] =
          idx match
            case DFVal.Const(dfType = DFInt32, data = data: Option[BigInt] @unchecked) =>
              data.map(_.toInt)
            case _ => None

    final case class SelectField(
        dfType: DFType,
        relValRef: PartialRef,
        fieldName: String,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Partial derives ReadWriter:
      protected def protIsFullyAnonymous(using MemberGetSet): Boolean =
        relValRef.get.isFullyAnonymous
      protected def protGetConstData(using MemberGetSet): Option[Any] =
        val relVal = relValRef.get
        relVal.getConstData.map(relValData =>
          val idx = relVal.dfType.asInstanceOf[DFStruct].fieldRelBitLow(fieldName)
          relValData.asInstanceOf[List[?]](idx)
        )
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: SelectField =>
          this.dfType =~ that.dfType && this.relValRef =~ that.relValRef &&
          this.fieldName == that.fieldName &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected[ir] def protIsSimilarTo(that: CanBeExpr)(using MemberGetSet): Boolean =
        that match
          case that: SelectField =>
            this.dfType.isSimilarTo(that.dfType) &&
            this.relValRef.get.isSimilarTo(that.relValRef.get) &&
            this.fieldName == that.fieldName
          case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
      def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
      def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
      def copyWithNewRefs(using RefGen): this.type = copy(
        dfType = dfType.copyWithNewRefs,
        ownerRef = ownerRef.copyAsNewRef,
        relValRef = relValRef.copyAsNewRef
      ).asInstanceOf[this.type]
    end SelectField
  end Alias
end DFVal

final case class DFRange(
    startRef: DFRange.Ref,
    endRef: DFRange.Ref,
    op: DFRange.Op,
    stepRef: DFRange.Ref,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFMember derives ReadWriter:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFRange =>
      this.startRef =~ that.startRef && this.endRef =~ that.endRef && this.stepRef =~ that.stepRef &&
      this.op == that.op &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = List(startRef, endRef, stepRef)
  def copyWithNewRefs(using RefGen): this.type = copy(
    startRef = startRef.copyAsNewRef,
    endRef = endRef.copyAsNewRef,
    stepRef = stepRef.copyAsNewRef,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end DFRange

object DFRange:
  type Ref = DFRef.TwoWay[DFVal, DFRange]
  enum Op extends StableEnum derives CanEqual, ReadWriter:
    case Until, To

final case class DFNet(
    lhsRef: DFNet.Ref,
    op: DFNet.Op,
    rhsRef: DFNet.Ref,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends Statement:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFNet =>
      this.lhsRef =~ that.lhsRef && this.op == that.op && this.rhsRef =~ that.rhsRef &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = List(lhsRef, rhsRef)
  def copyWithNewRefs(using RefGen): this.type = copy(
    lhsRef = lhsRef.copyAsNewRef,
    rhsRef = rhsRef.copyAsNewRef,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end DFNet

object DFNet:
  type Ref = DFRef.TwoWay[DFVal | DFInterfaceOwner, DFNet]
  enum Op extends StableEnum derives CanEqual, ReadWriter:
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
    def unapply(arg: DFNet)(using MemberGetSet): Option[(toVal: DFVal, fromVal: DFVal)] = arg match
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
    def unapply(arg: DFNet)(using MemberGetSet): Option[(toVal: DFVal, fromVal: DFVal)] = arg match
      case Assignment(lhs, rhs) if arg.op == Op.Assignment => Some(lhs, rhs)
      case _                                               => None
  object NBAssignment:
    def unapply(arg: DFNet)(using MemberGetSet): Option[(toVal: DFVal, fromVal: DFVal)] = arg match
      case Assignment(lhs, rhs) if arg.op == Op.NBAssignment => Some(lhs, rhs)
      case _                                                 => None
  object Connection:
    def unapply(net: DFNet)(using
        MemberGetSet
    ): Option[
      (
          toVal: DFVal.Dcl | DFVal.OPEN | DFInterfaceOwner,
          fromVal: DFVal | DFInterfaceOwner,
          swapped: Boolean
      )
    ] =
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

final case class StepBlock(
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFBlock,
      DFMember.Named derives ReadWriter:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: StepBlock =>
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = Nil
  def copyWithNewRefs(using RefGen): this.type = copy(
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end StepBlock
object StepBlock:
  extension (stepBlock: StepBlock)
    def isOnEntry(using MemberGetSet): Boolean = stepBlock.getName == "onEntry"
    def isOnExit(using MemberGetSet): Boolean = stepBlock.getName == "onExit"
    def isRegular(using MemberGetSet): Boolean = stepBlock.getName match
      case "onEntry" | "onExit" => false
      case _                    => true

final case class Goto(
    stepRef: Goto.Ref,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends Statement:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: Goto =>
      this.stepRef =~ that.stepRef &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = List(stepRef)
  def copyWithNewRefs(using RefGen): this.type = copy(
    stepRef = stepRef.copyAsNewRef,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end Goto

object Goto:
  case object ThisStep extends DFMember.Empty
  case object NextStep extends DFMember.Empty
  case object FirstStep extends DFMember.Empty
  type Ref = DFRef.TwoWay[StepBlock | ThisStep.type | NextStep.type | FirstStep.type, Goto]

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
) extends DFDomainOwner derives ReadWriter:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFInterfaceOwner =>
      this.domainType =~ that.domainType &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = domainType.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    domainType = domainType.copyWithNewRefs,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end DFInterfaceOwner

sealed trait DFBlock extends DFOwner
object DFBlock:
  given ReadWriter[DFBlock] = ReadWriter.merge(
    summon[ReadWriter[ProcessBlock]],
    summon[ReadWriter[DFConditional.Block]],
    summon[ReadWriter[DFLoop.Block]],
    summon[ReadWriter[StepBlock]],
    summon[ReadWriter[DomainBlock]],
    summon[ReadWriter[DFDesignBlock]]
  )

final case class ProcessBlock(
    sensitivity: ProcessBlock.Sensitivity,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFBlock,
      DFOwnerNamed derives ReadWriter:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: ProcessBlock =>
      this.sensitivity =~ that.sensitivity &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = sensitivity.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    sensitivity = sensitivity.copyWithNewRefs,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end ProcessBlock
object ProcessBlock:
  sealed trait Sensitivity extends HasRefCompare[Sensitivity], Product, Serializable
      derives CanEqual,
        ReadWriter
  object Sensitivity:
    case object All extends Sensitivity:
      protected def `prot_=~`(that: Sensitivity)(using MemberGetSet): Boolean = that match
        case All => true
        case _   => false
      lazy val getRefs: scala.List[DFRef.TwoWayAny] = Nil
      def copyWithNewRefs(using RefGen): this.type = this
    final case class List(refs: scala.List[DFVal.Ref]) extends Sensitivity:
      protected def `prot_=~`(that: Sensitivity)(using MemberGetSet): Boolean = that match
        case that: List => this.refs.lazyZip(that.refs).forall(_ =~ _)
        case _          => false
      lazy val getRefs: scala.List[DFRef.TwoWayAny] = refs
      def copyWithNewRefs(using RefGen): this.type =
        List(refs.map(_.copyAsNewRef)).asInstanceOf[this.type]
end ProcessBlock

object DFConditional:
  sealed trait Block extends DFBlock derives ReadWriter:
    type THeader <: Header
    val guardRef: Block.GuardRef
    val prevBlockOrHeaderRef: Block.Ref
  object Block:
    type Ref = DFRef.TwoWay[Block | Header, DFMember]
    type GuardRef = DFRef.TwoWay[DFVal | DFMember.Empty, DFMember]

  sealed trait Header extends DFVal.CanBeExpr derives ReadWriter:
    type TBlock <: Block

  final case class DFMatchHeader(
      dfType: DFType,
      selectorRef: DFVal.Ref,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Header derives ReadWriter:
    type TBlock = DFCaseBlock
    // TODO: if all returned expressions in all blocks and the selector is constant, then
    // the returned result is a fully anonymous
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean = false
    // TODO: if all returned expressions in all blocks and the selector is constant, then
    // the returned result is a constant
    protected def protGetConstData(using MemberGetSet): Option[Any] = None
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFMatchHeader =>
        this.dfType =~ that.dfType && this.selectorRef =~ that.selectorRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected[ir] def protIsSimilarTo(that: DFVal.CanBeExpr)(using MemberGetSet): Boolean =
      false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = selectorRef :: dfType.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      dfType = dfType.copyWithNewRefs,
      selectorRef = selectorRef.copyAsNewRef,
      ownerRef = ownerRef.copyAsNewRef
    ).asInstanceOf[this.type]
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
    lazy val getRefs: List[DFRef.TwoWayAny] =
      List(guardRef, prevBlockOrHeaderRef) ++ pattern.getRefs
    def copyWithNewRefs(using RefGen): this.type = copy(
      pattern = pattern.copyWithNewRefs,
      guardRef = guardRef.copyAsNewRef,
      prevBlockOrHeaderRef = prevBlockOrHeaderRef.copyAsNewRef,
      ownerRef = ownerRef.copyAsNewRef
    ).asInstanceOf[this.type]
  end DFCaseBlock
  object DFCaseBlock:
    type Ref = DFRef.TwoWay[DFCaseBlock | DFMatchHeader, Block]
    sealed trait Pattern extends HasRefCompare[Pattern] derives CanEqual, ReadWriter
    object Pattern:
      case object CatchAll extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean = this == that
        lazy val getRefs: List[DFRef.TwoWayAny] = Nil
        def copyWithNewRefs(using RefGen): this.type = this
      final case class Singleton(valueRef: DFVal.Ref) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: Singleton =>
              this.valueRef =~ that.valueRef
            case _ => false
        lazy val getRefs: List[DFRef.TwoWayAny] = List(valueRef)
        def copyWithNewRefs(using RefGen): this.type =
          copy(valueRef.copyAsNewRef).asInstanceOf[this.type]
      final case class Alternative(list: List[Pattern]) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: Alternative =>
              this.list.lazyZip(that.list).forall(_ =~ _)
            case _ => false
        lazy val getRefs: List[DFRef.TwoWayAny] = list.flatMap(_.getRefs)
        def copyWithNewRefs(using RefGen): this.type = copy(
          list.map(_.copyWithNewRefs)
        ).asInstanceOf[this.type]
      final case class Struct(name: String, fieldPatterns: List[Pattern]) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: Struct =>
              this.name == that.name && this.fieldPatterns
                .lazyZip(that.fieldPatterns)
                .forall(_ =~ _)
            case _ => false
        lazy val getRefs: List[DFRef.TwoWayAny] = fieldPatterns.flatMap(_.getRefs)
        def copyWithNewRefs(using RefGen): this.type = copy(
          fieldPatterns = fieldPatterns.map(_.copyWithNewRefs)
        ).asInstanceOf[this.type]
      final case class Bind(ref: Bind.Ref, pattern: Pattern) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: Bind =>
              this.ref =~ that.ref && this.pattern =~ that.pattern
            case _ => false
        lazy val getRefs: List[DFRef.TwoWayAny] = ref :: pattern.getRefs
        def copyWithNewRefs(using RefGen): this.type = copy(
          ref = ref.copyAsNewRef,
          pattern = pattern.copyWithNewRefs
        ).asInstanceOf[this.type]
      final case class NamedArg(name: String, pattern: Pattern) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: NamedArg =>
              this.name == that.name && this.pattern =~ that.pattern
            case _ => false
        lazy val getRefs: List[DFRef.TwoWayAny] = pattern.getRefs
        def copyWithNewRefs(using RefGen): this.type = copy(
          pattern = pattern.copyWithNewRefs
        ).asInstanceOf[this.type]
      object Bind:
        type Ref = DFRef.TwoWay[DFVal, DFCaseBlock]
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
        lazy val getRefs: List[DFRef.TwoWayAny] = refs
        def copyWithNewRefs(using RefGen): this.type = copy(
          refs = refs.map(_.copyAsNewRef)
        ).asInstanceOf[this.type]
      end BindSI
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
    // the returned result is a fully anonymous
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean = false
    // TODO: if all returned expressions in all blocks and the selector is constant, then
    // the returned result is a constant
    protected def protGetConstData(using MemberGetSet): Option[Any] = None
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFIfHeader =>
        this.dfType =~ that.dfType &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = dfType.getRefs
    protected[ir] def protIsSimilarTo(that: DFVal.CanBeExpr)(using MemberGetSet): Boolean =
      false
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      dfType = dfType.copyWithNewRefs,
      ownerRef = ownerRef.copyAsNewRef
    ).asInstanceOf[this.type]
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
    lazy val getRefs: List[DFRef.TwoWayAny] = List(guardRef, prevBlockOrHeaderRef)
    def copyWithNewRefs(using RefGen): this.type = copy(
      guardRef = guardRef.copyAsNewRef,
      prevBlockOrHeaderRef = prevBlockOrHeaderRef.copyAsNewRef,
      ownerRef = ownerRef.copyAsNewRef
    ).asInstanceOf[this.type]
  end DFIfElseBlock
  object DFIfElseBlock:
    type Ref = DFRef.TwoWay[DFIfElseBlock | DFIfHeader, Block]
end DFConditional

object DFLoop:
  sealed trait Block extends DFBlock derives ReadWriter:
    def isCombinational(using MemberGetSet): Boolean = this.hasTagOf[CombinationalTag]
  final case class DFForBlock(
      iteratorRef: DFForBlock.IteratorRef,
      rangeRef: DFForBlock.RangeRef,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Block:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFForBlock =>
        this.iteratorRef =~ that.iteratorRef && this.rangeRef =~ that.rangeRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = List(iteratorRef, rangeRef)
    def copyWithNewRefs(using RefGen): this.type = copy(
      iteratorRef = iteratorRef.copyAsNewRef,
      rangeRef = rangeRef.copyAsNewRef,
      ownerRef = ownerRef.copyAsNewRef
    ).asInstanceOf[this.type]
  end DFForBlock
  object DFForBlock:
    type IteratorRef = DFRef.TwoWay[DFVal.Dcl, DFForBlock]
    type RangeRef = DFRef.TwoWay[DFRange, DFForBlock]

  final case class DFWhileBlock(
      guardRef: DFWhileBlock.GuardRef,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Block:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFWhileBlock =>
        this.guardRef =~ that.guardRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = List(guardRef)
    def copyWithNewRefs(using RefGen): this.type = copy(
      guardRef = guardRef.copyAsNewRef,
      ownerRef = ownerRef.copyAsNewRef
    ).asInstanceOf[this.type]
  end DFWhileBlock
  object DFWhileBlock:
    type GuardRef = DFRef.TwoWay[DFVal, DFWhileBlock]
end DFLoop

final case class DFDesignBlock(
    domainType: DomainType,
    dclMeta: Meta,
    instMode: DFDesignBlock.InstMode,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFBlock,
      DFDomainOwner derives ReadWriter:
  val dclName: String = dclMeta.name
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFDesignBlock =>
      this.domainType =~ that.domainType &&
      this.dclMeta == that.dclMeta &&
      this.instMode == that.instMode &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = domainType.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    domainType = domainType.copyWithNewRefs,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end DFDesignBlock

object DFDesignBlock:
  import InstMode.BlackBox.Source
  enum InstMode extends StableEnum derives CanEqual, ReadWriter:
    case Normal, Def, Simulation
    case BlackBox(verilogSrc: Source, vhdlSrc: Source)
  object InstMode:
    object BlackBox:
      enum Source extends StableEnum derives CanEqual, ReadWriter:
        case NA
        case File(path: String)
        case Library(libName: String, nameSpace: String)

  extension (dsn: DFDesignBlock)
    def isDuplicate: Boolean = dsn.hasTagOf[DuplicateTag]
    def isBlackBox: Boolean = dsn.instMode.isInstanceOf[InstMode.BlackBox]
    def inSimulation: Boolean = dsn.instMode == InstMode.Simulation
    def getCommonDesignWith(dsn2: DFDesignBlock)(using MemberGetSet): DFDesignBlock =
      def getOwnerDesignChain(dsn: DFDesignBlock): List[DFDesignBlock] =
        var chain = List(dsn)
        while (!chain.head.isTop)
          chain = chain.head.getOwnerDesign :: chain
        chain
      var chain1 = getOwnerDesignChain(dsn)
      var chain2 = getOwnerDesignChain(dsn2)
      while (chain1.length > 1 && chain1.drop(1).headOption == chain2.drop(1).headOption)
        chain1 = chain1.drop(1)
        chain2 = chain2.drop(1)
      chain1.head
  end extension

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
      DFDomainOwner derives ReadWriter:
  def flattenMode: dfhdl.hw.annotation.flattenMode = meta.annotations.collectFirst {
    case fm: dfhdl.hw.annotation.flattenMode => fm
  }.getOrElse(dfhdl.hw.annotation.flattenMode.defaultPrefixUnderscore)
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DomainBlock =>
      this.domainType =~ that.domainType &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = domainType.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    domainType = domainType.copyWithNewRefs,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end DomainBlock

// sealed trait Timer extends DFMember.Named
// object Timer:
//   type Ref = DFRef.TwoWay[Timer, DFMember]
//   type TriggerRef = DFRef.TwoWay[DFVal | DFMember.Empty, DFMember]
//   final case class Periodic(
//       triggerRef: TriggerRef,
//       rateOpt: Option[Rate],
//       ownerRef: DFOwner.Ref,
//       meta: Meta,
//       tags: DFTags
//   ) extends Timer:
//     protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
//       case that: Periodic =>
//         this.triggerRef =~ that.triggerRef && this.rateOpt == that.rateOpt &&
//         this.meta =~ that.meta && this.tags =~ that.tags
//       case _ => false
//     protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
//     protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
//     lazy val getRefs: List[DFRef.TwoWayAny] = List(triggerRef)
//     def copyWithNewRefs(using RefGen): this.type = copy(
//       triggerRef = triggerRef.copyAsNewRef,
//       ownerRef = ownerRef.copyAsNewRef
//     ).asInstanceOf[this.type]
//   end Periodic

//   final case class Func(
//       sourceRef: Timer.Ref,
//       op: Func.Op,
//       arg: Time | Ratio,
//       ownerRef: DFOwner.Ref,
//       meta: Meta,
//       tags: DFTags
//   ) extends Timer:
//     protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
//       case that: Func =>
//         this.sourceRef =~ that.sourceRef && this.op == that.op && this.arg == that.arg &&
//         this.meta =~ that.meta && this.tags =~ that.tags
//       case _ => false
//     protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
//     protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
//     lazy val getRefs: List[DFRef.TwoWayAny] = List(sourceRef)
//     def copyWithNewRefs(using RefGen): this.type = copy(
//       sourceRef = sourceRef.copyAsNewRef,
//       ownerRef = ownerRef.copyAsNewRef
//     ).asInstanceOf[this.type]
//   end Func
//   object Func:
//     enum Op derives CanEqual:
//       case Delay, `*`, /

//   final case class IsActive(
//       timerRef: Ref,
//       ownerRef: DFOwner.Ref,
//       meta: Meta,
//       tags: DFTags
//   ) extends DFVal.CanBeExpr:
//     val dfType: DFType = DFBool
//     // TODO: revisit this in the future. can an active indication of a timer be fully anonymous?
//     protected def protIsFullyAnonymous(using MemberGetSet): Boolean = false
//     protected def protGetConstData(using MemberGetSet): Option[Any] = None
//     protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
//       case that: IsActive =>
//         this.timerRef =~ that.timerRef &&
//         this.meta =~ that.meta && this.tags =~ that.tags
//       case _ => false
//     protected[ir] def protIsSimilarTo(that: DFVal.CanBeExpr)(using MemberGetSet): Boolean =
//       false
//     protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
//     protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
//     lazy val getRefs: List[DFRef.TwoWayAny] = List(timerRef)
//     def updateDFType(dfType: DFType): this.type = this
//     def copyWithNewRefs(using RefGen): this.type = copy(
//       timerRef = timerRef.copyAsNewRef,
//       ownerRef = ownerRef.copyAsNewRef
//     ).asInstanceOf[this.type]
//   end IsActive
// end Timer

final case class Wait(
    triggerRef: Wait.TriggerRef,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends Statement:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: Wait =>
      this.triggerRef =~ that.triggerRef &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = List(triggerRef)
  def copyWithNewRefs(using RefGen): this.type = copy(
    triggerRef = triggerRef.copyAsNewRef,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end Wait

object Wait:
  type TriggerRef = DFRef.TwoWay[DFVal, Wait]

final case class TextOut(
    op: TextOut.Op,
    msgParts: List[String],
    msgArgs: List[DFVal.Ref],
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends Statement:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: TextOut =>
      this.op =~ that.op && this.msgParts == that.msgParts &&
      this.msgArgs.lazyZip(that.msgArgs).forall(_ =~ _) &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = op.getRefs ++ msgArgs
  def copyWithNewRefs(using RefGen): this.type = copy(
    op = op.copyWithNewRefs,
    msgArgs = msgArgs.map(_.copyAsNewRef),
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end TextOut

object TextOut:
  type AssertionRef = DFRef.TwoWay[DFVal, TextOut]
  enum Severity extends StableEnum derives CanEqual, ReadWriter:
    case Info, Warning, Error, Fatal
  enum Op extends HasRefCompare[Op], StableEnum derives CanEqual, ReadWriter:
    case Print, Println, Debug, Finish
    case Report(severity: Severity) extends Op
    case Assert(assertionRef: AssertionRef, severity: Severity) extends Op
    lazy val getRefs: List[DFRef.TwoWayAny] = this match
      case Assert(assertion, _) => List(assertion)
      case _                    => Nil
    protected def `prot_=~`(that: Op)(using MemberGetSet): Boolean = (this, that) match
      case (thisAssert: Assert, thatAssert: Assert) =>
        thisAssert.assertionRef =~ thatAssert.assertionRef && thisAssert.severity == thatAssert.severity
      case _ => this equals that
    def copyWithNewRefs(using RefGen): this.type = this match
      case Assert(assertionRef, severity) =>
        Assert(assertionRef = assertionRef.copyAsNewRef, severity = severity)
          .asInstanceOf[this.type]
      case _ => this
  end Op
end TextOut
