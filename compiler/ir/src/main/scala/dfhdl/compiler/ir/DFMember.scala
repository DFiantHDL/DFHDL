package dfhdl.compiler
package ir
import dfhdl.internals.*
import upickle.default.*
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.reflect.{ClassTag, classTag}
import dfhdl.compiler.ir.PhysicalNumber.Ops.MHz

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
    case o: DFOwner => o
    case _: DFMember.Empty =>
      // Empty ownerRef on a DFDesignBlock typically indicates a non-top
      // sub-design whose `ownerRef` has been remapped to Empty under the new
      // convention. Recover the parent owner via any DFDesignInst whose
      // `designRef` targets this block (every non-top design has at least
      // one instance after duplicate elimination). The actual top has no
      // DFDesignInst, so it falls through to the standard "No owner" throw.
      this match
        case d: DFDesignBlock =>
          getSet.designDB.designBlockInstMap.get(d).flatMap(_.headOption) match
            case Some(inst) => inst.getOwner
            case None       =>
              throw new Exception(s"No owner found for member $this.")
        case _ =>
          throw new Exception(s"No owner found for member $this.")
  final def getOwnerNamed(using MemberGetSet): DFOwnerNamed = getOwner match
    case b: DFOwnerNamed => b
    case o               => o.getOwnerNamed
  final def getOwnerBlock(using MemberGetSet): DFBlock = getOwner match
    case b: DFBlock => b
    case o          => o.getOwnerBlock
  final def getOwnerStepBlock(using MemberGetSet): StepBlock = getOwner match
    case b: StepBlock => b
    case o            => o.getOwnerStepBlock
  final def getOwnerProcessBlock(using MemberGetSet): ProcessBlock = getOwner match
    case b: ProcessBlock => b
    case o               => o.getOwnerProcessBlock
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
      case thisDsn: DFDesignBlock if !getSet.isMutable =>
        getSet.designDB.designBlockInstMap(thisDsn).exists(_.isInsideOwner(thatOwner))
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
    var dsn = Set(inside)
    while (!dsn.contains(outside) && dsn.nonEmpty)
      distance = distance + 1
      dsn = dsn.flatMap(getSet.designDB.designBlockOwnershipMap(_))
    assert(dsn.nonEmpty)
    distance
end DFMember

object DFMember:
  given ReadWriter[DFMember] = ReadWriter.merge(
    summon[ReadWriter[DFMember.Empty]],
    summon[ReadWriter[DFVal]],
    summon[ReadWriter[Statement]],
    summon[ReadWriter[DFInterfaceOwner]],
    summon[ReadWriter[DFBlock]],
    summon[ReadWriter[DFConditional.Header]],
    summon[ReadWriter[DFRange]],
    summon[ReadWriter[DFDesignInst]]
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
      case DomainType.RT => true
      case _             => false
    def isInEDDomain(using MemberGetSet): Boolean = member.getDomainType match
      case DomainType.ED => true
      case _             => false
    def isInProcess(using MemberGetSet): Boolean = member.isOwnedCond(cond = {
      case _: ProcessBlock  => Some(true)
      case _: DFDomainOwner => Some(false)
      case _                => None
    })
    def toJson(using Writer[DFMember]): String = write(member)
    def getConstraints(using getSet: MemberGetSet): List[constraints.Constraint] =
      val allAnnotations = member match
        case design: DFDesignBlock =>
          design.meta.annotations
        case designInst: DFDesignInst =>
          designInst.meta.annotations ++ designInst.getDesignBlock.getConstraints
        case interface: DFInterfaceOwner =>
          interface.dclMeta.annotations.view ++ interface.meta.annotations
        case _ => member.meta.annotations.view
      allAnnotations.collect {
        case c: constraints.Constraint => c
      }.toList
    end getConstraints
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
    given ReadWriter[Empty] = ReadWriter.merge(
      summon[ReadWriter[Empty.type]],
      summon[ReadWriter[Goto.ThisStep.type]],
      summon[ReadWriter[Goto.NextStep.type]],
      summon[ReadWriter[Goto.FirstStep.type]]
    )
    given ReadWriter[Empty.type] = macroRW

  sealed trait Named extends DFMember:
    final def getName(using MemberGetSet): String = this match
      case o: DFDesignBlock if o.isTop          => o.dclName
      case o: DFDesignBlock if getSet.isMutable => o.getCachedDesignInst.getName
      case _                                    => meta.name
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
end DFMember

sealed trait Statement extends DFMember derives ReadWriter

sealed trait DFVal extends DFMember.Named:
  val dfType: DFType
  def widthUNSAFE(using MemberGetSet): Int = widthIntOpt.get
  def widthIntOpt(using MemberGetSet): Option[Int] = dfType.widthIntOpt
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
  final def isConst(using MemberGetSet): Boolean = getConstData[Any].isConst
  protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any]
  private var cachedConstDataReady: Boolean = false
  private var cachedConstData: ConstData[Any] = ConstData.NotConst
  final def wasConstDataAccessed: Boolean = cachedConstDataReady
  final def getConstData[T](using
      getSet: MemberGetSet,
      policy: ConstData.CachePolicy
  ): ConstData[T] =
    policy match
      case ConstData.CachePolicy.NoCache =>
        cachedConstDataReady = false
      case ConstData.CachePolicy.GoThroughDesignParams if this.isDesignParam =>
        cachedConstDataReady = false
      case _ =>
    if (cachedConstDataReady) cachedConstData.asInstanceOf[ConstData[T]]
    else
      cachedConstData = protGetConstData
      // disable NotConst constant data caching during mutation, since some data like CLK_FREQ
      // cannot be attained during mutation and returns NotConst
      cachedConstDataReady =
        policy match
          case ConstData.CachePolicy.NoCache                                     => false
          case ConstData.CachePolicy.GoThroughDesignParams if this.isDesignParam => false
          case _                                                                 =>
            !(getSet.isMutable && cachedConstData == ConstData.NotConst)
      cachedConstData.asInstanceOf[ConstData[T]]
  end getConstData
  final def getConstDataThroughParams[T](using MemberGetSet): Option[T] =
    getConstData[T](using getSet, ConstData.CachePolicy.GoThroughDesignParams).toOption
  final def getConstDataOrDefault[T](using MemberGetSet): T =
    getConstDataThroughParams[T].getOrElse(dfType.defaultData.asInstanceOf[T])
  def updateDFType(dfType: DFType): this.type
end DFVal

object DFVal:
  type Ref = DFRef.TwoWay[DFVal, DFMember]
  given ReadWriter[DFVal] = ReadWriter.merge(
    summon[ReadWriter[DFVal.Dcl]],
    summon[ReadWriter[DFVal.Special]],
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
    enum Dir derives CanEqual, ReadWriter:
      case VAR, IN, OUT, INOUT
    export Dir.{VAR, IN, OUT, INOUT}
    enum Special derives CanEqual, ReadWriter:
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
    def isPortOutPBNS(using MemberGetSet): Boolean = dfVal match
      case pbns: DFVal.PortByNameSelect =>
        pbns.dir match
          case Modifier.OUT => true
          case _            => false
      case _ => dfVal.isPortOut
    def isPortIn: Boolean = dfVal match
      case dcl: DFVal.Dcl =>
        dcl.modifier.dir match
          case Modifier.IN => true
          case _           => false
      case _ => false
    def isPortInPBNS(using MemberGetSet): Boolean = dfVal match
      case pbns: DFVal.PortByNameSelect =>
        pbns.dir match
          case Modifier.IN => true
          case _           => false
      case _ => dfVal.isPortIn
    def isVar: Boolean = dfVal match
      case dcl: DFVal.Dcl =>
        dcl.modifier.dir match
          case Modifier.VAR => true
          case _            => false
      case _ => false
    def isOpen: Boolean = dfVal match
      case DFVal.Special(kind = DFVal.Special.OPEN) => true
      case _                                        => false
    def isDesignParam: Boolean = dfVal match
      case _: DFVal.DesignParam => true
      case _                    => false
    def isReg: Boolean = dfVal match
      case dcl: DFVal.Dcl => dcl.modifier.isReg
      case _              => false
    @tailrec def dealias(using
        MemberGetSet
    ): Option[ConnectToVal] = dfVal match
      case dcl: DFVal.Dcl                     => Some(dcl)
      case pbns: DFVal.PortByNameSelect       => Some(pbns)
      case open: DFVal.Special if open.isOpen => Some(open)
      case alias: DFVal.Alias                 => alias.relValRef.get.dealias
      case _                                  => None
    @tailrec private def departial(slice: Slice)(using MemberGetSet): (DFVal, Slice) =
      dfVal match
        case partial: DFVal.Alias.Partial =>
          val relVal = partial.relValRef.get
          partial match
            case partial: DFVal.Alias.ApplyRange =>
              partial.idxLowRef.getIntOpt match
                case Some(idxLow) => relVal.departial(slice.shift(idxLow))
                case None         => relVal.departial(Slice.Unknown)
            case partial: DFVal.Alias.ApplyIdx =>
              partial.relIdx.get match
                case DFVal.Alias.ApplyIdx.ConstIdx(idx) =>
                  partial.dfType.widthIntOpt match
                    case Some(w) => relVal.departial(slice.shift(idx * w))
                    case None    => relVal.departial(Slice.Unknown)
                // if not a constant index selection, then the entire value range is affected
                case _ =>
                  relVal.dealias match
                    case Some(dcl: DFVal.Dcl) => (dcl, Slice.fromWidthOpt(dcl.dfType.widthIntOpt))
                    case _ => (relVal, Slice.fromWidthOpt(relVal.dfType.widthIntOpt))
            case partial: DFVal.Alias.SelectField =>
              relVal.dfType match
                case structType: DFStruct =>
                  relVal.departial(slice.shift(structType.fieldRelBitLow(partial.fieldName)))
                case _ => relVal.departial(slice)
            case _ => relVal.departial(slice)
          end match
        case _ => (dfVal, slice)
      end match
    end departial
    // for a given value remove partial selections as possible
    def departial(using MemberGetSet): (DFVal, Slice) =
      departial(Slice.fromWidthOpt(dfVal.dfType.widthIntOpt))
    def departialPBNS(using MemberGetSet): Option[(ConnectToVal, Slice)] =
      departial match
        case (dcl: DFVal.Dcl, slice)                     => Some(dcl, slice)
        case (pbns: DFVal.PortByNameSelect, slice)       => Some(pbns, slice)
        case (open: DFVal.Special, slice) if open.isOpen => Some(open, slice)
        case _                                           => None
    def departialDcl(using MemberGetSet): Option[(DFVal.Dcl, Slice)] =
      departial match
        case (dcl: DFVal.Dcl, slice) => Some(dcl, slice)
        case _                       => None
    def isBubble(using MemberGetSet): Boolean =
      dfVal match
        case c: DFVal.Const          => c.dfType.isDataBubble(c.data.asInstanceOf[c.dfType.Data])
        case f: DFVal.Func           => f.args.exists(_.get.isBubble)
        case a: DFVal.Alias.ApplyIdx => a.relValRef.get.isBubble || a.relIdx.get.isBubble
        case a: DFVal.Alias.Partial  => a.relValRef.get.isBubble
        case _                       => false
  end extension
  // can be an expression
  sealed trait CanBeExpr extends DFVal

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
  ) extends CanBeExpr, CanBeGlobal derives ReadWriter:
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean = this.isAnonymous
    protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any] =
      ConstData.KnownConst(data)
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Const =>
        this.dfType =~ that.dfType && this.data.equals(that.data) &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = dfType.getRefs ++ meta.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      meta = meta.copyWithNewRefs,
      dfType = dfType.copyWithNewRefs,
      ownerRef = ownerRef.copyAsNewRef
    ).asInstanceOf[this.type]
  end Const

  final case class DesignParam(
      dfType: DFType,
      defaultValRef: DesignParam.DefaultValRef,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends CanBeExpr derives ReadWriter:
    assert(!this.isAnonymous, "Design parameters cannot be anonymous.")
    // the value will be cached during elaboration, because the reference via the design's paramMap
    // will not be available until the design is fully elaborated. during initial contruction and mutation,
    // the value will be cached in core.DFVal.DesignParam, and later cleared in core.Design
    private var cachedAppliedVal: Option[DFVal] = None
    protected[compiler] def appliedValRefOpt(using MemberGetSet): Option[DFDesignInst.ParamRef] =
      val ownerDesign = getOwnerDesign
      if (ownerDesign.isTop) None
      else
        val instOpt =
          if (getSet.isMutable) Some(ownerDesign.getCachedDesignInst)
          else getSet.designDB.designBlockInstMap.get(ownerDesign).flatMap(_.headOption)
        instOpt.flatMap(_.paramMap.get(getName))
    def appliedValOpt(using MemberGetSet): Option[DFVal] =
      if (getSet.isMutable) cachedAppliedVal.orElse(appliedValRefOpt.map(_.get))
      else appliedValRefOpt.map(_.get)
    def appliedOrDefaultValRef(using MemberGetSet): DFVal.Ref =
      appliedValRefOpt.getOrElse(defaultValRef.asInstanceOf[DFVal.Ref])
    def appliedOrDefaultVal(using MemberGetSet): DFVal =
      appliedValOpt.getOrElse(defaultValRef.get.asInstanceOf[DFVal])
    protected[dfhdl] def setCachedAppliedVal(dfVal: DFVal): Unit = cachedAppliedVal = Some(dfVal)
    protected[dfhdl] def clearCachedAppliedVal(): Unit = cachedAppliedVal = None
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean = false
    protected def protGetConstData(using
        getSet: MemberGetSet,
        policy: ConstData.CachePolicy
    ): ConstData[Any] =
      if (this.getOwnerDesign.isDeviceTop || policy != ConstData.CachePolicy.Always)
        val updatedPolicy = policy match
          // once hitting a design parameter, if the policy is GoThroughDesignParams,
          // it should be updated to NoCache to avoid irrelevant data caching
          case ConstData.CachePolicy.GoThroughDesignParams => ConstData.CachePolicy.NoCache
          case other                                       => other
        appliedOrDefaultVal.getConstData(using getSet, updatedPolicy)
      else ConstData.UnknownConst(this)
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DesignParam =>
        // design parameters are considered to be the same even if they are referencing
        // a different member (this should be quite common), because that member is
        // external to the design. however, different default value is considered to be a
        // different design parameter.
        this.dfType =~ that.dfType && this.defaultValRef =~ that.defaultValRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] =
      defaultValRef :: dfType.getRefs ++ meta.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      meta = meta.copyWithNewRefs,
      dfType = dfType.copyWithNewRefs,
      ownerRef = ownerRef.copyAsNewRef,
      defaultValRef = defaultValRef.copyAsNewRef
    ).asInstanceOf[this.type]
  end DesignParam
  object DesignParam:
    type DefaultValRef = DFRef.TwoWay[DFVal | DFMember.Empty, DesignParam]

  final case class Special(
      dfType: DFType,
      kind: Special.Kind,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends CanBeExpr derives ReadWriter:
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean = true
    protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any] =
      kind match
        case Special.CLK_FREQ =>
          if (getSet.isMutable) ConstData.NotConst // disable during elaboration
          else
            // Read the resolved @timing.clock(rate = ...) for this owner. The
            // DB.resolvedClkRstMap lazy val handles default-derivation,
            // related-domain walking, and relaxation in one shot — no separate
            // stage needs to run first.
            val rate = getSet.designDB.resolvedClkRstMap.get(this.getOwnerDomain)
              .flatMap(_._1)
              .flatMap(_.rate.toOption)
              .map(_.to_freq)
              .getOrElse(0.MHz)
            ConstData.KnownConst(rate)
        case _ => ConstData.NotConst
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Special =>
        this.dfType =~ that.dfType && this.kind == that.kind &&
        this.meta =~ that.meta && this.tags =~ that.tags

      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = dfType.getRefs ++ meta.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      meta = meta.copyWithNewRefs,
      dfType = dfType.copyWithNewRefs,
      ownerRef = ownerRef.copyAsNewRef
    ).asInstanceOf[this.type]
  end Special
  object Special:
    enum Kind derives CanEqual, ReadWriter:
      case NOTHING, OPEN, CLK_FREQ
    export Kind.{NOTHING, OPEN, CLK_FREQ}

  final case class Dcl(
      dfType: DFType,
      modifier: Modifier,
      initRefList: List[Dcl.InitRef],
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFVal derives ReadWriter:
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean = false
    protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any] =
      ConstData.NotConst
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Dcl =>
        this.dfType =~ that.dfType && this.modifier == that.modifier &&
        this.initRefList =~ that.initRefList &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    def initList(using MemberGetSet): List[DFVal] = initRefList.map(_.get)
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = dfType.getRefs ++ initRefList ++ meta.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      meta = meta.copyWithNewRefs,
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
  // Funcs with associative ops (+, *, &, |, ^, ++) may have more than 2 args when
  // consecutive same-op anonymous Funcs are merged during elaboration (e.g., `a + b + c`
  // produces a single `Func(+, [a, b, c])` instead of nested binary Funcs).
  // + and * are only merged when all args have the same width (non-carry).
  // - is NOT merged (not truly associative for reordering).
  // ++ is only merged for flat DFBits concatenation, not struct/vector/string.
  // The position spans from the first operand to the last in the merged chain.
  final case class Func(
      dfType: DFType,
      op: Func.Op,
      args: List[DFVal.Ref],
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends CanBeExpr, CanBeGlobal derives ReadWriter:
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean =
      args.forall(_.get.isFullyAnonymous)
    protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any] =
      val args = this.args.map(_.get)
      val argConstData = args.map(_.getConstData[Any])
      if (argConstData.exists(_ == ConstData.NotConst)) ConstData.NotConst
      else if (argConstData.view.exists(_.isInstanceOf[ConstData.UnknownConst[?]]))
        ConstData.UnknownConst(this)
      else
        val argData = argConstData.collect { case ConstData.KnownConst(d) => d }
        val argTypes = args.map(_.dfType)
        ConstData.KnownConst(calcFuncData(dfType, op, argTypes, argData))
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Func =>
        this.dfType =~ that.dfType && this.op == that.op && this.args =~ that.args &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = dfType.getRefs ++ args ++ meta.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      meta = meta.copyWithNewRefs,
      dfType = dfType.copyWithNewRefs,
      ownerRef = ownerRef.copyAsNewRef,
      args = args.map(_.copyAsNewRef)
    ).asInstanceOf[this.type]
  end Func

  object Func:
    enum Op derives CanEqual, ReadWriter:
      case +, -, *, /, ===, =!=, <, >, <=, >=, &, |, ^, %, ++
      case >>, <<, **, ror, rol, reverse, repeat
      case unary_-, unary_~, unary_!
      case rising, falling
      case clog2, max, min, abs, sel
      // special-case of initFile construct for vectors of bits
      case InitFile(format: InitFileFormat, path: String)
    object Op:
      val associativeSet = Set(Op.+, Op.-, Op.`*`, Op.&, Op.|, Op.^, Op.++, Op.max, Op.min)

  final case class PortByNameSelect(
      dfType: DFType,
      dir: Modifier.Dir,
      designInstRef: PortByNameSelect.Ref,
      portNamePath: String,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFVal derives ReadWriter:
    def getDesignInst(using MemberGetSet): DFDesignInst = designInstRef.get
    def isIn: Boolean = dir == Modifier.IN
    def isOut: Boolean = dir == Modifier.OUT
    def portName: String = portNamePath.split('.').last
    protected def protIsFullyAnonymous(using MemberGetSet): Boolean = false
    protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any] =
      ConstData.NotConst
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: PortByNameSelect =>
        this.dfType =~ that.dfType && this.designInstRef =~ that.designInstRef &&
        this.portNamePath == that.portNamePath &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = designInstRef :: dfType.getRefs ++ meta.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      meta = meta.copyWithNewRefs,
      dfType = dfType.copyWithNewRefs,
      ownerRef = ownerRef.copyAsNewRef,
      designInstRef = designInstRef.copyAsNewRef
    ).asInstanceOf[this.type]
  end PortByNameSelect
  object PortByNameSelect:
    type Ref = DFRef.TwoWay[DFDesignInst, PortByNameSelect]

  sealed trait Alias extends CanBeExpr:
    val relValRef: Alias.Ref
    lazy val getRefs: List[DFRef.TwoWayAny] = relValRef :: dfType.getRefs ++ meta.getRefs

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
      protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any] =
        val relVal = relValRef.get
        relVal.getConstData[Any] match
          case ConstData.KnownConst(relValData) =>
            dfType.widthIntOpt match
              case Some(_) =>
                ConstData.KnownConst(
                  dataConversion(dfType, relVal.dfType)(relValData.asInstanceOf[relVal.dfType.Data])
                )
              case None => ConstData.UnknownConst(this)
          case other => other
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: AsIs =>
          this.dfType =~ that.dfType && this.relValRef =~ that.relValRef &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
      def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
      def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
      def copyWithNewRefs(using RefGen): this.type = copy(
        meta = meta.copyWithNewRefs,
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
      protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any] =
        ConstData.NotConst
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
      override lazy val getRefs: List[DFRef.TwoWayAny] =
        relValRef :: dfType.getRefs ++ initRefOption ++ meta.getRefs
      def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
      def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
      def copyWithNewRefs(using RefGen): this.type = copy(
        meta = meta.copyWithNewRefs,
        dfType = dfType.copyWithNewRefs,
        ownerRef = ownerRef.copyAsNewRef,
        relValRef = relValRef.copyAsNewRef,
        initRefOption = initRefOption.map(_.copyAsNewRef)
      ).asInstanceOf[this.type]
    end History

    object History:
      type InitRef = DFRef.TwoWay[DFVal, History]
      enum Op derives CanEqual, ReadWriter:
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
      def elementWidthUNSAFE(using MemberGetSet): Int = dfType.runtimeChecked match
        case DFBits(_) | DFUInt(_) | DFSInt(_) => 1
        case DFVector(cellType = cellType)     => cellType.widthUNSAFE
      def elementWidthIntOpt(using MemberGetSet): Option[Int] = dfType.runtimeChecked match
        case DFBits(_) | DFUInt(_) | DFSInt(_) => Some(1)
        case DFVector(cellType = cellType)     => cellType.widthIntOpt
        case _                                 => None
      protected def protIsFullyAnonymous(using MemberGetSet): Boolean =
        relValRef.get.isFullyAnonymous
      protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any] =
        val relVal = relValRef.get
        (relVal.getConstData[Any], idxHighRef.getIntConstData, idxLowRef.getIntConstData) match
          case (
                ConstData.KnownConst(relValData),
                ConstData.KnownConst(idxHigh),
                ConstData.KnownConst(idxLow)
              ) =>
            ConstData.KnownConst(
              selRangeData(relVal.dfType, relValData, idxHigh, idxLow)
            )
          case (
                ConstData.NotConst,
                _,
                _
              ) | (_, ConstData.NotConst, _) | (_, _, ConstData.NotConst) =>
            ConstData.NotConst
          case _ => ConstData.UnknownConst(this)
        end match
      end protGetConstData
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: ApplyRange =>
          this.dfType =~ that.dfType && this.relValRef =~ that.relValRef &&
          this.idxHighRef =~ that.idxHighRef && this.idxLowRef =~ that.idxLowRef &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
      override lazy val getRefs: List[DFRef.TwoWayAny] =
        dfType.getRefs ++ meta.getRefs ++ List(relValRef) ++
          (idxHighRef match
            case ref: DFRef.TypeRef => List(ref);
            case _                  => Nil) ++
          (idxLowRef match
            case ref: DFRef.TypeRef => List(ref);
            case _                  => Nil)
      def updateDFType(dfType: DFType): this.type = this
      def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
      def copyWithNewRefs(using RefGen): this.type = copy(
        meta = meta.copyWithNewRefs,
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
      protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any] =
        val relVal = relValRef.get
        val relIdx = this.relIdx.get
        (relVal.getConstData[Any], relIdx.getConstData[Option[BigInt]]) match
          case (ConstData.KnownConst(relValData), ConstData.KnownConst(Some(idx: BigInt))) =>
            val idxInt = idx.toInt
            val outData = relVal.dfType match
              case DFBits(_) =>
                val data = relValData.asInstanceOf[(BitVector, BitVector)]
                if (data._2.bit(idxInt)) None
                else Some(data._1.bit(idxInt))
              case DFUInt(_) | DFSInt(_) =>
                relValData.asInstanceOf[Option[BigInt]].map(_.testBit(idxInt))
              case DFVector(_, _) =>
                relValData.asInstanceOf[Vector[?]](idxInt)
              case _ => ???
            ConstData.KnownConst(outData)
          case (ConstData.KnownConst(_), ConstData.KnownConst(_: None.type)) =>
            ConstData.KnownConst(None)
          case (ConstData.UnknownConst(_), _) | (_, ConstData.UnknownConst(_)) =>
            ConstData.UnknownConst(this)
          case _ => ConstData.NotConst
        end match
      end protGetConstData
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: ApplyIdx =>
          this.dfType =~ that.dfType && this.relValRef =~ that.relValRef &&
          this.relIdx =~ that.relIdx &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
      override lazy val getRefs: List[DFRef.TwoWayAny] =
        relIdx :: relValRef :: dfType.getRefs ++ meta.getRefs
      def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
      def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
      def copyWithNewRefs(using RefGen): this.type = copy(
        meta = meta.copyWithNewRefs,
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
      protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any] =
        val relVal = relValRef.get
        relVal.getConstData[Any] match
          case ConstData.KnownConst(relValData) =>
            val idx = relVal.dfType.asInstanceOf[DFStruct].fieldRelBitLow(fieldName)
            ConstData.KnownConst(relValData.asInstanceOf[List[?]](idx))
          case other => other
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: SelectField =>
          this.dfType =~ that.dfType && this.relValRef =~ that.relValRef &&
          this.fieldName == that.fieldName &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
      def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
      def copyWithoutGlobalCtx: this.type = copy().asInstanceOf[this.type]
      def copyWithNewRefs(using RefGen): this.type = copy(
        meta = meta.copyWithNewRefs,
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
      this.startRef =~ that.startRef && this.endRef =~ that.endRef &&
      this.stepRef =~ that.stepRef &&
      this.op == that.op &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = startRef :: endRef :: stepRef :: meta.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    meta = meta.copyWithNewRefs,
    startRef = startRef.copyAsNewRef,
    endRef = endRef.copyAsNewRef,
    stepRef = stepRef.copyAsNewRef,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end DFRange

object DFRange:
  type Ref = DFRef.TwoWay[DFVal, DFRange]
  enum Op derives CanEqual, ReadWriter:
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
  lazy val getRefs: List[DFRef.TwoWayAny] = lhsRef :: rhsRef :: meta.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    meta = meta.copyWithNewRefs,
    lhsRef = lhsRef.copyAsNewRef,
    rhsRef = rhsRef.copyAsNewRef,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end DFNet

object DFNet:
  type Ref = DFRef.TwoWay[DFVal | DFInterfaceOwner, DFNet]
  enum Op derives CanEqual, ReadWriter:
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
          toVal: DFVal.Dcl | DFVal.PortByNameSelect | DFVal.Special | DFInterfaceOwner,
          fromVal: DFVal | DFInterfaceOwner,
          swapped: Boolean
      )
    ] =
      if (net.isConnection) (net.lhsRef.get, net.rhsRef.get) match
        case (lhsVal: DFVal, rhsVal: DFVal) =>
          val toLeft = getSet.designDB.connectionTable.getNets(lhsVal).contains(net)
          if (toLeft) Some(lhsVal.dealias.get, rhsVal, false)
          else Some(rhsVal.dealias.get, lhsVal, true)
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
  lazy val getRefs: List[DFRef.TwoWayAny] = meta.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    meta = meta.copyWithNewRefs,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end StepBlock
object StepBlock:
  extension (stepBlock: StepBlock)
    def isOnEntry(using MemberGetSet): Boolean = stepBlock.getName == "onEntry"
    def isOnExit(using MemberGetSet): Boolean = stepBlock.getName == "onExit"
    def isFallThrough(using MemberGetSet): Boolean = stepBlock.getName == "fallThrough"
    def isRegular(using MemberGetSet): Boolean = stepBlock.getName match
      case "onEntry" | "onExit" | "fallThrough" => false
      case _                                    => true

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
  lazy val getRefs: List[DFRef.TwoWayAny] = List(stepRef) ++ meta.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    meta = meta.copyWithNewRefs,
    stepRef = stepRef.copyAsNewRef,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end Goto

object Goto:
  case object ThisStep extends DFMember.Empty
  given ReadWriter[ThisStep.type] = macroRW
  case object NextStep extends DFMember.Empty
  given ReadWriter[NextStep.type] = macroRW
  case object FirstStep extends DFMember.Empty
  given ReadWriter[FirstStep.type] = macroRW
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
    dclMeta: Meta,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFDomainOwner derives ReadWriter:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFInterfaceOwner =>
      this.domainType =~ that.domainType &&
      this.dclMeta =~ that.dclMeta && this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = domainType.getRefs ++ meta.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    dclMeta = dclMeta.copyWithNewRefs,
    meta = meta.copyWithNewRefs,
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
  lazy val getRefs: List[DFRef.TwoWayAny] = sensitivity.getRefs ++ meta.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    meta = meta.copyWithNewRefs,
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
        case that: List => this.refs =~ that.refs
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
    protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any] =
      ConstData.NotConst
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFMatchHeader =>
        this.dfType =~ that.dfType && this.selectorRef =~ that.selectorRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = selectorRef :: dfType.getRefs ++ meta.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      meta = meta.copyWithNewRefs,
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
      meta = meta.copyWithNewRefs,
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
            case that: Alternative => this.list =~ that.list
            case _                 => false
        lazy val getRefs: List[DFRef.TwoWayAny] = list.flatMap(_.getRefs)
        def copyWithNewRefs(using RefGen): this.type = copy(
          list.map(_.copyWithNewRefs)
        ).asInstanceOf[this.type]
      final case class Struct(name: String, fieldPatterns: List[Pattern]) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: Struct => this.name == that.name && this.fieldPatterns =~ that.fieldPatterns
            case _            => false
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
              this.op == that.op && this.parts == that.parts && this.refs =~ that.refs
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
    protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any] =
      ConstData.NotConst
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFIfHeader =>
        this.dfType =~ that.dfType &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    lazy val getRefs: List[DFRef.TwoWayAny] = dfType.getRefs ++ meta.getRefs
    def updateDFType(dfType: DFType): this.type = copy(dfType = dfType).asInstanceOf[this.type]
    def copyWithNewRefs(using RefGen): this.type = copy(
      meta = meta.copyWithNewRefs,
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
    lazy val getRefs: List[DFRef.TwoWayAny] = guardRef :: prevBlockOrHeaderRef :: meta.getRefs
    def copyWithNewRefs(using RefGen): this.type = copy(
      meta = meta.copyWithNewRefs,
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
    def isFallThrough(using MemberGetSet): Boolean = this.hasTagOf[FallThroughTag]
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
    lazy val getRefs: List[DFRef.TwoWayAny] = iteratorRef :: rangeRef :: meta.getRefs
    def copyWithNewRefs(using RefGen): this.type = copy(
      meta = meta.copyWithNewRefs,
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
    lazy val getRefs: List[DFRef.TwoWayAny] = guardRef :: meta.getRefs
    def copyWithNewRefs(using RefGen): this.type = copy(
      meta = meta.copyWithNewRefs,
      guardRef = guardRef.copyAsNewRef,
      ownerRef = ownerRef.copyAsNewRef
    ).asInstanceOf[this.type]
  end DFWhileBlock
  object DFWhileBlock:
    type GuardRef = DFRef.TwoWay[DFVal, DFWhileBlock]
end DFLoop

final case class DFDesignBlock(
    domainType: DomainType,
    instMode: DFDesignBlock.InstMode,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFBlock,
      DFDomainOwner derives ReadWriter:
  final val dclMeta: Meta = meta
  val dclName: String = dclMeta.name
  private var designInstCache: Option[DFDesignInst] = None
  protected[dfhdl] def setDesignInstCache(designInst: DFDesignInst): Unit =
    designInstCache = Some(designInst)
  protected[dfhdl] def clearDesignInstCache(): Unit = designInstCache = None
  protected[dfhdl] def copyDesignInstCacheFrom(other: DFDesignBlock): Unit =
    designInstCache = other.designInstCache
  protected[dfhdl] def getCachedDesignInst(using MemberGetSet): DFDesignInst =
    assert(getSet.isMutable, "Design inst cache should only be used during elaboration")
    designInstCache.get
  // Under the new convention every non-top sub-design has its `ownerRef`
  // remapped to DFMember.Empty in the immutable DB (so it "behaves as Top in
  // its own sub-DB"). The base `DFOwner.isTop` (which checks ownerRef.get) is
  // therefore unreliable for "is this the actual top of the design DB?" once
  // the DB is immutable. During elaboration the ownerRef hasn't been remapped
  // yet and the DB isn't queryable, so we keep the ownerRef-based answer.
  override def isTop(using MemberGetSet): Boolean =
    if (getSet.isMutable) ownerRef.get == DFMember.Empty
    else this eq getSet.designDB.top
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFDesignBlock =>
      this.domainType =~ that.domainType &&
      this.dclMeta =~ that.dclMeta &&
      this.instMode == that.instMode &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] =
    domainType.getRefs ++ dclMeta.getRefs

  /** Whether this design is considered to be a device's top-level design. THIS MAY NOT BE THE TOP
    * DESIGN, for example if the design is in a simulation. A design is considered to be a device
    * top-level design if it has a device ID constraint (usually as a result of a device resource
    * instantiated within).
    */
  lazy val isDeviceTop: Boolean = this.meta.annotations.exists {
    case _: constraints.DeviceID => true
    case _                       => false
  }
  def copyWithNewRefs(using RefGen): this.type = copy(
    meta = meta.copyWithNewRefs,
    domainType = domainType.copyWithNewRefs,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end DFDesignBlock

object DFDesignBlock:
  import InstMode.BlackBox.Source
  enum InstMode derives CanEqual, ReadWriter:
    case Normal, Def, Simulation
    case BlackBox(source: Source)
  object InstMode:
    import constraints.DeviceID.Vendor
    object BlackBox:
      enum Source derives CanEqual, ReadWriter:
        case NA
        case Files(path: List[String])
        case Library(libName: String, nameSpace: String)
        case VendorIP(vendor: Vendor, typeName: String)

  extension (dsn: DFDesignBlock)
    def isDuplicate: Boolean = dsn.hasTagOf[DuplicateTag]
    def isBlackBox: Boolean = dsn.instMode.isInstanceOf[InstMode.BlackBox]
    def isVendorIPBlackbox: Boolean = dsn.instMode match
      case InstMode.BlackBox(_: InstMode.BlackBox.Source.VendorIP) => true
      case _                                                       => false
    def inSimulation: Boolean = dsn.instMode == InstMode.Simulation
    def getCommonDesignWith(dsn2: DFDesignBlock)(using MemberGetSet): DFDesignBlock =
      def getOwnerDesignChain(dsn: DFDesignBlock): List[Set[DFDesignBlock]] =
        var chain = List(Set(dsn))
        while (chain.head.nonEmpty)
          chain = chain.head.flatMap(getSet.designDB.designBlockOwnershipMap(_)) :: chain
        chain.drop(1) // drop the empty set at the end
      var chain1 = getOwnerDesignChain(dsn)
      var chain2 = getOwnerDesignChain(dsn2)
      while (
        chain1.length > 1 &&
        chain1.drop(1).head.intersect(chain2.drop(1).headOption.getOrElse(Set.empty)).nonEmpty
      )
        chain1 = chain1.drop(1)
        chain2 = chain2.drop(1)
      val finalIntersect = chain1.head.intersect(chain2.head)
      assert(finalIntersect.size == 1)
      finalIntersect.head
    end getCommonDesignWith
  end extension

  object Top:
    def unapply(block: DFDesignBlock)(using MemberGetSet): Boolean = block.isTop
  object Internal:
    def unapply(block: DFDesignBlock)(using MemberGetSet): Boolean = !block.isTop
end DFDesignBlock

// Legacy alias retained during the DFDesignInst split refactor for call sites
// that still mean "a DFDesignBlock behaving as an instantiation". Migrate them
// to the new DFDesignInst case class as consumers are updated.
type DFDesignInstOld = DFDesignBlock
val DFDesignInstOld = DFDesignBlock

final case class DFDesignInst(
    designRef: DFDesignInst.DesignRef,
    paramMap: DFDesignInst.ParamMap,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFMember.Named derives ReadWriter:
  def getDesignBlock(using MemberGetSet): DFDesignBlock = designRef.get
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFDesignInst =>
      this.designRef =~ that.designRef &&
      this.paramMap =~ that.paramMap &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] =
    paramMap.values.toList ++ meta.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    meta = meta.copyWithNewRefs,
    designRef = designRef.copyAsNewRef,
    paramMap = paramMap.map((k, v) => k -> v.copyAsNewRef),
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end DFDesignInst

object DFDesignInst:
  type DesignRef = DFRef.OneWay[DFDesignBlock]
  type ParamRef = DFRef.TwoWay[DFVal, DFDesignInst]
  type ParamMap = ListMap[String, ParamRef]

final case class DomainBlock(
    domainType: DomainType,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFBlock,
      DFDomainOwner derives ReadWriter:
  def flattenMode: annotation.FlattenMode = meta.annotations.collectFirst {
    case fm: annotation.FlattenMode => fm
  }.getOrElse(annotation.FlattenMode.defaultPrefixUnderscore)
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DomainBlock =>
      this.domainType =~ that.domainType &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = domainType.getRefs ++ meta.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    meta = meta.copyWithNewRefs,
    domainType = domainType.copyWithNewRefs,
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end DomainBlock

object DomainBlock

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
//     protected def protGetConstData(using MemberGetSet, ConstData.CachePolicy): ConstData[Any] = ConstData.NotConst
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
  lazy val getRefs: List[DFRef.TwoWayAny] = triggerRef :: meta.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    meta = meta.copyWithNewRefs,
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
      this.op =~ that.op && this.msgParts == that.msgParts && this.msgArgs =~ that.msgArgs &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  lazy val getRefs: List[DFRef.TwoWayAny] = op.getRefs ++ msgArgs ++ meta.getRefs
  def copyWithNewRefs(using RefGen): this.type = copy(
    meta = meta.copyWithNewRefs,
    op = op.copyWithNewRefs,
    msgArgs = msgArgs.map(_.copyAsNewRef),
    ownerRef = ownerRef.copyAsNewRef
  ).asInstanceOf[this.type]
end TextOut

object TextOut:
  type AssertionRef = DFRef.TwoWay[DFVal, TextOut]
  enum Severity derives CanEqual, ReadWriter:
    case Info, Warning, Error, Fatal
  enum Op extends HasRefCompare[Op] derives CanEqual, ReadWriter:
    case Print, Println, Debug, Finish
    case Report(severity: Severity) extends Op
    case Assert(assertionRef: AssertionRef, severity: Severity) extends Op
    lazy val getRefs: List[DFRef.TwoWayAny] = this match
      case Assert(assertion, _) => List(assertion)
      case _                    => Nil
    protected def `prot_=~`(that: Op)(using MemberGetSet): Boolean = (this, that) match
      case (thisAssert: Assert, thatAssert: Assert) =>
        thisAssert.assertionRef =~ thatAssert.assertionRef &&
        thisAssert.severity == thatAssert.severity
      case _ => this equals that
    def copyWithNewRefs(using RefGen): this.type = this match
      case Assert(assertionRef, severity) =>
        Assert(assertionRef = assertionRef.copyAsNewRef, severity = severity)
          .asInstanceOf[this.type]
      case _ => this
  end Op
end TextOut
