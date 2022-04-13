package DFiant.compiler
package ir
import DFiant.internals.*

import annotation.tailrec

trait HasRefCompare[T <: HasRefCompare[T]]:
  private var cachedCompare: Option[(T, Boolean)] = None
  final def =~(that: T)(using MemberGetSet): Boolean =
    cachedCompare match
      case Some(prevCompare, result) if prevCompare eq that => result
      case _ =>
        val res = this `prot_=~` that
        cachedCompare = Some(that, res)
        res
  protected def `prot_=~`(that: T)(using MemberGetSet): Boolean

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
    case DFMember.Empty => throw new IllegalArgumentException("No owner found.")
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
  final def isMemberOf(that: DFOwnerNamed)(using MemberGetSet): Boolean =
    this match
      case DFDesignBlock.Top() => false
      case _                   => getOwnerNamed == that
  final def isSameOwnerDesignAs(that: DFMember)(using MemberGetSet): Boolean =
    (this, that) match
      case (DFDesignBlock.Top(), DFDesignBlock.Top()) => this == that
      case (DFDesignBlock.Top(), _)                   => false
      case (_, DFDesignBlock.Top())                   => false
      case _                                          => getOwnerDesign == that.getOwnerDesign
  final def isOneLevelBelow(that: DFMember)(using MemberGetSet): Boolean =
    this match
      case DFDesignBlock.Top() => false
      case _                   => getOwnerDesign isSameOwnerDesignAs that
  // true if and only if the member is outside the design at any level
  final def isOutsideOwner(that: DFOwner)(using MemberGetSet): Boolean =
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
  final def isInsideOwner(that: DFOwner)(using MemberGetSet): Boolean =
    isInsideOwner(this, that)
  final def getOwnerChain(using MemberGetSet): List[DFBlock] =
    this match
      case d @ DFDesignBlock.Top() => Nil
      case _ =>
        if (getOwnerBlock.isTop) List(getOwnerBlock)
        else getOwnerBlock.getOwnerChain :+ getOwnerBlock
end DFMember

object DFMember:
  type Empty = Empty.type
  case object Empty extends DFMember:
    val ownerRef: DFOwner.Ref = DFRef.OneWay.Empty
    val meta: Meta = Meta(Some("Empty"), Position.unknown)
    val tags: DFTags = DFTags.empty
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case Empty => true
      case _     => false
    protected def setMeta(meta: Meta): this.type = this
    protected def setTags(tags: DFTags): this.type = this

  sealed trait Named extends DFMember:
    final val name: String = meta.name
    final val isAnonymous: Boolean = meta.isAnonymous
    final def getFullName(using MemberGetSet): String = this match
      case o: DFOwner if o.isTop => o.name
      case _                     => s"${getOwnerNamed.getFullName}.${name}"
    def getRelativeName(callOwner: DFOwner)(using MemberGetSet): String =
      val namedOwner = callOwner.getThisOrOwnerNamed
      if (this isMemberOf namedOwner) name
      else if (getOwnerNamed isOneLevelBelow namedOwner) s"${getOwnerNamed.name}.$name"
      else if (callOwner isInsideOwner this.getOwnerNamed) name
      else
        // more complex referencing just summons the two owner chains and compares them.
        // it is possible to do this more efficiently but the simple cases cover the most common usage anyway
        val memberChain = this.getOwnerChain
        val ctxChain = namedOwner.getOwnerChain
        "???" // TODO
  end Named
end DFMember

sealed trait DFVal extends DFMember.Named:
  val dfType: DFType

object DFVal:
  type Ref = DFRef.TwoWay[DFVal, DFMember]
  enum Modifier derives CanEqual:
    case VAR, IN, OUT, INOUT, REG, WIRE

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
    @tailrec def dealias(using MemberGetSet): Option[DFVal.Dcl] = dfVal match
      case dcl: DFVal.Dcl     => Some(dcl)
      case alias: DFVal.Alias => alias.relValRef.get.dealias
      case _                  => None
  end extension

  // can be an expression
  sealed trait CanBeExpr extends DFVal

  final case class Const(
      token: DFTokenAny,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends CanBeExpr:
    val dfType = token.dfType
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Const =>
        given CanEqual[Any, Any] = CanEqual.derived
        this.token == that.token &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  end Const

  final case class Dcl(
      dfType: DFType,
      modifier: Modifier,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFVal:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Dcl =>
        this.dfType == that.dfType && this.modifier == that.modifier &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  end Dcl

  final case class Func(
      dfType: DFType,
      op: Func.Op,
      args: List[DFVal.Ref],
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends CanBeExpr:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Func =>
        this.dfType == that.dfType && this.op == that.op && (this.args
          .lazyZip(that.args)
          .forall((l, r) => l =~ r)) &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  end Func

  object Func:
    enum Op derives CanEqual:
      case +, -, *, /, ===, =!=, <, >, <=, >=, &, |, ^, %, ++
      case >>, <<, ror, rol, reverse
      case unary_-, unary_~, unary_!
      case rising, falling

  sealed trait Alias extends CanBeExpr:
    val relValRef: Alias.Ref

  object Alias:
    type Ref = DFRef.TwoWay[DFVal, Alias]
    // This is complete alias that consumes its relative val
    sealed trait Consumer extends Alias:
      val relValRef: ConsumerRef
    type ConsumerRef = DFRef.TwoWay[DFVal, Consumer]

    // This is a partial alias that can propagate its modifier.
    // E.g., a mutable variable `x` that we select its bit `x(1)` is also mutable.
    sealed trait Partial extends Alias:
      val relValRef: PartialRef
    type PartialRef = DFRef.TwoWay[DFVal, Partial]

    final case class AsIs(
        dfType: DFType,
        relValRef: ConsumerRef,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Consumer:
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: AsIs =>
          this.dfType == that.dfType && this.relValRef =~ that.relValRef &&
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
        initOption: Option[DFTokenAny],
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Consumer:
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: History =>
          this.dfType == that.dfType && this.relValRef =~ that.relValRef &&
          this.step == that.step && this.op == that.op && this.initOption == that.initOption
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    end History

    object History:
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
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: ApplyIdx =>
          this.dfType == that.dfType && this.relValRef =~ that.relValRef &&
          this.relIdx =~ that.relIdx &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    end ApplyIdx

    final case class SelectField(
        dfType: DFType,
        relValRef: PartialRef,
        fieldName: String,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Partial:
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: SelectField =>
          this.dfType == that.dfType && this.relValRef =~ that.relValRef &&
          this.fieldName == that.fieldName &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    end SelectField

    final case class RegDIN(
        dfType: DFType,
        relValRef: ConsumerRef,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Consumer:
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: RegDIN =>
          this.dfType == that.dfType && this.relValRef =~ that.relValRef &&
          this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    end RegDIN
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
end DFNet

object DFNet:
  type Ref = DFRef.TwoWay[DFVal | DFInterfaceOwner, DFNet]
  enum Op derives CanEqual:
    case Assignment, Connection, LateConnection, LazyConnection
  extension (net: DFNet)
    def isAssignment = net.op match
      case Op.Assignment => true
      case _             => false
    def isConnection = net.op match
      case Op.Connection | Op.LateConnection | Op.LazyConnection => true
      case _                                                     => false
    def isLateConnection = net.op match
      case Op.LateConnection => true
      case _                 => false
    def isLazyConnection = net.op match
      case Op.LazyConnection => true
      case _                 => false

  object Assignment:
    def unapply(arg: DFNet)(using MemberGetSet): Option[(DFVal, DFVal)] = arg match
      case DFNet(DFRef(toVal: DFVal), Op.Assignment, DFRef(fromVal: DFVal), _, _, _) =>
        Some(toVal, fromVal)
      case _ => None
  object Connection:
    def unapply(net: DFNet)(using
        MemberGetSet
        //             toVal                      fromVal              Swapped
    ): Option[(DFVal.Dcl | DFInterfaceOwner, DFVal | DFInterfaceOwner, Boolean)] =
      if (net.isConnection) (net.lhsRef.get, net.rhsRef.get) match
        case (lhsVal: DFVal, rhsVal: DFVal) =>
          val toLeft = lhsVal.dealias.flatMap(getSet.designDB.connectionTable.get).contains(net)
          if (toLeft) Some(lhsVal.dealias.get, rhsVal, false)
          else Some(rhsVal.dealias.get, lhsVal, true)
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
end DFInterfaceOwner

sealed trait DFBlock extends DFOwner

final case class AlwaysBlock(
    sensitivity: AlwaysBlock.Sensitivity,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFBlock,
      DFOwnerNamed:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: AlwaysBlock =>
      this.sensitivity =~ that.sensitivity &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
end AlwaysBlock
object AlwaysBlock:
  sealed trait Sensitivity extends HasRefCompare[Sensitivity], Product, Serializable
      derives CanEqual
  object Sensitivity:
    case object All extends Sensitivity:
      protected def `prot_=~`(that: Sensitivity)(using MemberGetSet): Boolean = that match
        case All => true
        case _   => false
    final case class List(refs: scala.List[DFVal.Ref]) extends Sensitivity:
      protected def `prot_=~`(that: Sensitivity)(using MemberGetSet): Boolean = that match
        case that: List => this.refs.lazyZip(that.refs).forall(_ =~ _)
        case _          => false

object DFConditional:
  sealed trait Block extends DFBlock:
    val guardRef: Block.GuardRef
    val prevBlockOrHeaderRef: Block.Ref
  object Block:
    type Ref = DFRef.TwoWay[Block | Header, DFMember]
    type GuardRef = DFRef.TwoWay[DFVal | DFMember.Empty, DFMember]

  sealed trait Header extends DFVal.CanBeExpr

  final case class DFMatchHeader(
      dfType: DFType,
      selectorRef: DFVal.Ref,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Header:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFMatchHeader =>
        this.dfType == that.dfType && this.selectorRef =~ that.selectorRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  end DFMatchHeader

  final case class DFCaseBlock(
      pattern: DFCaseBlock.Pattern,
      guardRef: Block.GuardRef,
      prevBlockOrHeaderRef: DFCaseBlock.Ref,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Block:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFCaseBlock =>
        this.pattern =~ that.pattern && this.guardRef =~ that.guardRef &&
        this.prevBlockOrHeaderRef =~ that.prevBlockOrHeaderRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  end DFCaseBlock
  object DFCaseBlock:
    type Ref = DFRef.TwoWay[DFCaseBlock | DFMatchHeader, Block]
    sealed trait Pattern extends HasRefCompare[Pattern] derives CanEqual
    object Pattern:
      case object CatchAll extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean = this == that
      final case class Singleton(token: DFTokenAny) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean = this == that
      final case class Alternative(list: List[Pattern]) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: Alternative =>
              this.list.lazyZip(that.list).forall(_ =~ _)
            case _ => false
      final case class Struct(name: String, fieldPatterns: List[Pattern]) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: Struct =>
              this.name == that.name && this.fieldPatterns
                .lazyZip(that.fieldPatterns)
                .forall(_ =~ _)
            case _ => false
      final case class Bind(ref: Bind.Ref, pattern: Pattern) extends Pattern:
        protected def `prot_=~`(that: Pattern)(using MemberGetSet): Boolean =
          that match
            case that: Bind =>
              this.ref =~ that.ref && this.pattern =~ that.pattern
            case _ => false
      object Bind:
        type Ref = DFRef.OneWay[DFVal]
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
    end Pattern
  end DFCaseBlock

  final case class DFIfHeader(
      dfType: DFType,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Header:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFMatchHeader =>
        this.dfType == that.dfType &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  end DFIfHeader

  final case class DFIfElseBlock(
      guardRef: Block.GuardRef,
      prevBlockOrHeaderRef: DFIfElseBlock.Ref,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Block:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFIfElseBlock =>
        this.guardRef =~ that.guardRef && this.prevBlockOrHeaderRef =~ that.prevBlockOrHeaderRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  end DFIfElseBlock
  object DFIfElseBlock:
    type Ref = DFRef.TwoWay[DFIfElseBlock | DFIfHeader, Block]
end DFConditional

final case class DFDesignBlock(
    domainType: DomainType,
    dclName: String,
    dclPosition: Position,
    inSimulation: Boolean,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFBlock,
      DFDomainOwner:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFDesignBlock =>
      this.domainType == that.domainType &&
      this.dclName == that.dclName && this.dclPosition == that.dclPosition &&
      this.inSimulation == that.inSimulation &&
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
end DFDesignBlock

object DFDesignBlock:
  object Top:
    def unapply(block: DFDesignBlock)(using MemberGetSet): Boolean = block.isTop
  object Internal:
    def unapply(block: DFDesignBlock)(using MemberGetSet): Boolean = !block.isTop

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
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: IsActive =>
        this.timerRef =~ that.timerRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  end IsActive
end Timer
