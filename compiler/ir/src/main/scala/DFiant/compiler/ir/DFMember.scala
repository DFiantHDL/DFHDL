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
  final def getOwnerNamed(using MemberGetSet): DFOwner.Named = getOwner match
    case b: DFOwner.Named => b
    case o                => o.getOwnerNamed
  final def getOwnerBlock(using MemberGetSet): DFBlock = getOwner match
    case b: DFBlock => b
    case o          => o.getOwnerBlock
  final def getOwnerDesign(using MemberGetSet): DFDesignBlock =
    getOwnerBlock match
      case d: DFDesignBlock => d
      case b: DFBlock       => b.getOwnerDesign
  final def getThisOrOwnerDesign(using MemberGetSet): DFDesignBlock = this match
    case d: DFDesignBlock => d
    case x                => x.getOwnerDesign
  final def hasLateConstruction: Boolean = meta.lateConstruction
  final def isMemberOfDesign(that: DFDesignBlock)(using MemberGetSet): Boolean =
    this match
      case DFDesignBlock.Top() => false
      case _                   => getOwnerDesign == that
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
    val meta: Meta = Meta(Some("Empty"), Position.unknown, false)
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
      val designOwner = callOwner.getThisOrOwnerDesign
      if (this isMemberOfDesign designOwner) name
      else if (getOwnerDesign isOneLevelBelow designOwner) s"${getOwnerDesign.name}.$name"
      else if (callOwner isInsideOwner this.getOwnerDesign) name
      else
        // more complex referencing just summons the two owner chains and compares them.
        // it is possible to do this more efficiently but the simple cases cover the most common usage anyway
        val memberChain = this.getOwnerChain
        val ctxChain = designOwner.getOwnerChain
        ??? // TODO
  end Named

  sealed trait NamedOrAnonymous extends Named
end DFMember

sealed trait DFVal extends DFMember.Named:
  val dfType: DFType

object DFVal:
  type Ref = DFRef.TwoWay[DFVal]
  sealed trait Modifier extends Product, Serializable derives CanEqual
  object Modifier:
    sealed trait Assignable extends Modifier
    sealed trait Connectable extends Modifier
    sealed trait VAL extends Modifier
    sealed trait Extendable extends VAL
    case object VAR extends VAL, Assignable, Connectable
    sealed trait Port extends VAL, Assignable, Connectable
    case object IN extends Port
    case object OUT extends Port
    case object INOUT extends Port

  extension (dfVal: DFVal)
    def isPort: Boolean = dfVal match
      case dcl: DFVal.Dcl =>
        dcl.modifier match
          case _: Modifier.Port => true
          case _                => false
      case _ => false
    @tailrec def dealias(using MemberGetSet): Option[DFVal.Dcl] = dfVal match
      case dcl: DFVal.Dcl     => Some(dcl)
      case alias: DFVal.Alias => alias.relValRef.get.dealias
      case _                  => None

  final case class Const(
      token: DFTokenAny,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFVal,
        DFMember.NamedOrAnonymous:
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
  ) extends DFVal,
        DFMember.NamedOrAnonymous:
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
      case >>, <<
      case unary_-, unary_~, unary_!

  sealed trait Alias extends DFVal, DFMember.NamedOrAnonymous:
    val relValRef: DFVal.Ref

  object Alias:
    final case class AsIs(
        dfType: DFType,
        relValRef: DFVal.Ref,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Alias:
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
        relValRef: DFVal.Ref,
        step: Int,
        op: History.Op,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Alias:
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: History =>
          this.dfType == that.dfType && this.relValRef =~ that.relValRef &&
            this.step == that.step && this.op == that.op &&
            this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
    end History

    object History:
      enum Op derives CanEqual:
        case Prev, Pipe

    final case class ApplyRange(
        relValRef: DFVal.Ref,
        relBitHigh: Int,
        relBitLow: Int,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Alias:
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
        relValRef: DFVal.Ref,
        relIdx: DFVal.Ref,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Alias:
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
        relValRef: DFVal.Ref,
        fieldName: String,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Alias:
      protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: SelectField =>
          this.dfType == that.dfType && this.relValRef =~ that.relValRef &&
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
end DFNet

object DFNet:
  type Ref = DFRef.TwoWay[DFVal | DFInterfaceOwner]
  enum Op derives CanEqual:
    case Assignment, Connection, LazyConnection

sealed trait DFOwner extends DFMember:
  val meta: Meta
  def isTop(using MemberGetSet): Boolean = ownerRef.get match
    case DFMember.Empty => true
    case _              => false

object DFOwner:
  type Named = DFOwner & DFMember.Named
  type Ref = DFRef.OneWay[DFOwner | DFMember.Empty]

final case class DFInterfaceOwner(
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFOwner,
      DFMember.Named:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFInterfaceOwner =>
      this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]

sealed trait DFBlock extends DFOwner
object DFConditional:
  sealed trait Block extends DFBlock:
    val prevBlockOrHeaderRef: Block.Ref
  object Block:
    type Ref = DFRef.OneWay[Block | Header]

  sealed trait Header extends DFVal

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
      guardRef: DFCaseBlock.GuardRef,
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
    type GuardRef = DFRef.TwoWay[DFVal | DFMember.Empty]
    type Ref = DFRef.OneWay[DFCaseBlock | DFMatchHeader]
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
      condRef: DFIfElseBlock.CondRef,
      prevBlockOrHeaderRef: DFIfElseBlock.Ref,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Block:
    protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFIfElseBlock =>
        this.condRef =~ that.condRef && this.prevBlockOrHeaderRef =~ that.prevBlockOrHeaderRef &&
          this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type = copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type = copy(tags = tags).asInstanceOf[this.type]
  end DFIfElseBlock
  object DFIfElseBlock:
    type CondRef = DFRef.TwoWay[DFVal | DFMember.Empty]
    type Ref = DFRef.OneWay[DFIfElseBlock | DFIfHeader]
end DFConditional

final case class DFDesignBlock(
    dclName: String,
    dclPosition: Position,
    inSimulation: Boolean,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFBlock,
      DFMember.Named:
  protected def `prot_=~`(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFDesignBlock =>
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
