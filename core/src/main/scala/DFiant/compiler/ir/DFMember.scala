package DFiant.compiler
package ir
import DFiant.internals.*
import annotation.tailrec

sealed trait DFMember extends Product, Serializable derives CanEqual:
  val ownerRef: DFOwner.Ref
  val meta: Meta
  val tags: DFTags
  def =~(that: DFMember)(using MemberGetSet): Boolean
  final def setMeta(metaFunc: Meta => Meta)(using
      getSet: MemberGetSet
  ): this.type =
    getSet.set(this)(m => setMeta(metaFunc(m.meta)))
  final def setTags(tagsFunc: DFTags => DFTags)(using
      getSet: MemberGetSet
  ): this.type =
    getSet.set(this)(m => setTags(tagsFunc(m.tags)))
  protected def setMeta(meta: Meta): this.type
  protected def setTags(tags: DFTags): this.type
  final def getOwner(using MemberGetSet): DFOwner = this match
    case o: DFOwner if o.isTop => o
    case _                     => ownerRef.get
  final def getOwnerNamed(using MemberGetSet): DFOwner.Named =
    ownerRef.get match
      case b: DFOwner.Named => b
      case o                => o.getOwnerNamed
  final def getOwnerBlock(using MemberGetSet): DFBlock = ownerRef.get match
    case b: DFBlock => b
    case o          => o.getOwnerBlock
  final def getOwnerDesign(using MemberGetSet): DFDesignBlock =
    getOwnerBlock match
      case d: DFDesignBlock => d
      case b: DFBlock       => b.getOwnerDesign
  final def getThisOrOwnerDesign(using MemberGetSet): DFDesignBlock = this match
    case d: DFDesignBlock => d
    case x                => x.getOwnerDesign
  final val hasLateConstruction: Boolean = meta.lateConstruction
  final def isMemberOfDesign(that: DFDesignBlock)(using MemberGetSet): Boolean =
    getOwnerDesign == that
  final def isSameOwnerDesignAs(that: DFMember)(using MemberGetSet): Boolean =
    getOwnerDesign == that.getOwnerDesign
  final def isOneLevelBelow(that: DFMember)(using MemberGetSet): Boolean =
    this match
      case DFDesignBlock.Top()       => false
      case _: DFDesignBlock          => getOwnerDesign isSameOwnerDesignAs that
      case _ if getOwnerDesign.isTop => false
      case _                         => getOwnerDesign isSameOwnerDesignAs that
  // true if and only if the member is outside the design at any level
  final def isOutsideOwner(that: DFOwner)(using MemberGetSet): Boolean =
    !isInsideOwner(that)
  @tailrec private def isInsideOwner(thisMember: DFMember, thatOwner: DFOwner)(
      using MemberGetSet
  ): Boolean =
    (thisMember.getOwner, thatOwner) match
      case (a, b) if a == b         => true
      case (DFDesignBlock.Top(), _) => false
      case (od, _)                  => isInsideOwner(od, thatOwner)
  // true if and only if the member is inside the design at any level
  final def isInsideOwner(that: DFOwner)(using MemberGetSet): Boolean =
    isInsideOwner(this, that)
  final def getOwnerChain(using MemberGetSet): List[DFBlock] =
    if (getOwnerBlock.isTop) List(getOwnerBlock)
    else getOwnerBlock.getOwnerChain :+ getOwnerBlock
end DFMember

object DFMember:
  sealed trait Named extends DFMember:
    final val name: String = meta.name
    final val isAnonymous: Boolean = meta.isAnonymous
    final def getFullName(using MemberGetSet): String = this match
      case o @ DFDesignBlock.Top() => o.name
      case _                       => s"${getOwnerNamed.getFullName}.${name}"
    def getRelativeName(callOwner: DFOwner)(using MemberGetSet): String =
      val designOwner = callOwner.getThisOrOwnerDesign
      if (this isMemberOfDesign designOwner) name
      else if (getOwnerDesign isOneLevelBelow designOwner)
        s"${getOwnerDesign.name}.$name"
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

  final case class Const(
      token: DFTokenAny,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFVal,
        DFMember.NamedOrAnonymous:
    val dfType = token.dfType
    def =~(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Const =>
        given CanEqual[Any, Any] = CanEqual.derived
        this.token == that.token &&
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type =
      copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type =
      copy(tags = tags).asInstanceOf[this.type]
  end Const

  final case class Dcl(
      dfType: DFType,
      modifier: Modifier,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFVal:
    def =~(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Dcl =>
        this.dfType == that.dfType && this.modifier == that.modifier &&
          this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type =
      copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type =
      copy(tags = tags).asInstanceOf[this.type]
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
    def =~(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Func =>
        this.dfType == that.dfType && this.op == that.op && (this.args
          .lazyZip(that.args)
          .forall((l, r) => l =~ r)) &&
          this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type =
      copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type =
      copy(tags = tags).asInstanceOf[this.type]
  end Func

  object Func:
    enum Op derives CanEqual:
      case +, -, *, /, ===, =!=, <, >, <=, >=, &, |, ^, %, ++
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
      def =~(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: AsIs =>
          this.dfType == that.dfType && this.relValRef =~ that.relValRef &&
            this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type =
        copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type =
        copy(tags = tags).asInstanceOf[this.type]
    end AsIs

    final case class Prev(
        dfType: DFType,
        relValRef: DFVal.Ref,
        step: Int,
        op: Prev.Op,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Alias:
      def =~(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: Prev =>
          this.dfType == that.dfType && this.relValRef =~ that.relValRef &&
            this.step == that.step && this.op == that.op &&
            this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type =
        copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type =
        copy(tags = tags).asInstanceOf[this.type]
    end Prev

    object Prev:
      enum Op derives CanEqual:
        case State, Pipe

    final case class ApplyRange(
        relValRef: DFVal.Ref,
        relBitHigh: Int,
        relBitLow: Int,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Alias:
      val dfType: DFType = DFBits(relBitHigh - relBitLow + 1)
      def =~(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: ApplyRange =>
          this.relValRef =~ that.relValRef &&
            this.relBitHigh == that.relBitHigh && this.relBitLow == that.relBitLow &&
            this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type =
        copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type =
        copy(tags = tags).asInstanceOf[this.type]
    end ApplyRange
    final case class ApplyIdx(
        dfType: DFType,
        relValRef: DFVal.Ref,
        relIdx: DFVal.Ref,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Alias:
      def =~(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: ApplyIdx =>
          this.dfType == that.dfType && this.relValRef =~ that.relValRef &&
            this.relIdx =~ that.relIdx &&
            this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type =
        copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type =
        copy(tags = tags).asInstanceOf[this.type]
    end ApplyIdx

    final case class SelectField(
        dfType: DFType,
        relValRef: DFVal.Ref,
        fieldName: String,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Alias:
      def =~(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: SelectField =>
          this.dfType == that.dfType && this.relValRef =~ that.relValRef &&
            this.fieldName == that.fieldName &&
            this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type =
        copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type =
        copy(tags = tags).asInstanceOf[this.type]
    end SelectField

  end Alias
end DFVal

final case class DFNet(
    toRef: DFVal.Ref,
    op: DFNet.Op,
    fromRef: DFVal.Ref,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFMember:
  def =~(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFNet =>
      this.toRef =~ that.toRef && this.op == that.op && this.fromRef =~ that.fromRef &&
        this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type =
    copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type =
    copy(tags = tags).asInstanceOf[this.type]
end DFNet

object DFNet:
  enum Op derives CanEqual:
    case Assignment, Connection, LazyConnection

sealed trait DFOwner extends DFMember:
  val meta: Meta
  def isTop: Boolean = ownerRef match
    case _: DFRef.Empty => true
    case _              => false

object DFOwner:
  type Named = DFOwner & DFMember.Named
  type Ref = DFRef.OneWay[DFOwner]

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
    def =~(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFMatchHeader =>
        this.dfType == that.dfType && this.selectorRef =~ that.selectorRef &&
          this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type =
      copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type =
      copy(tags = tags).asInstanceOf[this.type]
  end DFMatchHeader

  final case class DFCaseBlock(
      pattern: DFCaseBlock.Pattern,
      guard: DFVal.Ref,
      prevBlockOrHeaderRef: DFCaseBlock.Ref,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Block:
    def =~(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFCaseBlock =>
        this.pattern =~ that.pattern && this.prevBlockOrHeaderRef =~ that.prevBlockOrHeaderRef &&
          this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type =
      copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type =
      copy(tags = tags).asInstanceOf[this.type]
  end DFCaseBlock
  object DFCaseBlock:
    type Ref = DFRef.OneWay[DFCaseBlock | DFMatchHeader]
    sealed trait Pattern derives CanEqual:
      def =~(that: Pattern)(using MemberGetSet): Boolean
    object Pattern:
      case object NoPattern extends Pattern:
        def =~(that: Pattern)(using MemberGetSet): Boolean = this == that
      final case class Singleton(token: DFTokenAny) extends Pattern:
        def =~(that: Pattern)(using MemberGetSet): Boolean = this == that

  final case class DFIfHeader(
      dfType: DFType,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Header:
    def =~(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFMatchHeader =>
        this.dfType == that.dfType &&
          this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type =
      copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type =
      copy(tags = tags).asInstanceOf[this.type]
  end DFIfHeader

  final case class DFIfElseBlock(
      condRef: DFVal.Ref,
      prevBlockOrHeaderRef: DFIfElseBlock.Ref,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends Block:
    def =~(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: DFIfElseBlock =>
        this.condRef =~ that.condRef && this.prevBlockOrHeaderRef =~ that.prevBlockOrHeaderRef &&
          this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type =
      copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type =
      copy(tags = tags).asInstanceOf[this.type]
  end DFIfElseBlock
  object DFIfElseBlock:
    type Ref = DFRef.OneWay[DFIfElseBlock | DFIfHeader]
end DFConditional

final case class DFDesignBlock(
    designType: String,
    inSimulation: Boolean,
    ownerRef: DFOwner.Ref,
    meta: Meta,
    tags: DFTags
) extends DFBlock,
      DFMember.Named:
  def =~(that: DFMember)(using MemberGetSet): Boolean = that match
    case that: DFDesignBlock =>
      this.designType == that.designType && this.inSimulation == that.inSimulation &&
        this.meta =~ that.meta && this.tags =~ that.tags
    case _ => false
  protected def setMeta(meta: Meta): this.type =
    copy(meta = meta).asInstanceOf[this.type]
  protected def setTags(tags: DFTags): this.type =
    copy(tags = tags).asInstanceOf[this.type]
end DFDesignBlock

object DFDesignBlock:
  object Top:
    def unapply(block: DFDesignBlock): Boolean = block.isTop
  object Internal:
    def unapply(block: DFDesignBlock): Boolean = !block.isTop

sealed trait DFSimMember extends DFMember
object DFSimMember:
  final case class Assert(
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFSimMember:
    def =~(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Assert =>
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type =
      copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type =
      copy(tags = tags).asInstanceOf[this.type]
