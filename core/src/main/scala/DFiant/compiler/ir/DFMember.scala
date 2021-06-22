package DFiant.compiler
package ir
import DFiant.internals.*
import annotation.tailrec

sealed trait DFMember extends Product, Serializable:
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
    this match {
      case DFDesignBlock.Top()       => false
      case _: DFDesignBlock          => getOwnerDesign isSameOwnerDesignAs that
      case _ if getOwnerDesign.isTop => false
      case _                         => getOwnerDesign isSameOwnerDesignAs that
    }
  //true if and only if the member is outside the design at any level
  final def isOutsideOwner(that: DFOwner)(using MemberGetSet): Boolean =
    !isInsideOwner(that)
  @tailrec private def isInsideOwner(thisMember: DFMember, thatOwner: DFOwner)(
      using MemberGetSet
  ): Boolean = {
    (thisMember.getOwner, thatOwner) match {
      case (a, b) if a == b         => true
      case (DFDesignBlock.Top(), _) => false
      case (od, _)                  => isInsideOwner(od, thatOwner)
    }
  }
  //true if and only if the member is inside the design at any level
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
        //more complex referencing just summons the two owner chains and compares them.
        //it is possible to do this more efficiently but the simple cases cover the most common usage anyway
        val memberChain = this.getOwnerChain
        val ctxChain = designOwner.getOwnerChain
        ??? //TODO
  end Named

  sealed trait NamedOrAnonymous extends Named
end DFMember

sealed trait DFVal extends DFMember.Named:
  val dfType: DFType

object DFVal:
  type Ref = DFRef.TwoWay[DFVal]
  sealed trait Modifier extends Product, Serializable
  object Modifier:
    sealed trait Assignable extends Modifier
    sealed trait Connectable extends Modifier
    sealed trait VAL extends Modifier
    case object VAR extends VAL, Assignable, Connectable
    sealed trait Port extends VAL, Assignable, Connectable
    case object IN extends Port
    case object OUT extends Port
    case object INOUT extends Port

  final case class Const(
      token: DFType.Token,
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFVal,
        DFMember.NamedOrAnonymous:
    val dfType = token.dfType
    def =~(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Const =>
        this.token == that.token &&
          this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type =
      copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type =
      copy(meta = meta).asInstanceOf[this.type]

  final case class Dcl(
      dfType: DFType,
      modifier: Modifier,
      externalInit: Option[Seq[DFType.Token]],
      ownerRef: DFOwner.Ref,
      meta: Meta,
      tags: DFTags
  ) extends DFVal:
    def =~(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Dcl =>
        this.dfType == that.dfType && this.modifier == that.modifier && this.externalInit == that.externalInit &&
          this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type =
      copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type =
      copy(meta = meta).asInstanceOf[this.type]

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
      copy(meta = meta).asInstanceOf[this.type]

  object Func:
    enum Op:
      case +, -, *, /, ==, !=, <, >, <=, >=, &, |, ^, %, ++
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
        copy(meta = meta).asInstanceOf[this.type]

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
        copy(meta = meta).asInstanceOf[this.type]

    object Prev:
      enum Op:
        case State, Pipe

    final case class BitsWL(
        dfType: DFType,
        relValRef: DFVal.Ref,
        relWidth: Int,
        relBitLow: Int,
        ownerRef: DFOwner.Ref,
        meta: Meta,
        tags: DFTags
    ) extends Alias:
      def =~(that: DFMember)(using MemberGetSet): Boolean = that match
        case that: BitsWL =>
          this.dfType == that.dfType && this.relValRef =~ that.relValRef &&
            this.relWidth == that.relWidth && this.relBitLow == that.relBitLow &&
            this.meta =~ that.meta && this.tags =~ that.tags
        case _ => false
      protected def setMeta(meta: Meta): this.type =
        copy(meta = meta).asInstanceOf[this.type]
      protected def setTags(tags: DFTags): this.type =
        copy(meta = meta).asInstanceOf[this.type]

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
    copy(meta = meta).asInstanceOf[this.type]

object DFNet:
  enum Op:
    case Assignment, Connection, LazyConnection

sealed trait DFOwner extends DFMember:
  val meta: Meta
  def isTop: Boolean = ownerRef match
    case DFOwner.EmptyRef => true
    case _                => false

object DFOwner:
  type Named = DFOwner & DFMember.Named
  type Ref = DFRef.OneWay[DFOwner]
  object EmptyRef extends Ref:
    lazy val refType = throw new IllegalArgumentException(
      "Illegal access to a top-level's owner ref"
    )

sealed trait DFBlock extends DFOwner
sealed trait DFConditionalBlock extends DFBlock

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
    copy(meta = meta).asInstanceOf[this.type]

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
      copy(meta = meta).asInstanceOf[this.type]
