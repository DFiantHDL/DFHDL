package DFiant.compiler
package ir
import DFiant.internals.*

sealed trait DFMember extends Product, Serializable:
  type Meta <: ir.Meta
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

sealed trait DFVal extends DFMember:
  type Meta = MemberMeta
  val dfType: DFType

object DFVal:
  type Ref = DFRef.TwoWay[DFVal]

  final case class Const(
      token: DFType.Token,
      ownerRef: DFOwner.Ref,
      meta: MemberMeta,
      tags: DFTags
  ) extends DFVal:
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
      modifier: Dcl.Modifier,
      externalInit: Option[Seq[DFType.Token]],
      ownerRef: DFOwner.Ref,
      meta: MemberMeta,
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

  object Dcl:
    sealed trait Modifier extends Product, Serializable
    enum Port extends Modifier:
      case IN, OUT, INOUT
    case object VAR extends Modifier

  final case class Func(
      dfType: DFType,
      op: Func.Op,
      args: List[DFVal.Ref],
      ownerRef: DFOwner.Ref,
      meta: MemberMeta,
      tags: DFTags
  ) extends DFVal:
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
      case +, -, *, /, ==, !=, <, >, <=, >=, &, |, ^, %, ++, !

  sealed trait Alias extends DFVal:
    val relValRef: DFVal.Ref

  object Alias:
    final case class AsIs(
        dfType: DFType,
        relValRef: DFVal.Ref,
        ownerRef: DFOwner.Ref,
        meta: MemberMeta,
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
        meta: MemberMeta,
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
        meta: MemberMeta,
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
    meta: MemberMeta,
    tags: DFTags
) extends DFMember:
  type Meta = MemberMeta
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
  type Meta = OwnerMeta
  val meta: OwnerMeta

object DFOwner:
  type Ref = DFRef.OneWay[DFOwner]

sealed trait DFSimMember extends DFMember
object DFSimMember:
  final case class Assert(
      ownerRef: DFOwner.Ref,
      meta: MemberMeta,
      tags: DFTags
  ) extends DFSimMember:
    type Meta = MemberMeta
    def =~(that: DFMember)(using MemberGetSet): Boolean = that match
      case that: Assert =>
        this.meta =~ that.meta && this.tags =~ that.tags
      case _ => false
    protected def setMeta(meta: Meta): this.type =
      copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type =
      copy(meta = meta).asInstanceOf[this.type]
