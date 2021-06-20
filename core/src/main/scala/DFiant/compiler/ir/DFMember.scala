package DFiant.compiler
package ir
import DFiant.internals.*

sealed trait DFMember extends Product, Serializable:
  type Meta <: ir.Meta
  val ownerRef: DFOwner.Ref
  val meta: Meta
  val tags: DFTags
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
    protected def setMeta(meta: Meta): this.type =
      copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type =
      copy(meta = meta).asInstanceOf[this.type]

  object Func:
    enum Op:
      case +, -, *, /, ==, !=, <, >, <=, >=, &, |, ^, %, ++, !

final case class DFNet(
    toRef: DFVal.Ref,
    op: DFNet.Op,
    fromRef: DFVal.Ref,
    ownerRef: DFOwner.Ref,
    meta: MemberMeta,
    tags: DFTags
) extends DFMember:
  type Meta = MemberMeta
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
    protected def setMeta(meta: Meta): this.type =
      copy(meta = meta).asInstanceOf[this.type]
    protected def setTags(tags: DFTags): this.type =
      copy(meta = meta).asInstanceOf[this.type]
