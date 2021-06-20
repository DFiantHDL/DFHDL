package DFiant.compiler
package ir
import DFiant.internals.*

import scala.reflect.{ClassTag, classTag}

sealed trait DFMember extends Product, Serializable:
  val ownerRef: DFOwner.Ref
  val meta: Meta
  val tags: DFMember.Tags

object DFMember:
  sealed trait Ref:
    type Member <: DFMember
    val refType: ClassTag[Member]
    def get(using MemberGetSet): Member
  trait OneWayRef[M <: DFMember] extends Ref:
    type Member = M
  trait TwoWayRef[M <: DFMember] extends Ref:
    type Member = M
    lazy val originRef: OneWayRef[DFMember]

  sealed trait Tag extends Product with Serializable
  trait TagOf[-T <: DFMember] extends Tag
  type Tags = Map[ClassTag[_], Tag]

sealed trait DFVal extends DFMember:
  val dfType: DFType

object DFVal:
  type Ref = DFMember.TwoWayRef[DFVal]

  final case class Const(
      token: DFType.Token,
      ownerRef: DFOwner.Ref,
      meta: MemberMeta,
      tags: DFMember.Tags
  ) extends DFVal:
    val dfType = token.dfType

  final case class Dcl(
      dfType: DFType,
      modifier: Dcl.Modifier,
      externalInit: Option[Seq[DFType.Token]],
      ownerRef: DFOwner.Ref,
      meta: MemberMeta,
      tags: DFMember.Tags
  ) extends DFVal
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
      tags: DFMember.Tags
  ) extends DFVal
  object Func:
    enum Op:
      case +, -, *, /, ==, !=, <, >, <=, >=, &, |, ^, %, ++, !

final case class DFNet(
    toRef: DFVal.Ref,
    op: DFNet.Op,
    fromRef: DFVal.Ref,
    ownerRef: DFOwner.Ref,
    meta: MemberMeta,
    tags: DFMember.Tags
) extends DFMember

object DFNet:
  enum Op:
    case Assignment, Connection, LazyConnection

sealed trait DFOwner extends DFMember:
  val meta: OwnerMeta

object DFOwner:
  type Ref = DFMember.OneWayRef[DFOwner]

sealed trait DFSimMember extends DFMember
object DFSimMember:
  final case class Assert(
      ownerRef: DFOwner.Ref,
      meta: OwnerMeta,
      tags: DFMember.Tags
  ) extends DFSimMember
