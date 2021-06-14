package DFiant.compiler
package ir
import DFiant.internals.*

import scala.reflect.ClassTag

sealed trait DFMember extends Product, Serializable:
  val ownerRef: DFOwner.Ref

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

sealed trait DFVal extends DFMember:
  val dfType: DFType

object DFVal:
  type Ref = DFMember.TwoWayRef[DFVal]
  
  final case class Const(token: DFToken, ownerRef: DFOwner.Ref) extends DFVal:
    val dfType = token.dfType
  
  final case class Dcl(
      dfType: DFType,
      modifier: Dcl.Modifier,
      externalInit: Option[Seq[DFToken]],
      ownerRef: DFOwner.Ref
  ) extends DFVal
  object Dcl:
    enum Modifier:
      case IN, OUT, INOUT, VAR
  
  final case class Func(
      dfType: DFType,
      op: Func.Op,
      args: List[DFVal.Ref],
      ownerRef: DFOwner.Ref
  ) extends DFVal
  object Func:
    enum Op:
      case +, -, *, /, ==, !=, <, >, <=, >=, &, |, ^, %, ++, !

final case class DFNet(
    toRef: DFVal.Ref,
    op: DFNet.Op,
    fromRef: DFVal.Ref,
    ownerRef: DFOwner.Ref
) extends DFMember

object DFNet:
  enum Op:
    case Assignment, Connection, LazyConnection

sealed trait DFOwner extends DFMember
object DFOwner:
  type Ref = DFMember.OneWayRef[DFOwner]

sealed trait DFSimMember extends DFMember
object DFSimMember:
  final case class Assert(ownerRef: DFOwner.Ref) extends DFSimMember
